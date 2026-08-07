-- | Wayland connection management: the socket, object id allocation, request
-- batching, and the event dispatch loop.
--
-- The model mirrors @libwayland@ closely enough to be unsurprising: requests
-- are buffered and flushed together, events are read in batches and
-- dispatched to per-object listeners. It differs in that listeners are plain
-- Haskell closures stored in an 'IntMap' rather than vtables, and that a
-- decode failure is an exception rather than an abort.
module XMonad.River.Connection
  ( -- * Connection
    Connection
  , connect
  , connectTo
  , disconnect
    -- * Requests
  , request
  , newObject
  , freeObject
    -- * Listeners
  , Listener
  , setListener
  , clearListener
  , decode
    -- * Event loop
  , flush
  , dispatch
  , dispatchPending
  , roundtrip
    -- * Registry
  , Global(..)
  , getRegistry
  , bindGlobal
    -- * Errors
  , WaylandError(..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Word (Word16, Word32)
import System.Environment (lookupEnv)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NBS

import Data.Store.Core (Peek)

import XMonad.River.Wire

--------------------------------------------------------------------------------
-- Errors

data WaylandError
  = ConnectionFailed String
  | ProtocolError ObjectId Word32 ByteString
    -- ^ A @wl_display.error@ event: the offending object, an interface
    -- specific code, and a human readable message.
  | DecodeError String
  | Disconnected
  deriving (Show)

instance Exception WaylandError

--------------------------------------------------------------------------------
-- Connection

-- | A handler for events delivered to a single object. Receives the opcode and
-- the raw message body; generated code supplies the decoder.
type Listener = Word16 -> ByteString -> IO ()

-- | Decode an event body, turning a malformed message into an exception. A
-- decode failure means the server sent something this client's generated
-- bindings do not understand, which is not recoverable.
decode :: Peek a -> ByteString -> IO a
decode p body = either (throwIO . DecodeError . show) pure (decodeBody p body)

data Connection = Connection
  { connSocket    :: !N.Socket
  , connNextId    :: !(IORef Word32)
  , connFreeIds   :: !(IORef [Word32])
    -- ^ Ids reclaimed via @wl_display.delete_id@, reused before allocating new
    -- ones as the protocol requires.
  , connOut       :: !(IORef Encoded)
    -- ^ Pending requests, accumulated as a single writer rather than a list of
    -- buffers so that 'flush' neither allocates per message nor copies any
    -- byte twice.
  , connIn        :: !(IORef ByteString)
    -- ^ Bytes read but not yet forming a complete message.
  , connListeners :: !(IORef (IM.IntMap Listener))
  }

-- | The @wl_display@ object always has id 1.
displayId :: ObjectId
displayId = ObjectId 1

-- | Connect using the environment, following the usual Wayland client rules:
-- @WAYLAND_SOCKET@ (an inherited, already connected fd) takes precedence, then
-- @WAYLAND_DISPLAY@ resolved against @XDG_RUNTIME_DIR@, defaulting to
-- @wayland-0@.
connect :: IO Connection
connect = do
  mSock <- lookupEnv "WAYLAND_SOCKET"
  case mSock >>= readMaybeInt of
    Just fd -> do
      sock <- N.mkSocket (fromIntegral (fd :: Int))
      newConnection sock
    Nothing -> do
      disp <- maybe "wayland-0" id <$> lookupEnv "WAYLAND_DISPLAY"
      if take 1 disp == "/"
        then connectTo disp
        else do
          mDir <- lookupEnv "XDG_RUNTIME_DIR"
          case mDir of
            Nothing -> throwIO (ConnectionFailed "XDG_RUNTIME_DIR is not set")
            Just dir -> connectTo (dir ++ "/" ++ disp)
  where
    readMaybeInt s = case reads s of
      [(n, "")] -> Just n
      _         -> Nothing

-- | Connect to an explicit socket path.
connectTo :: FilePath -> IO Connection
connectTo path = do
  sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  N.connect sock (N.SockAddrUnix path)
  newConnection sock

newConnection :: N.Socket -> IO Connection
newConnection sock = do
  -- Client ids start at 2; 1 is wl_display.
  nextId    <- newIORef 2
  freeIds   <- newIORef []
  out       <- newIORef mempty
  inBuf     <- newIORef BS.empty
  listeners <- newIORef IM.empty
  let conn = Connection sock nextId freeIds out inBuf listeners
  setListener conn displayId (displayListener conn)
  pure conn

disconnect :: Connection -> IO ()
disconnect = N.close . connSocket

-- | @wl_display@ has two events we must handle ourselves: protocol errors, and
-- id recycling.
displayListener :: Connection -> Listener
displayListener conn opcode body = case opcode of
  -- error(object_id, code, message)
  0 -> do
    (oid, code, msg) <-
      decode ((,,) <$> getObject <*> getWord32 <*> getString) body
    throwIO (ProtocolError oid code msg)
  -- delete_id(id)
  1 -> do
    i <- decode getWord32 body
    clearListener conn (ObjectId i)
    modifyIORef' (connFreeIds conn) (i :)
  _ -> pure ()

--------------------------------------------------------------------------------
-- Objects and requests

-- | Allocate a client-side object id.
newObject :: Connection -> IO ObjectId
newObject conn = do
  free <- readIORef (connFreeIds conn)
  case free of
    (i:rest) -> do
      writeIORef (connFreeIds conn) rest
      pure (ObjectId i)
    [] -> do
      i <- readIORef (connNextId conn)
      writeIORef (connNextId conn) (i + 1)
      pure (ObjectId i)

-- | Drop a destroyed object's listener. The id itself is only reusable once
-- the server confirms with @delete_id@, so it is not returned to the free list
-- here.
freeObject :: Connection -> ObjectId -> IO ()
freeObject = clearListener

-- | Queue a request. Nothing is written to the socket until 'flush'.
request :: Connection -> ObjectId -> Word16 -> Encoded -> IO ()
request conn oid opcode args =
  modifyIORef' (connOut conn) (<> encodeMessage oid opcode args)

--------------------------------------------------------------------------------
-- Listeners

setListener :: Connection -> ObjectId -> Listener -> IO ()
setListener conn (ObjectId i) l =
  modifyIORef' (connListeners conn) (IM.insert (fromIntegral i) l)

clearListener :: Connection -> ObjectId -> IO ()
clearListener conn (ObjectId i) =
  modifyIORef' (connListeners conn) (IM.delete (fromIntegral i))

--------------------------------------------------------------------------------
-- Event loop

-- | Write all buffered requests to the socket.
--
-- Every pending request is written into one buffer sized exactly to hold them
-- all, so the batch costs a single allocation and a single @write@, and no
-- byte is copied twice on its way out.
flush :: Connection -> IO ()
flush conn = do
  pending <- readIORef (connOut conn)
  writeIORef (connOut conn) mempty
  let bs = runEncoded pending
  unless (BS.null bs) $ NBS.sendAll (connSocket conn) bs

-- | Flush, block for at least one batch of events, and dispatch them all.
-- This is the equivalent of @wl_display_dispatch@.
dispatch :: Connection -> IO ()
dispatch conn = do
  flush conn
  chunk <- NBS.recv (connSocket conn) 4096
  when (BS.null chunk) $ throwIO Disconnected
  -- ByteString's append short-circuits when either side is empty, so this is
  -- free whenever the previous read ended on a message boundary -- which is
  -- the common case. Only a partial trailing message costs a copy, and then
  -- only of that fragment.
  modifyIORef' (connIn conn) (<> chunk)
  dispatchPending conn
  flush conn

-- | Dispatch any complete messages already buffered, without reading from the
-- socket.
dispatchPending :: Connection -> IO ()
dispatchPending conn = do
  buf <- readIORef (connIn conn)
  let (msgs, rest) = splitMessages buf
  writeIORef (connIn conn) rest
  mapM_ deliver msgs
  where
    deliver (h, body) = do
      ls <- readIORef (connListeners conn)
      case IM.lookup (fromIntegral (unObjectId (msgObject h))) ls of
        -- Events for objects we have already destroyed are expected and
        -- ignored: the server may not have processed the destructor yet.
        Nothing -> pure ()
        Just l  -> l (msgOpcode h) body

-- | Send @wl_display.sync@ and dispatch until the server answers, so that all
-- previously queued requests have been processed.
roundtrip :: Connection -> IO ()
roundtrip conn = do
  done <- newIORef False
  cb <- newObject conn
  setListener conn cb $ \_ _ -> writeIORef done True
  -- wl_display.sync(new_id wl_callback)
  request conn displayId 0 (argObject cb)
  let loop = do
        d <- readIORef done
        unless d (dispatch conn >> loop)
  loop
  clearListener conn cb

--------------------------------------------------------------------------------
-- Registry

data Global = Global
  { globalName      :: !Word32
  , globalInterface :: !ByteString
  , globalVersion   :: !Word32
  } deriving (Eq, Show)

-- | Create the registry and collect every global the server advertises. The
-- returned action performs a roundtrip, so the list is complete when it
-- returns.
getRegistry :: Connection -> IO (ObjectId, [Global])
getRegistry conn = do
  reg <- newObject conn
  acc <- newIORef []
  setListener conn reg $ \opcode body -> case opcode of
    -- global(name, interface, version)
    0 -> do
      (name, iface, ver) <-
        decode ((,,) <$> getWord32 <*> getString <*> getWord32) body
      modifyIORef' acc (Global name iface ver :)
    -- global_remove(name); nothing we bind is expected to vanish
    _ -> pure ()
  -- wl_display.get_registry(new_id wl_registry)
  request conn displayId 1 (argObject reg)
  roundtrip conn
  globals <- reverse <$> readIORef acc
  pure (reg, globals)

-- | Bind a global, clamping to the version the server offers. Returns
-- 'Nothing' if the interface is absent or older than @minVersion@.
bindGlobal
  :: Connection
  -> ObjectId      -- ^ the registry
  -> [Global]
  -> ByteString    -- ^ interface name
  -> Word32        -- ^ minimum acceptable version
  -> Word32        -- ^ maximum version this client understands
  -> IO (Maybe (ObjectId, Word32))
bindGlobal conn reg globals iface minVersion maxVersion =
  case filter ((== iface) . globalInterface) globals of
    [] -> pure Nothing
    (g:_)
      | globalVersion g < minVersion -> pure Nothing
      | otherwise -> do
          let ver = min maxVersion (globalVersion g)
          oid <- newObject conn
          -- wl_registry.bind(name, new_id)
          request conn reg 0 (argUInt (globalName g) <> argNewIdAny iface ver oid)
          pure (Just (oid, ver))
