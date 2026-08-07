{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE DeriveTraversable           #-}

-- | The 'X' monad, 'XConfig', 'LayoutClass' and friends.
--
-- Deliberately a close mirror of xmonad's @XMonad.Core@, because the point of
-- this package is that configs and contrib modules written against that API
-- keep working. Differences are confined to what X11 exposed and river does
-- not:
--
-- * 'XConf' holds a river 'Connection' rather than a @Display@, and there is
--   no root window.
-- * @clientMask@ and @rootMask@ are gone: river delivers exactly the events
--   the window management protocol defines, with no event mask to select.
-- * @handleEventHook@ is typed over 'RiverEvent' rather than an Xlib @Event@.
module XMonad.Core
  ( -- * The X monad
    X(..)
  , XState(..)
  , XConf(..)
  , XConfig(..)
  , runX
  , catchX
  , userCode
  , userCodeDef
  , io
  , catchIO
  , withWindowSet
  , whenJust
  , whenX
  , trace
  , warnUnimplemented
    -- * Window set types
  , WindowSet
  , WindowSpace
  , WorkspaceId
  , ScreenId(..)
  , ScreenDetail(..)
    -- * Layouts
  , LayoutClass(..)
  , Layout(..)
  , readsLayout
    -- * Messages
  , Message
  , SomeMessage(..)
  , fromMessage
  , LayoutMessages(..)
    -- * Queries and manage hooks
  , Query(..)
  , runQuery
  , liftQuery
  , ManageHook
    -- * Extensible state
  , ExtensionClass(..)
  , StateExtension(..)
    -- * River plumbing
  , RiverEvent(..)
  , RiverWindow(..)
  , RiverOutput(..)
  , RiverSeat(..)
  , LayerFocus(..)
  , layerHasFocus
  , asksRiver
  , riverRequest
  , manageDirty
  , RestartRequested(..)
  , sendRestart
  , setMainThread
    -- * Lifecycle
  , getXMonadDir
  , getXMonadDataDir
  , ConfExtension(..)
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (liftM2, unless, when)
import Control.Monad.Reader
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Default.Class (Default(..))
import Data.IORef
import Data.Int (Int32)
import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import Data.Monoid (All, Endo(..))
import Data.Typeable (Typeable, TypeRep, cast)
import System.Directory (getAppUserDataDirectory)
import System.IO (hFlush, hPutStrLn, stderr)
import qualified Control.Exception as E
import qualified Data.Map as M
import qualified Data.Set as S

import XMonad.River.Connection (Connection)
import XMonad.River.Wire (ObjectId)
import XMonad.River.X11Compat
import XMonad.StackSet (Stack, StackSet, Workspace(..))
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Window set

type WorkspaceId = String

newtype ScreenId = S Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

-- | The physical geometry of a screen. Under X11 this also carried a
-- @rect_...@ struct for xinerama; here it is just the output's rectangle in
-- river's global coordinate space.
newtype ScreenDetail = SD { screenRect :: Rectangle }
  deriving (Eq, Show, Read)

type WindowSet = StackSet WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowSpace = Workspace WorkspaceId (Layout Window) Window

--------------------------------------------------------------------------------
-- River objects

-- | Everything the compositor has told us about one window.
--
-- @river_window_v1@ delivers these as separate events, accumulated here. Note
-- that 'rwPid' is river's @unreliable_pid@: racy under PID reuse and not
-- one-to-one with windows, but exactly what @manageSpawn@ and
-- @isAutomatedBrowser@ in this config already assume.
data RiverWindow = RiverWindow
  { rwObject     :: !ObjectId
  , rwNode       :: !ObjectId
  , rwAppId      :: !(Maybe ByteString)
  , rwTitle      :: !(Maybe ByteString)
  , rwPid        :: !(Maybe Int32)
  , rwIdentifier :: !(Maybe ByteString)
  , rwParent     :: !(Maybe ObjectId)
  , rwDimensions :: !(Int32, Int32)
  , rwNew        :: !Bool
  , rwClosed     :: !Bool
  , rwHidden     :: !Bool
  } deriving (Eq, Show)

data RiverOutput = RiverOutput
  { roObject   :: !ObjectId
  , roPosition :: !(Int32, Int32)
  , roSize     :: !(Int32, Int32)
  , roRemoved  :: !Bool
  , roLayerObject :: !(Maybe ObjectId)
    -- ^ The @river_layer_shell_output_v1@ for this output, when layer shell is
    -- supported. Kept so it can be destroyed with the output.
  , roLayerArea :: !(Maybe Rectangle)
    -- ^ What is left of the output after subtracting the exclusive zones of
    -- layer surfaces — the space a bar or dock has reserved. 'Nothing' until
    -- the first @non_exclusive_area@ event, and when layer shell is absent.
  } deriving (Eq, Show)

data RiverSeat = RiverSeat
  { rsObject  :: !ObjectId
  , rsRemoved :: !Bool
  , rsLayerObject :: !(Maybe ObjectId)
  , rsLayerFocus  :: !LayerFocus
  } deriving (Eq, Show)

-- | Whether a layer surface holds keyboard focus on a seat.
--
-- The distinction that matters to the window manager is not exclusive versus
-- non-exclusive but focused versus not: in both focused cases river gives the
-- layer surface the keyboard, and setting window focus in the same manage
-- sequence would take it away again. That is what makes a fuzzel prompt usable
-- rather than an unfocused rectangle.
data LayerFocus
  = LayerFocusNone
  | LayerFocusNonExclusive
  | LayerFocusExclusive
  deriving (Eq, Show)

-- | Does a layer surface currently own the keyboard on this seat?
layerHasFocus :: LayerFocus -> Bool
layerHasFocus LayerFocusNone = False
layerHasFocus _              = True

-- | Events the river layer surfaces to @handleEventHook@.
--
-- This replaces Xlib's @Event@. It is deliberately small: most of what an
-- X11 window manager learns from raw events, river delivers as accumulated
-- state on 'RiverWindow' instead.
data RiverEvent
  = WindowAdded !ObjectId
  | WindowClosed !ObjectId
  | WindowTitleChanged !ObjectId !(Maybe ByteString)
  | WindowAppIdChanged !ObjectId !(Maybe ByteString)
  | OutputAdded !ObjectId
  | OutputRemoved !ObjectId
  | SeatAdded !ObjectId
  | SeatRemoved !ObjectId
  | SessionLocked
  | SessionUnlocked
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- The X monad

data XState = XState
  { windowset        :: !WindowSet
    -- ^ workspace list
  , mapped           :: !(S.Set Window)
    -- ^ windows currently shown
  , waitingUnmap     :: !(M.Map Window Int)
    -- ^ retained for API compatibility; river has no unmap race to track
  , dragging         :: !(Maybe (Position -> Position -> X (), X ()))
  , extensibleState  :: !(M.Map String (Either String StateExtension))
  , numberlockMask   :: !KeyMask
    -- ^ retained for API compatibility; river resolves modifiers itself
  }

data XConf = XConf
  { config        :: !(XConfig Layout)
  , riverConn     :: !Connection
  , riverManager  :: !ObjectId
  , riverBindings :: !ObjectId
    -- ^ the @river_xkb_bindings_v1@ global
  , riverWindows  :: !(IORef (M.Map ObjectId RiverWindow))
  , riverOutputs  :: !(IORef (M.Map ObjectId RiverOutput))
  , riverSeats    :: !(IORef (M.Map ObjectId RiverSeat))
  , riverDirty    :: !(IORef Bool)
    -- ^ set when state changed outside a manage sequence, so that one must be
    -- requested with @manage_dirty@
  , inManageSeq   :: !(IORef Bool)
    -- ^ guards requests that river only permits during a manage sequence
  , riverRestart  :: !(IORef (Maybe String))
    -- ^ command to exec once river confirms the window manager has stopped
  , normalBorder  :: !(Word32, Word32, Word32, Word32)
  , focusedBorder :: !(Word32, Word32, Word32, Word32)
  , keyActions    :: !(M.Map (KeyMask, KeySym) (X ()))
  , buttonActions :: !(M.Map (KeyMask, Button) (Window -> X ()))
  , mouseFocused  :: !Bool
  , mousePosition :: !(Maybe (Position, Position))
  , currentEvent  :: !(Maybe RiverEvent)
  }

newtype X a = X (ReaderT XConf (StateT XState IO) a)
  deriving ( Functor, Applicative, Monad, MonadFail, MonadIO
           , MonadState XState, MonadReader XConf )

instance Semigroup a => Semigroup (X a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (X a) where
  mempty = pure mempty

instance Default a => Default (X a) where
  def = pure def

runX :: XConf -> XState -> X a -> IO (a, XState)
runX c st (X a) = runStateT (runReaderT a c) st

-- | Run an 'X' action, catching any exception and returning it. State changes
-- made before the exception are kept, matching xmonad.
catchX :: X a -> X a -> X a
catchX (X job) (X errcase) = do
  st <- get
  c <- ask
  (a, s') <- io $ runX c st (X job) `E.catch` \e -> do
    hPutStrLn stderr (show (e :: E.SomeException))
    runX c st (X errcase)
  put s'
  pure a

-- | Run a user-supplied action, discarding both its result and any exception
-- it throws. A broken hook must not take the window manager down with it.
userCode :: X a -> X (Maybe a)
userCode a = catchX (Just <$> a) (pure Nothing)

userCodeDef :: a -> X a -> X a
userCodeDef defValue a = fromMaybe defValue <$> userCode a

io :: MonadIO m => IO a -> m a
io = liftIO

catchIO :: MonadIO m => IO () -> m ()
catchIO f = io (f `E.catch` \e -> hPutStrLn stderr (show (e :: E.SomeException)) >> hFlush stderr)

withWindowSet :: (WindowSet -> X a) -> X a
withWindowSet f = gets windowset >>= f

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

whenX :: X Bool -> X () -> X ()
whenX a f = a >>= \b -> when b f

trace :: MonadIO m => String -> m ()
trace = io . hPutStrLn stderr

-- | Complain, once per process, that a compatibility shim is doing nothing.
--
-- Silence is the wrong default for a shim that changes behaviour. A config
-- rule that never fires looks like a bug in the config, and the person
-- debugging it has no reason to suspect the compatibility layer. Warning once
-- rather than on every call matters because several of these sit in manage
-- hooks, which run for every window.
--
-- Shims whose behaviour is genuinely unobservable — 'XMonad.Hooks.ManageHelpers.isNotification',
-- where the windows in question are not toplevels at all and so never reach a
-- manage hook — should not call this.
warnUnimplemented
  :: MonadIO m
  => String  -- ^ what is unimplemented, e.g. @"isFullscreen"@
  -> String  -- ^ what happens instead, and what to do about it
  -> m ()
warnUnimplemented name explanation = io $ do
  already <- atomicModifyIORef' warnedRef $ \seen ->
    (S.insert name seen, S.member name seen)
  unless already $
    hPutStrLn stderr ("xmonad-river: " ++ name ++ " is not implemented. " ++ explanation)

{-# NOINLINE warnedRef #-}
warnedRef :: IORef (S.Set String)
warnedRef = unsafePerformIO (newIORef S.empty)

--------------------------------------------------------------------------------
-- River plumbing

asksRiver :: (XConf -> a) -> X a
asksRiver = asks

-- | Issue a river request. Requests are buffered by the connection layer and
-- flushed by the event loop.
riverRequest :: (Connection -> a) -> X a
riverRequest f = f <$> asks riverConn

-- | Ask the compositor to start a manage sequence, because state it cannot
-- see has changed. This is what makes actions triggered from forked threads
-- and timers — of which this config has many — take effect.
manageDirty :: X ()
manageDirty = do
  ref <- asks riverDirty
  io (writeIORef ref True)

-- | Thrown into the main thread to ask for a restart.
data RestartRequested = RestartRequested deriving (Show)

instance E.Exception RestartRequested

{-# NOINLINE mainThreadRef #-}
mainThreadRef :: IORef (Maybe ThreadId)
mainThreadRef = unsafePerformIO (newIORef Nothing)

-- | Record the thread running the event loop, so 'sendRestart' can reach it.
setMainThread :: IO ()
setMainThread = writeIORef mainThreadRef . Just =<< myThreadId

-- | Ask the window manager to restart itself, from any thread.
--
-- This exists for the same reason xmonad's does: 'XMonad.Operations.restart'
-- runs in 'X', which is a 'StateT' over the event loop's own state, so a
-- forked thread cannot call it. This config needs exactly that — @M-q@ forks
-- to run the rebuild script and then wants a restart.
--
-- xmonad solved it by posting a client message to the X11 event queue. There
-- is no equivalent queue here, but Haskell offers something better: an
-- asynchronous exception thrown into the event loop's thread. Under the
-- threaded runtime the loop's blocking socket read is interruptible, so this
-- takes effect immediately rather than waiting for the next event.
sendRestart :: IO ()
sendRestart = readIORef mainThreadRef >>= \case
  Just tid -> E.throwTo tid RestartRequested
  Nothing -> hPutStrLn stderr
    "xmonad-river: sendRestart called before the event loop started"

--------------------------------------------------------------------------------
-- Configuration

data XConfig l = XConfig
  { normalBorderColor  :: !String
  , focusedBorderColor :: !String
  , terminal           :: !String
  , layoutHook         :: !(l Window)
  , manageHook         :: !ManageHook
  , handleEventHook    :: !(RiverEvent -> X All)
  , workspaces         :: ![String]
  , modMask            :: !KeyMask
  , keys               :: !(XConfig Layout -> M.Map (KeyMask, KeySym) (X ()))
  , mouseBindings      :: !(XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ()))
  , borderWidth        :: !Dimension
  , logHook            :: !(X ())
  , startupHook        :: !(X ())
  , focusFollowsMouse  :: !Bool
  , clickJustFocuses   :: !Bool
  , handleExtraArgs    :: !([String] -> XConfig Layout -> IO (XConfig Layout))
  , extensibleConf     :: !(M.Map TypeRep ConfExtension)
  }

data ConfExtension = forall a. Typeable a => ConfExtension a

--------------------------------------------------------------------------------
-- Layouts

class (Show (layout a), Typeable layout) => LayoutClass layout a where
  runLayout :: Workspace WorkspaceId (layout a) a
            -> Rectangle
            -> X ([(a, Rectangle)], Maybe (layout a))
  runLayout (Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms

  doLayout :: layout a -> Rectangle -> Stack a
           -> X ([(a, Rectangle)], Maybe (layout a))
  doLayout l r s = pure (pureLayout l r s, Nothing)

  pureLayout :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
  pureLayout _ r s = [(W.focus s, r)]

  emptyLayout :: layout a -> Rectangle -> X ([(a, Rectangle)], Maybe (layout a))
  emptyLayout _ _ = pure ([], Nothing)

  handleMessage :: layout a -> SomeMessage -> X (Maybe (layout a))
  handleMessage l = pure . pureMessage l

  pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
  pureMessage _ _ = Nothing

  description :: layout a -> String
  description = show

-- | An existentially wrapped layout, so that a workspace can hold any of them.
data Layout a = forall l. (LayoutClass l a, Read (l a)) => Layout (l a)

-- | Using an existing 'Layout' as a witness for the type, parse another.
readsLayout :: Layout a -> String -> [(Layout a, String)]
readsLayout (Layout l) s = [(Layout (asTypeOf x l), rs) | (x, rs) <- reads s]

instance LayoutClass Layout Window where
  runLayout (Workspace i (Layout l) ms) r =
    fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
  doLayout (Layout l) r s = fmap (fmap Layout) `fmap` doLayout l r s
  emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
  handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
  description (Layout l) = description l

instance Show (Layout a) where
  show (Layout l) = show l

--------------------------------------------------------------------------------
-- Messages

class Typeable a => Message a

data SomeMessage = forall a. Message a => SomeMessage a

fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- | River events are valid messages, so layouts can respond to them the way
-- xmonad layouts respond to X events.
instance Message RiverEvent

data LayoutMessages
  = Hide
    -- ^ sent when a layout becomes non-visible
  | ReleaseResources
    -- ^ sent when the window manager is exiting or restarting
  deriving (Eq, Show)

instance Message LayoutMessages

--------------------------------------------------------------------------------
-- Queries

newtype Query a = Query (ReaderT Window X a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader Window)

runQuery :: Query a -> Window -> X a
runQuery (Query m) w = runReaderT m w

-- | Lift an 'X' action into a 'Query', ignoring the window under
-- consideration.
liftQuery :: X a -> Query a
liftQuery = Query . lift

instance Semigroup a => Semigroup (Query a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (Query a) where
  mempty = pure mempty

instance Default a => Default (Query a) where
  def = pure def

-- | Uses 'Data.Monoid.Endo' rather than a local definition, because configs
-- construct these directly -- this one's debugManageHook returns @Endo id@.
type ManageHook = Query (Endo WindowSet)

--------------------------------------------------------------------------------
-- Extensible state

class Typeable a => ExtensionClass a where
  {-# MINIMAL initialValue #-}
  initialValue :: a
  extensionType :: a -> StateExtension
  extensionType = StateExtension

data StateExtension
  = forall a. ExtensionClass a => StateExtension a
  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a

--------------------------------------------------------------------------------
-- Lifecycle

-- | Directories, following the same rules xmonad uses.
getXMonadDir, getXMonadDataDir :: MonadIO m => m FilePath
getXMonadDir = io (getAppUserDataDirectory "xmonad")
getXMonadDataDir = getXMonadDir

-- 'restart' and 'recompile' live in "XMonad.Operations", which can reach the
-- river connection.
