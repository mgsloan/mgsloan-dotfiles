{-# OPTIONS_GHC -fno-warn-orphans #-}

module Monad where

import Control.Concurrent (forkIO)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Data.ByteString.Builder.Extra (flush)
import Prelude hiding (readFile)
import RIO
import RIO.ByteString (readFile)
import RIO.FilePath
import RIO.Process
import System.Environment
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.IO.Unsafe (unsafePerformIO)
import XMonad (X(..), Query(..), ManageHook)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Process.Typed as P

import Constants
import Spotify.Types

-- Orphan instances for XMonad types
deriving instance MonadThrow X
deriving instance MonadCatch X
deriving instance MonadThrow Query
deriving instance MonadCatch Query

-- | eXtended X monad, adds a reader environment compatible with rio.
newtype XX a = XX (ReaderT Env X a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadCatch
           , MonadThrow, MonadFail
           )

-- | eXtended IO monad, adds a reader environment compatible with
-- rio. This is similar to the 'XX' monad, except that it does not
-- have the 'XConf' environment or 'XState' state of the 'X'
-- monad.
--
-- This is desired in many cases, because the use of 'StateT' by 'X'
-- and 'XX' causes actions in these monads to need to be run
-- sequentially. So, this monad is used for actions that can be run
-- concurrently.
newtype Xio a = Xio (ReaderT Env IO a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadCatch
           , MonadThrow, MonadFail, MonadUnliftIO
           )

-- | Reader environment for 'XX' and 'Xio' monads.
data Env = Env
  { _envProcessContext :: !ProcessContext
  , _envLogFunc :: !LogFunc
  , _envHomeDir :: !FilePath
  , _envPidHooks :: !(TVar PidHooks)
  , _envPid :: !ProcessID
  , _envSystemdCatWorks :: !Bool
  , _envHeadphonesUuid :: !(Maybe Text)
  , _envReceiverUuid :: !(Maybe Text)
  , _envBackgroundsVar :: !(MVar (Maybe (V.Vector FilePath)))
  , _envNoStartupLock :: !Bool
  , _envSpotifyNoDbus :: !Bool
  , _envSpotify :: !(Maybe Spotify)
  }

type PidHooks = Map ProcessID ManageHook

initEnv :: IO Env
initEnv = do
  _envProcessContext <- mkDefaultProcessContext
  let _envLogFunc = initialLogFunc
  mhome <- lookupEnv "HOME"
  _envHomeDir <-
    case mhome of
      Nothing -> fail "Expected HOME environment variable to be set."
      Just home -> return home
  _envPidHooks <- newTVarIO mempty
  _envPid <- getProcessID
  _envSystemdCatWorks <- checkSystemdCatWorks _envLogFunc
  _envHeadphonesUuid <- readUuid _envLogFunc _envHomeDir "headphones"
  _envReceiverUuid <- readUuid _envLogFunc _envHomeDir "receiver"
  _envBackgroundsVar <- newMVar Nothing
  _envNoStartupLock <- (Just "true" ==) <$> lookupEnv "XMONAD_NO_STARTUP_LOCK"
  _envSpotifyNoDbus <- (Just "true" ==) <$> lookupEnv "SPOTIFY_NO_DBUS"
  sRefreshToken <-
    fmap (SpotifyRefreshToken . T.strip) <$>
    readToken _envLogFunc _envHomeDir "spotify.refresh_token"
  sClientId <-
    fmap (SpotifyClientId . T.strip) <$>
    readToken _envLogFunc _envHomeDir "spotify.client_id"
  sClientSecret <-
    fmap (SpotifyClientSecret . T.strip) <$>
    readToken _envLogFunc _envHomeDir "spotify.client_secret"
  sAccessTokenRef <- newIORef Nothing
  let _envSpotify=
        Spotify
          <$> sClientId
          <*> sClientSecret
          <*> sRefreshToken
          <*> pure sAccessTokenRef
  return Env {..}
  where

initialLogFunc :: LogFunc
initialLogFunc = mkLogFunc logger
  where
    logger _ _ lvl msg =
      case lvl of
        LevelDebug -> putTo stdout "[debug]"
        LevelInfo -> putTo stdout "[info]"
        LevelWarn -> putTo stdout "[warn]"
        LevelError -> putTo stderr "[error]"
        LevelOther name -> putTo stdout ("[" <> display name <> "]")
      where
        putTo output prefix = hPutBuilder output $
          getUtf8Builder (prefix <> " " <> msg <> "\n") <> flush

withEnv :: Env -> XX a -> X a
withEnv e (XX f) = runReaderT f e

toXX :: X a -> XX a
toXX = XX . lift

forkXio :: (MonadIO m, MonadReader Env m) => Xio () -> m ()
forkXio (Xio f) = do
  env <- ask
  void $ liftIO $ forkIO $ runReaderT f env

runXio :: (MonadIO m, MonadReader Env m) => Xio a -> m a
runXio (Xio f) = do
  env <- ask
  liftIO $ runReaderT f env

modifyPidHooks
  :: (MonadIO m, MonadReader Env m)
  => (PidHooks -> PidHooks)
  -> m ()
modifyPidHooks f = do
  var <- asks _envPidHooks
  atomically $ modifyTVar var f

getPidHooks :: (MonadIO m, MonadReader Env m) => m PidHooks
getPidHooks = do
  var <- asks _envPidHooks
  readTVarIO var

checkSystemdCatWorks :: LogFunc -> IO Bool
checkSystemdCatWorks logFunc = do
  let cfg = P.proc "systemd-cat"
                   (systemdCatArgs ++ ["-t", "xmonad-sanity-check"])
  exitCode <- tryAny $ runProcess $
    setStdin (byteStringInput "Log message from systemd-cat sanity check.") cfg
  flip runReaderT logFunc $ case exitCode of
    Right ExitSuccess -> do
      logInfo "systemd-cat sanity check passed."
      return True
    Right (ExitFailure code) -> do
      logError $ mconcat
        [ "When checking if systemd-cat works, it exited with failure status: "
        , fromString (show code)
        , "\nNote that it is being invoked with an argument added in my patch: "
        , "https://github.com/systemd/systemd/pull/11336"
        , "\nHopefully this patch will be available in future systemd versions."
        ]
      return False
    Left err -> do
      logError $ mconcat
        [ "Error encountered while checking if systemd-cat works: "
        , fromString (show err)
        ]
      return False

readUuid :: LogFunc -> FilePath -> String -> IO (Maybe Text)
readUuid logFunc homeDir name = do
  let fp = homeDir </> "env" </> "untracked" </> name ++ ".uuid"
  eres <- tryAny $ readFile fp
  flip runReaderT logFunc $ case eres of
    Left err -> do
      logError $ mconcat
        [ "Could not read ", fromString name, ".uuid file at "
        , fromString (show fp)
        , ", so bindings for (dis)connecting bluetooth headphones won't work."
        , " Error was:\n"
        , fromString (show err)
        ]
      return Nothing
    Right (T.lines . decodeUtf8Lenient -> (uuid : _)) -> do
      logInfo $ mconcat ["UUID of " <> fromString name <> " is ", display uuid]
      return (Just uuid)
    Right _ -> do
      logError $ mconcat
        [ "Did not expect "
        , fromString (show fp)
        , " to be empty."
        ]
      return Nothing

readToken :: LogFunc -> FilePath -> String -> IO (Maybe Text)
readToken logFunc homeDir name = do
  let fp = homeDir </> "env" </> "untracked" </> name
  eres <- tryAny $ readFile fp
  flip runReaderT logFunc $ case eres of
    Left err -> do
      logError $ mconcat
        [ "Could not read ", fromString name, " file at "
        , fromString (show fp)
        , ". Error was:\n"
        , fromString (show err)
        ]
      return Nothing
    Right (T.lines . decodeUtf8Lenient -> (uuid : _)) ->
      return (Just uuid)
    Right _ -> do
      logError $ mconcat
        [ "Did not expect "
        , fromString (show fp)
        , " to be empty."
        ]
      return Nothing

--------------------------------------------------------------------------------
-- Logging in pure context

-- | This is similar to the utilities in 'Debug.Trace', in that it
-- creates logging output without requiring IO.  However, rather than
-- using stdout directly, it uses the same logging code as everything
-- else. In my relatively contrarian opinion it's morally fine for
-- pure code to add to log output.
--
-- Use 'Show' to print a value to the log, with the specified name
-- to be included in a prefix of the log.
debug :: Show a => Utf8Builder -> a -> a
debug name x = unsafePerformIO doLogging `seq` x
  where
    doLogging =
      flip runReaderT initialLogFunc $ logDebug $ mconcat
        [ "**** DEBUG TRACE ", name,  ": "
        , fromString (show x)
        ]

--------------------------------------------------------------------------------
-- Lenses and RIO instances

makeLenses 'Env

instance HasProcessContext Env where
  processContextL = envProcessContext

instance HasLogFunc Env where
  logFuncL = envLogFunc
