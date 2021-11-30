module Process
  ( spawn
  , spawnOn
  , spawnAndDo
  , spawnStderrInfo
  , spawnAndNotifyFail
  , syncSpawn
  , syncSpawnStderrInfo
  , syncSpawnAndRead
  , syncSpawnAndReadInheritStdin
  , manageSpawn
  , getParentPids
  , getPidOfFocus
  , runQuery
  ) where

import RIO
import RIO.Process
import Safe
import System.Posix.Types (ProcessID)
import System.Process.Typed (Process(pHandle))
import XMonad (WorkspaceId, ManageHook, Query, doShift, withWindowSet)
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.Process as P
import qualified System.Process.Internals as P
import qualified System.Process.Typed as PT
import qualified XMonad
import qualified XMonad.Hooks.ManageHelpers as MH
import qualified XMonad.StackSet as W

import Constants
import Monad

spawn :: (MonadIO m, MonadReader Env m) => FilePath -> [String] -> m ()
spawn cmd args = forkXio $ syncSpawn cmd args

spawnStderrInfo :: (MonadIO m, MonadReader Env m) => FilePath -> [String] -> m ()
spawnStderrInfo cmd args = forkXio $ syncSpawnStderrInfo cmd args

spawnAndNotifyFail :: (MonadIO m, MonadReader Env m) => FilePath -> [String] -> m ()
spawnAndNotifyFail cmd args = forkXio $ syncSpawnAndNotifyFail cmd args

syncSpawn :: FilePath -> [String] -> Xio ()
syncSpawn = syncSpawnImpl systemdCatArgs

syncSpawnStderrInfo :: FilePath -> [String] -> Xio ()
syncSpawnStderrInfo = syncSpawnImpl systemdCatStderrInfoArgs

syncSpawnImpl :: [String] -> FilePath -> [String] -> Xio ()
syncSpawnImpl catArgs cmd args =
  withCurrentEnv $ loggedProc catArgs cmd args $ runProcess_ . setStdin closed

syncSpawnAndRead :: FilePath -> [String] -> Xio String
syncSpawnAndRead cmd args =
  withCurrentEnv $
  proc cmd args $
    fmap lazyBytesToString . readProcessStdout_ . setStdin closed

-- TODO: Once httpie is released with my patch perhaps this can be
-- removed https://github.com/jakubroztocil/httpie/pull/791
syncSpawnAndReadInheritStdin :: FilePath -> [String] -> Xio String
syncSpawnAndReadInheritStdin cmd args =
  withCurrentEnv $
  proc cmd args $
    fmap lazyBytesToString . readProcessStdout_

syncSpawnAndNotifyFail :: (MonadIO m, MonadReader Env m) => FilePath -> [String] -> m ()
syncSpawnAndNotifyFail cmd args = forkXio $ withCurrentEnv $ do
  cmdPath <- findExecutable cmd >>= either throwIO return
  (ec, o, e) <- PT.readProcess $ setStdin closed $ PT.proc cmdPath args
  case ec of
    ExitSuccess{} -> return ()
    ExitFailure{} ->
      syncSpawn "notify-send"
        [ unlines
          [ cmd ++ " exited with failure:"
          , "==== stderr:"
          , BS8.unpack e
          , "==== stdout:"
          , BS8.unpack o
          ]
        ]

loggedProc
  :: (MonadIO m, MonadReader Env m)
  => [String]
  -> FilePath
  -> [String]
  -> (ProcessConfig () () () -> m a)
  -> m a
loggedProc catArgs cmd args f = do
  cmdPath <- findExecutable cmd >>= either throwIO return
  -- Idea here is to not require running systemd-cat if it didn't work
  -- on startup. This way it isn't strictly needed in order to have a
  -- working xmonad.
  useSystemdCat <- view envSystemdCatWorks
  if useSystemdCat
    then do
      proc "systemd-cat" (catArgs ++ (cmdPath : args)) f
    else do
      logError $ mconcat
        [ "Not logging process output properly because "
        , "systemd-cat sanity check failed on xmonad start"
        ]
      proc cmd args f

--------------------------------------------------------------------------------
-- Spawning on specific workspaces
--
-- (inspired by code from XMonad.Actions.SpawnOn)

spawnOn
  :: (MonadIO m, MonadFail m, MonadReader Env m)
  => WorkspaceId -> FilePath -> [String] -> m ()
spawnOn workspace = spawnAndDo (doShift workspace)

spawnAndDo
  :: (MonadIO m, MonadFail m, MonadReader Env m)
  => ManageHook -> FilePath -> [String] -> m ()
spawnAndDo mh cmd args = do
  pidVar <- liftIO newEmptyMVar
  -- Fork a thread for managing the process.
  forkXio $ withCurrentEnv $ loggedProc systemdCatArgs cmd args $ \cfg0 ->
    liftIO $ withProcessWait (setStdin closed cfg0) $ \process -> do
      putMVar pidVar =<< getPid (pHandle process)
      checkExitCode process
  -- Expect to get a ProcessID for it within 100ms.
  result <- liftIO $ timeout (100 * 1000) $ tryAny $ takeMVar pidVar
  pid <- case result of
    Nothing ->
      fail "Timed out before process ID MVar filled"
    Just (Left e) ->
      fail $ "Error waiting for process ID MVar: " ++ show e
    Just (Right Nothing) ->
      fail "Process exited before process ID could be retrieved"
    Just (Right (Just pid)) ->
      return pid
  -- Expire the association after an arbitrary time interval (10s).
  forkXio $ do
    liftIO $ threadDelay (10 * 1000 * 1000)
    modifyPidHooks $ M.delete pid
  -- Associate this ProcessID with the manage hook.
  modifyPidHooks $ M.insert pid mh

manageSpawn :: Env -> ManageHook
manageSpawn env = do
  mpid <- MH.pid
  case mpid of
    Nothing -> return mempty
    Just pid0 -> do
      let xmonadPid = env ^. envPid
      hookMap <- runReaderT getPidHooks env
      let go pid
            | pid == xmonadPid = return mempty
            | Just mh <- M.lookup pid hookMap = mh
            | otherwise = do
                mppid <- runReaderT (getParentPid pid) env
                case mppid of
                  Nothing -> return mempty
                  Just ppid -> go ppid
      go pid0

getParentPid
  :: (MonadIO m, MonadReader e m, HasLogFunc e)
  => ProcessID
  -> m (Maybe ProcessID)
getParentPid pid = do
  let fp = "/proc/" ++ show (toInteger pid) ++ "/stat"
  econtents <- liftIO $ tryAny $ readFileUtf8 fp
  case econtents of
    Right (headMay . T.lines ->
      Just (T.words -> (_:_:_:(readMay . T.unpack -> Just ppid):_))) ->
        return (Just ppid)
    _ -> do
      logError $ mconcat
        [ "Expected success reading parent pid out of "
        , fromString (show fp)
        , ", but instead got: "
        , fromString (show econtents)
        ]
      return Nothing

--------------------------------------------------------------------------------
-- Get pid(s) of focused window

getParentPids
  :: (MonadIO m, MonadReader Env m)
  => ProcessID
  -> m [ProcessID]
getParentPids pid0 = do
  xmonadPid <- view envPid
  let go pid
        | pid == xmonadPid = return []
        | otherwise = do
            mppid <- getParentPid pid
            case mppid of
              Nothing -> return []
              Just ppid -> (ppid :) <$> go ppid
  go pid0

getPidOfFocus :: XX (Maybe ProcessID)
getPidOfFocus = fmap join $ runQuery MH.pid

--------------------------------------------------------------------------------
-- Utilities

lazyBytesToString :: LByteString -> String
lazyBytesToString = bytesToString . toStrictBytes

bytesToString :: ByteString -> String
bytesToString = T.unpack . decodeUtf8Lenient

-- | returns Just pid or Nothing if process has already exited
--
-- (copy+modified from https://stackoverflow.com/a/27388709)
getPid :: P.ProcessHandle -> IO (Maybe ProcessID)
getPid ph = P.withProcessHandle ph go
  where
    go ph_ = case ph_ of
      P.OpenHandle x -> return $ Just x
      P.OpenExtHandle x _ -> return $ Just x
      P.ClosedHandle _ -> return Nothing

runQuery :: Query a -> XX (Maybe a)
runQuery q = toXX $ withWindowSet $ \w -> forM (W.peek w) $ XMonad.runQuery q

-- | Hack to allow env vars to be updated in commands.
withCurrentEnv :: (HasProcessContext env, MonadReader env m, MonadIO m) => m a -> m a
withCurrentEnv f = do
  pc <- liftIO mkDefaultProcessContext
  local (set processContextL pc) f
