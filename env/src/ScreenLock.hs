module ScreenLock where

import XMonad.Util.SessionStart

import Imports
import Misc

-- | On initial startup, this will use the 'slock' utility to lock the
-- screen. The reason I do this, is that I think it's a cool idea to
-- load applications / set things up concurrently with the user typing
-- their password. See env/setup.md for details of how this is setup -
-- essentially just enabling gdm3 autologin.
--
-- Note that this is far from an ideal security mechanism. Many things
-- could go wrong which would cause your computer to be unlocked on
-- login.
--
-- Note that it's intentional that the initial startup action is in
-- the Xio monad. This way the startup hook can terminate quickly,
-- which gives the manage hook a chance to manage the windows created
-- by processes spawned at startup.
withScreenInitiallyLocked :: (Bool -> XX ()) -> Xio () -> XX ()
withScreenInitiallyLocked everyRunAction initialStartupAction = do
  env <- ask
  isStart <- toXX isSessionStart
  if isStart
    then do
      forkXio $
        if env ^. envNoStartupLock
          then runInitialStartupAction env
          else
            proc "slock" [] $ \slockConfig -> do
              logInfo "Attempting to use slock to lock screen"
              slockHandle <- exitOnError . startProcess $ setStdin closed slockConfig
              runInitialStartupAction env
              printAndIgnoreErrors env "check slock exit" $ checkExitCode slockHandle
              logInfo "Screen unlocked by user"
      printErrors env "everyRunAction" (everyRunAction isStart)
      toXX setSessionStarted
    else do
      forkXio $ notify "Restarted"
      printErrors env "everyRunAction" (everyRunAction isStart)
  where
    runInitialStartupAction env =
      printErrors env "initialStartupAction" initialStartupAction
    exitOnError f = f `catchAny` \err -> do
      logError $ mconcat
        [ "Exiting xmonad session due to startup failure: "
        , fromString (show err)
        ]
      exitFailure

-- | Provides the same interface as 'withScreenInitiallyLocked', but
-- does not lock the screen.
handleStartup :: (Bool -> XX ()) -> Xio () -> XX ()
handleStartup everyRunAction initialStartupAction = do
  env <- ask
  isStart <- toXX isSessionStart
  if isStart
    then do
      forkXio $ printErrors env "initialStartupAction" initialStartupAction
      printErrors env "everyRunAction" (everyRunAction isStart)
      toXX setSessionStarted
    else do
      forkXio $ notify "Restarted"
      printErrors env "everyRunAction" (everyRunAction isStart)
