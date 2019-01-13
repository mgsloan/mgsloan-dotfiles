{-# LANGUAGE OverloadedStrings #-}

module ScreenLock where

import RIO.Process
import System.Exit
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
withScreenInitiallyLocked :: XX () -> XX () -> XX ()
withScreenInitiallyLocked everyStartupAction initialStartupAction = do
  env <- ask
  isStart <- toXX isSessionStart
  if isStart
    then do
      logInfo "Attempting to use slock to lock screen"
      proc "slock" [] $ \slockConfig -> do
        slockHandle <- exitOnError . startProcess $ setStdin closed slockConfig
        printErrors env "everyStartupAction" everyStartupAction
        printErrors env "initialStartupAction" initialStartupAction
        printAndIgnoreErrors env "check slock exit" $ checkExitCode slockHandle
        logInfo "Screen unlocked by user"
        toXX setSessionStarted
    else do
      forkEnv $ notify "Restarted"
      printErrors env "everyStartupAction" everyStartupAction
  where
    exitOnError f = f `catchAny` \err -> do
      logError $ "Exiting xmonad session due to startup failure: " <> fromString (show err)
      liftIO exitFailure
