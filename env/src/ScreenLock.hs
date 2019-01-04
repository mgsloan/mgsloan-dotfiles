{-# LANGUAGE OverloadedStrings #-}

module ScreenLock where

import RIO.Process
import System.Exit
import XMonad.Util.SessionStart

import Imports
import Misc

withScreenInitiallyLocked :: MX () -> MX () -> MX ()
withScreenInitiallyLocked everyStartupAction initialStartupAction = do
  env <- ask
  isStart <- toMX isSessionStart
  if isStart
    then do
      logInfo "Attempting to use slock to lock screen"
      proc "slock" [] $ \slockConfig -> do
        slockHandle <- exitOnError . startProcess $ setStdin closed slockConfig
        printErrors env "everyStartupAction" everyStartupAction
        printErrors env "initialStartupAction" initialStartupAction
        printAndIgnoreErrors env "check slock exit" $ checkExitCode slockHandle
        logInfo "Screen unlocked by user"
        toMX setSessionStarted
    else do
      notify "Restarted"
      printErrors env "everyStartupAction" everyStartupAction
  where
    exitOnError f = f `catchAny` \err -> do
      logError $ "Exiting xmonad session due to startup failure: " <> fromString (show err)
      liftIO exitFailure
