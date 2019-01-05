{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Monad where

import Control.Concurrent
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Data.ByteString.Builder.Extra (flush)
import Data.Map (Map)
import RIO
import RIO.Process
import System.Environment
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import XMonad (X(..), Query(..), ManageHook)

-- Orphan instances for XMonad types
deriving instance MonadThrow X
deriving instance MonadCatch X
deriving instance MonadThrow Query
deriving instance MonadCatch Query

newtype MX a = MX (ReaderT Env X a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadCatch, MonadThrow)

data Env = Env
  { _envProcessContext :: ProcessContext
  , _envLogFunc :: LogFunc
  , _envHomeDir :: FilePath
  , _envPidHooks :: TVar PidHooks
  , _envPid :: ProcessID
  }

type PidHooks = Map ProcessID ManageHook

initEnv :: IO Env
initEnv = do
  _envProcessContext <- mkDefaultProcessContext
  let _envLogFunc = mkLogFunc logger
  mhome <- lookupEnv "HOME"
  _envHomeDir <-
    case mhome of
      Nothing -> fail "Expected HOME environment variable to be set."
      Just home -> return home
  _envPidHooks <- newTVarIO mempty
  _envPid <- getProcessID
  return Env {..}
  where
    logger _ _ lvl msg =
      case lvl of
        LevelDebug -> putTo stdout "[debug]"
        LevelInfo -> putTo stdout "[info]"
        LevelWarn -> putTo stdout "[warn]"
        LevelError -> putTo stderr "[error]"
        LevelOther name -> putTo stdout ("[" <> display name <> "]")
      where
        putTo output prefix =
          hPutBuilder output (getUtf8Builder (prefix <> " " <> msg <> "\n") <> flush)

withEnv :: Env -> MX a -> X a
withEnv e (MX f) = runReaderT f e

toMX :: X a -> MX a
toMX = MX . lift

forkEnv :: (MonadIO m, MonadReader Env m) => ReaderT Env IO () -> m ()
forkEnv f = do
  env <- ask
  void $ liftIO $ forkIO $ runReaderT f env

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

--------------------------------------------------------------------------------
-- Lenses and RIO instances

makeLenses 'Env

instance HasProcessContext Env where
  processContextL = envProcessContext

instance HasLogFunc Env where
  logFuncL = envLogFunc
