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
import RIO
import RIO.Process
import XMonad (X(..), Query(..))

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
  }

initEnv :: IO Env
initEnv = do
  _envProcessContext <- mkDefaultProcessContext
  let _envLogFunc = mkLogFunc logger
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

forkMX :: ReaderT Env IO () -> MX ThreadId
forkMX f = do
  env <- ask
  liftIO $ forkIO $ runReaderT f env

--------------------------------------------------------------------------------
-- Lenses and RIO instances for Env

makeLenses 'Env

instance HasProcessContext Env where
  processContextL = envProcessContext

instance HasLogFunc Env where
  logFuncL = envLogFunc
