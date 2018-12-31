{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Misc utilities for xmonad configuration
module Misc where

import Control.Monad.Catch
import Control.Exception.Safe (catchAny)
import Data.Char
import Debug.Trace (trace)
import System.IO
import XMonad hiding (trace)
import XMonad.Actions.Warp

debug :: Show a => a -> a
debug x = trace ("xmonad debug: " ++ show x) x

nxt :: (Eq a, Enum a, Bounded a) => a -> a
nxt x | x == maxBound = minBound
      | otherwise = succ x

{- TODO: figure out initial manage hook
runManageHookOnAll :: ManageHook -> X ()
runManageHookOnAll mh = void $ withWindowSet $ \s -> do
  mapM
    (\w -> runQuery mh w)
    (W.allWindows s)
-}

readToken :: FilePath -> X String
readToken = liftIO . fmap (takeWhile (not . isSpace)) . readFile

notify :: String -> X ()
-- FIXME: This is broken. Mostly works, but assumes haskell string
-- escaping == bash string escaping
notify msg = do
  liftIO $ putStrLn $ "XMonad notify: " ++ msg
  spawn $ "notify-send -i ~/env/xmonad.png XMonad " ++ show msg

warpMid :: X () -> X ()
warpMid = (>> warpToWindow (1/2) (1/2))

printHandlerErrors :: (String, X ()) -> (String, X ())
printHandlerErrors (k, f) =
  (k, printErrors ("Handler for " ++ k) f)

printErrors :: (MonadIO m, MonadCatch m) => String -> m () -> m ()
printErrors name f = f `catchAny` \err ->
  putErr $ "Error within " ++ name ++ ": " ++ show err

putErr :: MonadIO m => String -> m ()
putErr = liftIO . hPutStrLn stderr

deriving instance MonadThrow X
deriving instance MonadCatch X
deriving instance MonadThrow Query
deriving instance MonadCatch Query
