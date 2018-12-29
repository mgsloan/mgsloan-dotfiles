-- | Misc utilities for xmonad configuration
module Misc where

import XMonad hiding (trace)
import XMonad.Actions.Warp
import Data.Char
import Debug.Trace (trace)

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
