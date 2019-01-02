{-# LANGUAGE OverloadedStrings #-}

-- | Misc utilities for xmonad configuration
module Misc where

import Control.Monad.Catch
import Control.Monad.Trans.Class
import Data.Char
import Debug.Trace (trace)
import System.IO
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Warp
import Data.Text.Encoding (decodeUtf8With)

import Imports hiding (trace)

focusScreen :: Int -> MX ()
focusScreen = warpMid . viewScreen screenOrder . P

moveToScreen :: Int -> MX ()
moveToScreen = warpMid . sendToScreen screenOrder . P

-- | Orders screens primarily horizontally, from right to left.
screenOrder :: ScreenComparator
screenOrder =
  screenComparatorByRectangle $
  \(Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) -> compare (x2, y2) (x1, y1)

{- TODO: trace utility that uses logger
debug :: Show a => a -> a
debug x = trace ("xmonad debug: " ++ show x) x
-}

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

notify :: String -> MX ()
notify msg = do
  logInfo $ "XMonad notify: " <> fromString msg
  spawn "notify-send" ["-i", "~/env/xmonad.png", "XMonad", msg]

warpMid :: X () -> MX ()
warpMid f = toMX (f >> warpToWindow (1/2) (1/2))

printHandlerErrors :: Env -> (String, X ()) -> (String, X ())
printHandlerErrors env (k, f) =
  (k, printErrors env ("handler for " <> fromString k) f)

printErrors :: (MonadIO m, MonadCatch m) => Env -> Utf8Builder -> m a -> m a
printErrors env name f = f `catchAny` \err -> do
  liftIO $ flip runReaderT env $
    logError $ "Error within " <> name <> ": " <> fromString (show err)
  throwM err
