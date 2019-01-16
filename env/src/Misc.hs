-- | Misc utilities for xmonad configuration
module Misc where

import Control.Monad.Catch
import Data.Monoid
import System.Random (randomRIO)
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Warp
import qualified Data.Vector.Generic as V

import Imports hiding (trace)

focusScreen :: Int -> XX ()
focusScreen = warpMid . viewScreen screenOrder . P

moveToScreen :: Int -> XX ()
moveToScreen = warpMid . sendToScreen screenOrder . P

-- | Orders screens primarily horizontally, from right to left.
screenOrder :: ScreenComparator
screenOrder =
  screenComparatorByRectangle $
  \(Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) -> compare (x2, y2) (x1, y1)

nxt :: (Eq a, Enum a, Bounded a) => a -> a
nxt x | x == maxBound = minBound
      | otherwise = succ x

notify :: String -> Xio ()
notify msg = do
  homeDir <- view envHomeDir
  logInfo $ "XMonad notify: " <> fromString msg
  syncSpawn "notify-send" ["-i", homeDir </> "env/xmonad.png", "XMonad", msg]

dunstToggle :: Xio ()
dunstToggle = syncSpawn "notify-send" ["DUNST_COMMAND_TOGGLE"]

warpMid :: X () -> XX ()
warpMid f = toXX (f >> warpToWindow (1/2) (1/2))

printHandlerErrors :: Env -> (String, X ()) -> (String, X ())
printHandlerErrors env (k, f) =
  (k, printErrors env ("handler for " <> fromString k) f)

printErrors
  :: (MonadIO m, MonadCatch m)
  => Env -> Utf8Builder -> m a -> m a
printErrors env name f = f `catchAny` \err -> do
  liftIO $ flip runReaderT env $
    logError $ "Error within " <> name <> ": " <> fromString (show err)
  throwM err

printAndIgnoreErrors
  :: (MonadIO m, MonadCatch m)
  => Env -> Utf8Builder -> m () -> m ()
printAndIgnoreErrors env name f = f `catchAny` \err -> do
  liftIO $ flip runReaderT env $
    logError $ "Error within " <> name <> ": " <> fromString (show err)

debugManageHook :: Env -> ManageHook
debugManageHook env = do
  cls <- className
  t <- title
  liftIO $ flip runReaderT env $ do
    logDebug $ "ManageHook window class: " <> fromString (show cls)
    logDebug $ "ManageHook window title: " <> fromString (show t)
  return (Endo id)

randomIndex :: V.Vector v a => v a -> IO a
randomIndex v = (v V.!) <$> randomRIO (0, V.length v - 1)
