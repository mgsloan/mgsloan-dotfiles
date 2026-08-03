-- | Misc utilities for xmonad configuration
module Misc where

import Control.Monad.Catch
import Data.Monoid
import Data.List (isPrefixOf)
import System.Random (randomRIO)
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Warp
import XMonad.Hooks.ManageHelpers (pid)
import qualified Data.Vector.Generic as V
import qualified RIO.ByteString as BS
import qualified XMonad.StackSet as W

import Imports hiding (trace)

focusScreen :: Int -> XX ()
focusScreen = warpMid . toXX . viewScreen screenOrder . P

moveToScreen :: Int -> XX ()
moveToScreen = warpMid . toXX . sendToScreen screenOrder . P

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

notifyTruncated :: Int -> String -> Xio ()
notifyTruncated limit content =
  notify $ if length content > limit then take limit content ++ "..." else content

dunstToggle :: Xio ()
dunstToggle = syncSpawn "notify-send" ["DUNST_COMMAND_TOGGLE"]

warpMid :: XX () -> XX ()
warpMid f = do
  f
  toXX $ withWindowSet $ \ws -> do
    let focused = W.peek ws
    case focused of
      Nothing -> warpToScreen (W.screen (W.current ws)) (1/2) (1/2)
      Just _ -> warpToWindow (1/2) (1/2)

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

-- | Matches windows of a browser that was started by a test automation
-- tool such as puppeteer or playwright.
--
-- The window properties are no help on their own: an automated Chrome
-- has the same class as a normal one ("Google-chrome"). However, all of
-- these tools pass @--enable-automation@ to the browser, and
-- @_NET_WM_PID@ points at that browser process, so the flag can be read
-- back out of /proc.
isAutomatedBrowser :: Query Bool
isAutomatedBrowser = pid >>= \case
  Nothing -> return False
  Just browserPid -> liftIO $ do
    result <- tryAny $ readFileBinary ("/proc" </> show browserPid </> "cmdline")
    return $ case result of
      Left _ -> False
      -- The arguments in cmdline are NUL separated and NUL terminated.
      Right cmdline -> "--enable-automation" `elem` BS.split 0 cmdline

randomComponent :: (V.Vector v a, MonadIO m) => v a -> m a
randomComponent v = liftIO $ (v V.!) <$> randomRIO (0, V.length v - 1)

unusedAlphaLeaders :: [(String, X ())] -> [String]
unusedAlphaLeaders keymap
  = foldl' (\remaining binding -> filter (not . (`isPrefixOf` binding)) remaining)
           prefixes
           bindings
  where
    bindings = map fst keymap
    prefixes = ["M-" ++ [c] | c <- ['a'..'z']]
