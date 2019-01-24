module Screens where

import Data.List (isInfixOf)

import Imports
import Misc

data DetectedConfiguration
  = LaptopOnly
  | BigScreen
  | SideScreen
  | UnknownConfiguration
  deriving (Eq, Show)

detectScreens :: Xio DetectedConfiguration
detectScreens = do
  output <- syncSpawnAndRead "xrandr" ["--query"]
  forM_ (lines output) (logInfo . fromString)
  let hasLaptopScreen = "eDP-1-1 connected" `isInfixOf` output
      hasBigScreen = "DP-0.8 connected" `isInfixOf` output
      hasSideScreen = "DP-0 connected" `isInfixOf` output
      result = case (hasLaptopScreen, hasBigScreen, hasSideScreen) of
        (True, False, False) -> LaptopOnly
        (True, True, False) -> BigScreen
        (True, False, True) -> SideScreen
        _ -> UnknownConfiguration
  logInfo $ "Detected screen configuration: " <> fromString (show result)
  return result

configureScreens :: DetectedConfiguration -> Xio ()
configureScreens cfg = do
  env <- ask
  case cfg of
    LaptopOnly ->
      printErrors env "xrandr calls for laptop screen setup" $ do
        xrandr ["--output", "DP-0", "--off"]
        xrandr ["--output", "DP-0.8", "--off"]
    BigScreen ->
      printErrors env "xrandr calls for left screen setup" $ do
        xrandr ["--output", "DP-0", "--off"]
        xrandr ["--output", "DP-0.8", "--auto", "--left-of", "eDP-1-1"]
    SideScreen ->
      printErrors env "xrandr calls for right screen setup" $ do
        xrandr ["--output", "DP-0.8", "--off"]
        xrandr ["--output", "DP-0", "--auto", "--right-of", "eDP-1-1"]
    UnknownConfiguration ->
      return ()

xrandr :: [String] -> Xio ()
xrandr = syncSpawn "xrandr"
