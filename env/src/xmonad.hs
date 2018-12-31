{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (catch, IOException, throwIO, bracket)
import Control.Monad
import Control.Monad.Loops (whileM)
import Data.Char (isSpace)
import Data.IORef
import Data.List (intercalate, isInfixOf)
import Data.Maybe
import Data.Monoid
import Data.Time (getCurrentTime)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Info
import System.Process
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FlexibleManipulate hiding (position)
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SimpleDate (date)
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo (ifWindows)
import XMonad.Actions.WithAll
import XMonad.Config.Gnome (gnomeRegister)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.TrackFloating
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad hiding (cmd)
import XMonad.Util.Run
import XMonad.Util.SessionStart
import qualified Data.Map as M
import qualified Debug.Trace
import qualified XMonad.StackSet as W

import Background
import Bluetooth
import Byzanz
import Constants
import DoOnce
import Gist
import Misc
import Prompt
import RedShift
import Spotify
import TallWheel
import Todoist
import XInput
import qualified Brightness

main :: IO ()
main = do
  -- Note: ewmh is used so that keynav can get active window - see
  -- https://github.com/JamshedVesuna/vim-markdown-preview/issues/37
  xmonad $ ewmh $ def
    { borderWidth = 0 -- Focus indicated and determined by mouse.
    , modMask = mod4Mask
    , terminal = terminalSh
    , workspaces = workspaceNames
    , startupHook = startup
    , layoutHook = trackFloating $ TallWheel 1 (phi / 8) phi ||| Full
    , manageHook = printErrors "Manage hook" manageHooks
    -- No default key or mouse bindings
    , keys = const M.empty
    , mouseBindings = const M.empty
    }
    `additionalMouseBindings` mouse
    `additionalKeysP` keymap

-- | Startup Hook
startup :: X ()
startup = do
  isRestart <- not <$> isSessionStart
  -- First thing: Lock screen on start.
  when (not isRestart) $ spawnOnce "slock"
  printErrors "Startup hook" $ do
    gnomeRegister
    if isRestart
      then notify "Restarted"
      else notify "Started"
    setTouch Inactive
    when (not isRestart) $ do
      spawnOnce browser
      spawnOnce emacs
      spawnOnce "spotify"
      -- Set mouse pointer
      setDefaultCursor xC_left_ptr
      -- Set mouse acceleration to 4x with no threshold
      spawnOnce "xset m 4/1 0"
      -- Start keynav
      -- FIXME: have restart daemon
      spawnOnce "keynav"
      spawnOnce "redshift"
      spawnOnce "xmodmap ~/.Xmodmap"
      randomBackground
      setSessionStarted

  -- FIXME: This is for scrot. However, it seems that ~ doesn't get
  -- interpreted correctly.
  --
  -- io $ createDirectoryIfMissing True "~/pics/screenshots/"

manageHooks :: ManageHook
manageHooks
  = composeAll
  $ [ debugManageHook
    , className =? "XClock"   --> doCenterFloat
    , className =? "xmessage" --> doCenterFloat
    , className =? "Unity-fallback-mount-helper" --> doCenterFloat
    , appName =? "eog" --> doCenterFloat
    , namedScratchpadManageHook scratchpads
    , resource =? "gnome-panel" --> doShift "0"
    , className =? "desktop_window" --> doShift "0"
    , className =? "spotify" --> doShift "9"
    , manageSpawn
    ]
  where
    -- TODO: See if one of these already exists.
    debugManageHook = do
      cls <- className
      t <- title
      Debug.Trace.trace ("ManageHook window class: " ++ show cls) $
        Debug.Trace.trace ("ManageHook window title: " ++ show t) $
        return (Endo id)

scratchpads :: [NamedScratchpad]
scratchpads =
  [ urxvtPad "term" ["-title", "scratch_term"]
  , urxvtPad "ghci" ["-e", "ghci"]
  , urxvtPad "htop" ["-e", "htop"]
  , controlCenter "sound" "Sound"
  , controlCenter "display" "Displays"
  , emacsOpen "notes" "emacs-notes" ["~/notes.md"]
  , NS "power" "gnome-power-statistics" (className =? "Gnome-power-statistics") flt
  ]
 where
  urxvtPad n args = NS n
                       (unwords args')
                       ((intercalate "\NUL" args' `isInfixOf`) <$> stringProperty "WM_COMMAND")
                       flt
    where
      args' = [terminalCmd] ++ args ++ terminalArgs
  controlCenter n t = NS n
                         ("unity-control-center " ++ n)
                         (title =? t <&&> stringProperty "_GTK_APPLICATION_ID" =? "org.gnome.ControlCenter")
                         flt
  emacsOpen n t args = NS n
                          (unwords ("alacritty --title" : t : "-e" : "emacs" : args))
                          (title =? t)
                          flt
  flt = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)

openScratch :: String -> X ()
openScratch = namedScratchpadAction scratchpads

mouse :: [((KeyMask, Button), Window -> X ())]
mouse = [((mod4Mask, button1), printErrors "mouse handler:" . mouseWindow discrete)]

keymap :: [(String, X ())]
keymap =
  map printHandlerErrors $
  -- mod-[1..],       Switch to workspace N
  -- mod-shift-[1..], Move client to workspace N
  -- mod-ctrl-[1..],  Switch to workspace N on other screen
  [ (m ++ "M-" ++ i, warpMid $ f i)
  | i <- workspaceNames
  , (f, m) <- [ (windows . W.greedyView, "")
              , (windows . W.shift, "S-")
              , ((nextScreen >>) . windows . W.greedyView, "C-")
              ]
  ] ++
  [
  -- Recompile and restart XMonad
    ("M-q", do
     notify "Recompile + restart"
     spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

  -- Layout manipulation
  , ("M-<Space>", warpMid $ sendMessage NextLayout)

   -- Focus / switch windows between screens
  , ("M-u", warpMid $ viewScreen screenOrder $ P 2)
  , ("M-i", warpMid $ viewScreen screenOrder $ P 1)
  , ("M-o", warpMid $ viewScreen screenOrder $ P 0)
  , ("M-S-u", warpMid $ sendToScreen screenOrder $ P 2)
  , ("M-S-i", warpMid $ sendToScreen screenOrder $ P 1)
  , ("M-S-o", warpMid $ sendToScreen screenOrder $ P 0)

  -- Window navigation / manipulation
  , ("M-k",   warpMid $ windows W.focusDown)
  , ("M-j",   warpMid $ windows W.focusUp)
  , ("M-S-k", warpMid $ windows W.swapDown)
  , ("M-S-j", warpMid $ windows W.swapUp)

  -- Window kill
  , ("M-S-c", kill)

  -- Focus / switch master
  , ("M-h",   warpMid $ windows W.focusMaster)
  , ("M-S-h", warpMid dwmpromote)

  -- Sink floating windows
  , ("M-t", withFocused $ windows . W.sink) -- from default
  , ("M-S-t", sinkAll)

  -- Change number of windows in master region
  , (("M-,"), warpMid . sendMessage $ IncMasterN 1)
  , (("M-."), warpMid . sendMessage $ IncMasterN (-1))

  -- Change size of master region
  , ("M-l", sendMessage Shrink)
  , ("M-;", sendMessage Expand)

  -- Start programs or navigate to them
  , ("M-p", shellPrompt (xpconfig False))

  -- Spawn terminal
  , ("M-S-<Return>", spawn terminalSh)
  , ("M-c", spawn terminalSh) -- TODO: rather close to M-S-c

  -- Start common programs with one key-press
  , ("M-e", spawn emacs)
  , ("M-s", spawn "slock")

  -- Either take a screen snip and view it, or full screen snapshot.
  -- http://code.google.com/p/xmonad/issues/detail?id=476
  , ("M-r", spawn "sleep 0.2; scrot '/home/mgsloan/pics/screenshots/%Y-%m-%d_$wx$h_scrot.png' -s -e 'eog $f'")
  , ("M-S-r", byzanzPrompt (xpconfig False))

  , ("M-g M-h", gistFromClipboard "paste.hs")
  , ("M-g M-m", gistFromClipboard "paste.md")
  , ("M-g M-p", gistFromClipboard "paste.txt")

  , ("M-a M-a", openScratch "term")
  , ("M-a M-s", openScratch "sound")
  , ("M-a M-d", openScratch "display")
  , ("M-a M-h", openScratch "htop")
  , ("M-a M-g", openScratch "ghci")
  , ("M-a M-p", openScratch "power")
  , ("M-a M-n", openScratch "notes")

  , ("M-n", promptTodoistTask "TODO today: " "today")
  , ("M-S-n", promptTodoistTaskWithDate)

  , ("M-m M-m", spotifyTogglePlay)
  , ("M-m M-n", spotifyNext)
  , ("M-m M-p", spotifyPrevious)

  -- toggle redshift
  , ("M-S-w", cycleRedShift)

  , ("M-S-=", Brightness.increase)
  , ("M-S--", Brightness.decrease)
  , ("M-=", Brightness.brightest)
  , ("M--", Brightness.set 40)

  -- TODO: These bindings suck
  , ("M-b M-b", liftIO $ reconnectBluetooth ["V-MODA", "MX Ergo"])
  , ("M-b M-g", randomBackground)
  , ("M-b M-t", cycleTouch)
  , ("M-x M-x", do
        spawn "xrandr --output DP-0 --off"
        spawn "xrandr"
        spawn "xrandr --output DP-0.8 --auto --left-of eDP-1-1")
  , ("M-x M-r", do
        spawn "xrandr --output DP-0.8 --off"
        spawn "xrandr"
        spawn "xrandr --output DP-0 --auto --right-of eDP-1-1 --rotate normal")
  ]

-- | Orders screens primarily horizontally, from right to left.
screenOrder :: ScreenComparator
screenOrder =
  screenComparatorByRectangle $
  \(Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) -> compare (x2, y2) (x1, y1)

--------------------------------------------------------------------------------
-- Random desktop backgrounds

randomBackground :: X ()
randomBackground = io $ setRandomBackground "/home/mgsloan/env/backgrounds"
