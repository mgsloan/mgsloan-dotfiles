{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FlexibleManipulate hiding (position)
import XMonad.Actions.WithAll
import XMonad.Config.Gnome (gnomeRegister)
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.TrackFloating
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import Background
import Bluetooth
import Constants
import Gist
import Imports
import Misc
import Prompt
import RedShift
import ScreenLock
import Spotify
import TallWheel
import XInput
import qualified Brightness

main :: IO ()
main = do
  env <- initEnv
  xmonad $ ewmh $ def
    { borderWidth = 0
    , modMask = mod4Mask
    , terminal = terminalSh
    , workspaces = workspaceNames
    , startupHook = printErrors env "Startup hook" $ withEnv env startup
    , layoutHook = trackFloating $ TallWheel 1 (phi / 8) phi ||| Full
    , manageHook = printErrors env "Manage hook" (manageHooks env)
    , keys = const M.empty
    , mouseBindings = const M.empty
    }
    `additionalMouseBindings` mouse env
    `additionalKeysP` keymap env

startup :: MX ()
startup = do
  withScreenInitiallyLocked everyStartupAction initialStartupAction
  where
    everyStartupAction = toMX gnomeRegister
    initialStartupAction = do
      -- Disable touchpad
      setTouch Inactive
      -- Start default applications
      spawn "google-chrome" []
      spawn "emacs" []
      -- TODO: shift to workspace 0
      spawn "spotify" []
      -- Set mouse pointer
      toMX $ setDefaultCursor xC_left_ptr
      -- Set mouse acceleration to 4x with no threshold
      spawn "xset" ["m", "4/1", "0"]
      -- Start keynav, to drive mouse via keyboard
      spawn "keynav" []
      -- Start redshift, to tint colors at night
      startRedShift
      -- Apply keyboard remappings
      home <- view envHomeDir
      spawn "xmodmap" [home </> ".Xmodmap"]
      -- Start terminals that show latest errors from this boot, and
      -- most recent log output from processes started by xmonad.
      --
      -- TODO: automatically shift these to workspace 0
      spawn "urxvt" ["-e", "bash", "-c", "journalctl -p err -b -f"]
      spawn "urxvt" ["-e", "bash", "-c", "journalctl -f"]
      -- Choose a random desktop background
      randomBackground

manageHooks :: Env -> ManageHook
manageHooks env
  = composeAll
  $ [ debugManageHook env
    ]

mouse :: Env -> [((KeyMask, Button), Window -> X ())]
mouse env = [((mod4Mask, button1), mouseManipulate)]
  where
    mouseManipulate = printErrors env "mouse handler:" . mouseWindow discrete

keymap :: Env -> [(String, X ())]
keymap env =
  map (printHandlerErrors env . second (withEnv env)) $
  -- M-[1..]    Switch to workspace N
  -- M-S-[1..]  Move client to workspace N
  -- M-C-[1..]  Switch to workspace N on other screen
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
     -- TODO: Don't use sh for this
     spawn "/bin/sh" ["-c", "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"])

  -- Layout manipulation
  , ("M-<Space>", warpMid $ sendMessage NextLayout)

   -- Focus / switch windows between screens
  , ("M-u", focusScreen 2)
  , ("M-i", focusScreen 1)
  , ("M-o", focusScreen 0)
  , ("M-S-u", moveToScreen 2)
  , ("M-S-i", moveToScreen 1)
  , ("M-S-o", moveToScreen 0)

  -- Window navigation / manipulation
  , ("M-k", warpMid $ windows W.focusDown)
  , ("M-j", warpMid $ windows W.focusUp)
  , ("M-S-k", warpMid $ windows W.swapDown)
  , ("M-S-j", warpMid $ windows W.swapUp)

  -- Window kill
  , ("M-S-c", toMX kill)

  -- Focus / switch master
  , ("M-h", warpMid $ windows W.focusMaster)
  , ("M-S-h", warpMid dwmpromote)

  -- Sink floating windows
  , ("M-t", toMX . withFocused $ windows . W.sink) -- from default
  , ("M-S-t", toMX sinkAll)

  -- Change number of windows in master region
  , (("M-,"), warpMid . sendMessage $ IncMasterN 1)
  , (("M-."), warpMid . sendMessage $ IncMasterN (-1))

  -- Change size of master region
  , ("M-l", toMX $ sendMessage Shrink)
  , ("M-;", toMX $ sendMessage Expand)

  -- Start programs or navigate to them
  , ("M-p", shellPrompt)

  -- Spawn terminal
  , ("M-S-<Return>", spawn terminalCmd terminalArgs)

  -- Start common programs with one key-press
  , ("M-e", spawn "emacs" [])
  , ("M-s", spawn "slock" [])

  -- Either take a screen snip and view it, or full screen snapshot.
  -- http://code.google.com/p/xmonad/issues/detail?id=476
  -- , ("M-r", spawn "sleep 0.2; scrot '/home/mgsloan/pics/screenshots/%Y-%m-%d_$wx$h_scrot.png' -s -e 'eog $f'")
  -- , ("M-S-r", byzanzPrompt (xpconfig False))

  , ("M-g M-h", gistFromClipboard "paste.hs")
  , ("M-g M-m", gistFromClipboard "paste.md")
  , ("M-g M-p", gistFromClipboard "paste.txt")

  {- TODO: reinstate
  , ("M-n", promptTodoistTask "TODO today: " "today")
  , ("M-S-n", promptTodoistTaskWithDate)
  -}

  -- Spotify control
  , ("M-m M-m", spotifyTogglePlay)
  , ("M-m M-n", spotifyNext)
  , ("M-m M-p", spotifyPrevious)

  -- Enable / disable redshift
  , ("M-S-w", cycleRedShift)

  -- Brightness controll
  , ("M-S-=", toMX Brightness.increase)
  , ("M-S--", toMX Brightness.decrease)
  , ("M-=", toMX Brightness.brightest)
  , ("M--", toMX $ Brightness.set 40)

  -- TODO: These bindings suck
  , ("M-b M-b", liftIO $ reconnectBluetooth ["V-MODA", "MX Ergo"])
  , ("M-b M-g", randomBackground)
  , ("M-b M-t", cycleTouch)
  , ("M-x M-x", void $ forkMX $ printErrors env "xrandr calls for hidpi left screen" $ do
      syncSpawn "xrandr" ["--output", "DP-0", "--off"]
      syncSpawn "xrandr" ["--output", "DP-0.8", "--auto", "--left-of", "eDP-1-1"])
  , ("M-x M-r", void $ forkMX $ printErrors env "xrandr calls for stdpi right screen" $ do
      syncSpawn "xrandr" ["--output", "DP-0.8", "--off"]
      syncSpawn "xrandr" ["--output", "DP-0", "--auto", "--right-of", "eDP-1-1"])
  ]
