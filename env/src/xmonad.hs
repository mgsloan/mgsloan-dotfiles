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
import Screens
import Spotify
import TallWheel
import Touchpad
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

startup :: XX ()
startup = do
  withScreenInitiallyLocked everyRunAction initialStartupAction
  where
    everyRunAction :: Bool -> XX ()
    everyRunAction isStart = do
      -- Registers xmonad with the session manager.
      toXX gnomeRegister
      -- Sets the mouse pointer.
      toXX $ setDefaultCursor xC_left_ptr
      -- Start redshift, to tint colors at night
      when isStart startRedShift
    initialStartupAction :: Xio ()
    initialStartupAction = do
      -- Start terminals that show latest errors from this boot, and
      -- most recent log output from processes started by xmonad.
      spawnOn "0" "urxvt" (terminalArgs ++
        [ "new-session", "nmtui", ";"
        , "new-window", "-t", "btctl", "bluetoothctl", ";"
        , "new-window", "journalctl -f", ";"
        , "new-window", "journalctl -p err -b -f"
        ])
      spawnOn "0" "gnome-control-center" ["sound"]
      spawnOn "8" "edit_cfg" []
      -- Detect screen configuration, and launch default applications
      -- on appropriate workspaces.
      forkXio $ do
        screenConfiguration <- detectScreens
        let spawnEmacs ws = spawnOn ws "emacs" []
            spawnChrome ws = spawnOn ws "google-chrome" []
        case screenConfiguration of
          BigScreen -> do
            spawnEmacs "1"
            spawnChrome "1"
          _ -> do
            spawnEmacs "1"
            spawnChrome "2"
        spawnOn "9" "spotify" []
        configureScreens screenConfiguration
      -- Disable touchpad initially
      setTouchpad initialValue
      -- Set mouse acceleration to 4x with no threshold
      spawn "xset" ["m", "4/1", "0"]
      -- Start keynav, to drive mouse via keyboard
      spawn "keynav" []
      -- Apply keyboard remappings
      home <- view envHomeDir
      spawn "xmodmap" [home </> ".Xmodmap"]
      -- Choose a random desktop background
      randomBackground

manageHooks :: Env -> ManageHook
manageHooks env
  = composeAll
  $ [ debugManageHook env
    , manageSpawn env
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
    ("M-q", forkXio $ do
     notify "Recompile + restart"
     home <- view envHomeDir
     syncSpawnStderrInfo (home </> "env/rebuild.sh") []
     -- NOTE: it might be cleaner to invoke the 'restart' function
     -- directly.  However, it works within the X monad (which uses
     -- StateT), and therefore cannot be used in a forked thread. So,
     -- instead, use the X11 message queue to deliver the restart
     -- message to the handler.
     liftIO sendRestart)

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
  , ("M-S-c", toXX kill)

  -- Focus / switch master
  , ("M-h", warpMid $ windows W.focusMaster)
  , ("M-S-h", warpMid dwmpromote)

  -- Sink floating windows
  , ("M-t", toXX . withFocused $ windows . W.sink) -- from default
  , ("M-S-t", toXX sinkAll)

  -- Change number of windows in master region
  , (("M-,"), warpMid . sendMessage $ IncMasterN 1)
  , (("M-."), warpMid . sendMessage $ IncMasterN (-1))

  -- Change size of master region
  , ("M-l", toXX $ sendMessage Shrink)
  , ("M-;", toXX $ sendMessage Expand)

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
  , ("M-S-=", toXX Brightness.increase)
  , ("M-S--", toXX Brightness.decrease)
  , ("M-=", toXX Brightness.brightest)
  , ("M--", toXX $ Brightness.set 40)

  -- TODO: These bindings suck
  -- , ("M-b M-b", liftIO $ reconnectBluetooth ["V-MODA", "MX Ergo"])
  , ("M-b M-g", randomBackground)
  , ("M-b M-t", cycleTouchpad)
  , ("M-b M-h", forkXio connectHeadphones)
  , ("M-b M-S-h", forkXio disconnectHeadphones)
  , ("M-x M-x", forkXio (detectScreens >>= configureScreens))
  ]
