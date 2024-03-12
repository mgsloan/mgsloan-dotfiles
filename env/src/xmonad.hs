module Main (main) where

import Data.List (isSuffixOf)
import System.Environment (setEnv)
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FlexibleManipulate hiding (position)
import XMonad.Actions.WithAll
import XMonad.Config.Gnome (gnomeRegister)
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.FocusTracking
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import Audio
import Background
import Bluetooth
import Byzanz
import Focus
import Gist
import Imports
import Misc
import Notes
import Prompt
import RedShift
import Roam
import ScreenLock
import Screens
import Scrot
import Spotify
import TallWheel
import Tmux
import Touchpad
import WeeklyReview

main :: IO ()
main = do
  env <- initEnv
  putStrLn $ "Unused M-alpha leaders: "
           ++ show (unusedAlphaLeaders (keymap env))
  xmonad $ ewmh $ def
    { borderWidth = 0
    , modMask = mod4Mask
    , terminal = unwords (terminalCmd : terminalArgs)
    , workspaces = workspaceNames
    , startupHook = printErrors env "Startup hook" $ withEnv env startup
    , layoutHook = focusTracking $ TallWheel 1 (phi / 8) phi ||| Full
    , manageHook = printErrors env "Manage hook" (manageHooks env)
    , keys = const M.empty
    , mouseBindings = const M.empty
    }
    `additionalMouseBindings` mouse env
    `additionalKeysP` keymap env

startup :: XX ()
startup = do
  handleStartup everyRunAction initialStartupAction
  where
    everyRunAction :: Bool -> XX ()
    everyRunAction isStart = do
      -- Registers xmonad with the session manager.
      toXX gnomeRegister
      -- Sets the mouse pointer.
      toXX $ setDefaultCursor xC_left_ptr
      -- Start redshift, to tint colors at night
      when isStart redShiftStart
      -- Periodically choose a new random background.  Recompiles will
      -- restart this, but I don't mind that.
      forkXio $ forever $ do
        liftIO $ threadDelay (60 * 60 * 1000 * 1000) -- One hour
        randomBackground
    initialStartupAction :: Xio ()
    initialStartupAction = do
      startupLogTerminals
      startupWirelessTerminals
      startupInitialApplications
      startupMisc
      -- Choose a random desktop background
      randomBackground

-- | Starts terminals that show latest errors from this boot, and most
-- recent log output from processes started by xmonad.
startupLogTerminals :: Xio ()
startupLogTerminals = do
  let baseCmd = "journalctl --output short-precise --follow"
  spawnOrReplaceInteractiveTmuxShellOn "9" "syslog"
    $ baseCmd ++ " | ccze -A"
  spawnOrReplaceInteractiveTmuxShellOn "9" "errlog"
    $ baseCmd ++ " --priority err --boot | errlog-filter | ccze -A"

-- | Starts terminals used for controlling wifi and bluetooth. In the
-- case of the bluetooth terminal,
startupWirelessTerminals :: Xio ()
startupWirelessTerminals = do
  spawnOrReplaceInteractiveTmuxShellOn "0" "bt" "bluetoothctl"
  spawnOrReplaceInteractiveTmuxShellOn "0" "wifi" "nmtui connect"

-- | Starts a tmux session running nvtop and htop.
topTerminals :: Xio ()
topTerminals = do
  killTmuxSession "tops"
  spawn terminalCmd $ terminalArgs ++
    [ "new-session", "-s", "tops"
    , "-n", "nvtop", interactiveShellCommand "nvtop"
    , ";", "new-window", "-n", "htop", interactiveShellCommand "htop"
    ]

-- | Detect screen configuration, and launch default applications on
-- appropriate workspaces.
startupInitialApplications :: Xio ()
startupInitialApplications = do
  -- This is done in another thread, because it initially blocks on
  -- getting xrandr screen configuration output.
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
    spawnOn "7" "spotify" []
    configureScreens screenConfiguration

startupMisc :: Xio ()
startupMisc = do
  -- Disable touchpad initially
  setTouchpad initialValue
  -- Start keynav, to drive mouse via keyboard
  spawn "keynav" []
  -- Start dunst, for notifications
  spawn "dunst" []
  -- Start darkman for automatic dark mode theme switch
  spawn "darkman" ["run"]
  -- Create directories used for output
  homeDir <- view envHomeDir
  createDirectoryIfMissing True (homeDir </> "pics/screenshots")
  createDirectoryIfMissing True (homeDir </> "pics/screenshots-large")
  createDirectoryIfMissing True (homeDir </> "pics/screencaps")

manageHooks :: Env -> ManageHook
manageHooks env
  = composeAll
  $ [ manageSpawn env
    -- , debugManageHook env
    , title =? "Desktop" --> doShift "0"
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
  , (f, m) <- [ (focusWorkspace, "")
              , (toXX . windows . W.shift, "S-")
              , ((toXX nextScreen >>) . focusWorkspace, "C-")
              ]
  ] ++
  [
  -- Recompile and restart XMonad
    ("M-q", forkXio $ do
     notify "Recompile + restart"
     home <- view envHomeDir
     syncSpawnStderrInfo (home </> "env/scripts/rebuild.sh") [] `onException` do
       notify "Failed recompilation"
     -- NOTE: it might be cleaner to invoke the 'restart' function
     -- directly.  However, it works within the X monad (which uses
     -- StateT), and therefore cannot be used in a forked thread. So,
     -- instead, use the X11 message queue to deliver the restart
     -- message to the handler.
     liftIO sendRestart)

  -- Layout manipulation
  , ("M-<Space>", warpMid $ toXX $ sendMessage NextLayout)

  -- Focus / switch windows between screens
  , ("M-u", focusScreen 2)
  , ("M-i", focusScreen 1)
  , ("M-o", focusScreen 0)
  , ("M-S-u", moveToScreen 2)
  , ("M-S-i", moveToScreen 1)
  , ("M-S-o", moveToScreen 0)

  -- Window navigation / manipulation
  , ("M-k", warpMid $ toXX $ windows W.focusDown)
  , ("M-j", warpMid $ toXX $ windows W.focusUp)
  , ("M-S-k", warpMid $ toXX $ windows W.swapDown)
  , ("M-S-j", warpMid $ toXX $ windows W.swapUp)

  -- Window kill
  , ("M-S-c", toXX kill)

  -- Focus / switch master
  , ("M-h", warpMid $ toXX $ windows W.focusMaster)
  , ("M-S-h", warpMid $ toXX $ dwmpromote)

  -- Sink floating windows
  , ("M-t", toXX . withFocused $ windows . W.sink) -- from default
  , ("M-S-t", toXX sinkAll)

  -- Change number of windows in master region
  , (("M-,"), warpMid . toXX . sendMessage $ IncMasterN 1)
  , (("M-."), warpMid . toXX . sendMessage $ IncMasterN (-1))

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

  -- Spotify control
  , ("M-m M-l", spotifyLikeCurrentTrack)
  , ("M-m M-m", spotifyTogglePlay)
  , ("M-<Left>", spotifyPrevious)
  , ("M-<Right>", spotifyNext)
  , ("M-<Up>", spotifyAddToVolume 5)
  , ("M-<Down>", spotifyAddToVolume (-5))
  , ("M-S-<Up>", spotifySetVolume 100)
  , ("M-S-<Down>", spotifySetVolume 0)
  , ("M-m M-d", spotifyDebugPlayerInfo)
  , ("M-S-/", forkXio spotifyNotifyTrack)

  -- Brightness control
  , ("M-S-=", spawn (_envHomeDir env </> "env/scripts/brightness-increase.sh") ["50"])
  , ("M-S--", spawn (_envHomeDir env </> "env/scripts/brightness-decrease.sh") ["50"])
  , ("M-=", spawn (_envHomeDir env </> "env/scripts/brightness-set.sh") ["100000000"])
  , ("M--", spawn (_envHomeDir env </> "env/scripts/brightness-set.sh") ["40"])

  -- Volume control
  , ("M-S-f", forkXio $ unmuteAudio >> volumeUp)
  , ("M-S-d", forkXio $ unmuteAudio >> volumeDown)
  , ("M-f", forkXio $ unmuteAudio >> volumeMax)
  , ("M-d", forkXio toggleAudio)

  -- Media button versions of audio control
  , ("<XF86AudioRaiseVolume>", forkXio $ unmuteAudio >> volumeUp)
  , ("<XF86AudioLowerVolume>", forkXio $ unmuteAudio >> volumeDown)
  , ("<XF86AudioMute>", forkXio toggleAudio)
  , ("<XF86AudioMicMute>", forkXio toggleMicrophone)

  -- Context dependent play / pause
  , ("<XF86AudioPlay>", do
      mt <- runQuery title
      forkXio $ do
        let mightBeVideo =
              case debug "mightBeVideo title" mt of
                Nothing -> False
                Just t ->
                  "Netflix - Google Chrome" == t ||
                  " - YouTube - Google Chrome" `isSuffixOf` t ||
                  " | Prime Video - Google Chrome" `isSuffixOf` t ||
                  " | Coursera - Google Chrome" `isSuffixOf` t
        if mightBeVideo
          then do
            -- For some reason, running xdotool directly doesn't work,
            -- but running it in a temrinal does.
            --
            -- TODO: runQuery getWindowId as input to this
            spawnOn "0" "urxvt" $ ["-e", "xdotool", "getwindowfocus", "key", "space"]
            spotifyStop
          else spotifyTogglePlay)

  -- Screenshotting and gif recording
  , ("M-r", forkXio scrot)
  , ("M-S-r", byzanzPrompt)

  -- Get logs for focused window
  , ("M-y", do
      mpid <- getPidOfFocus
      case mpid of
        Nothing -> return ()
        Just pid -> forkXio $ do
          allPids <- (pid :) <$> getParentPids pid
          spawn terminalCmd $ terminalArgs ++ ["new-session", unwords $
            ["bash -c \"journalctl --boot --follow"]
            ++ map (\p -> "_PID=" ++ show p) allPids ++
            ["| ccze -A | less -R\""]])

  -- Random background
  , ("M-b M-g", forkXio randomBackground)

  -- TODO: bring back something like this once it's reliable
  -- , ("M-a", addTodoistTask)

  --
  , ("M-a", addNote (_envHomeDir env </> "docs/obsidian/notes.md"))

  -- Actions which seem too specialized / one-off to have
  -- keybindings. Nicer to just have a set of commands than filling up
  -- the keyboard with random shortcuts.
  , ("M-x", actionPrompt $ M.fromList $
      [ ("weekly-review", forkXio weeklyReview)
      -- Optional daily review if I feel like it.
      , ("daily-review", forkXio dailyReview)
      , ("update-backgrounds-list", forkXio $ void updateBackgrounds)
      , ("touchpad-toggle", touchpadToggle)
      , ( "connect-headphones"
        , forkXio (bluetoothConnect "headphones" envHeadphonesUuid)
        )
      , ( "disconnect-headphones"
        , forkXio (bluetoothDisconnect "headphones" envHeadphonesUuid)
        )
      , ( "connect-receiver"
        , forkXio (bluetoothConnect "receiver" envReceiverUuid)
        )
      , ( "disconnect-receiver"
        , forkXio (bluetoothDisconnect "receiver" envReceiverUuid)
        )
      , ("xrandrize", forkXio (detectScreens >>= configureScreens))
      , ("dunst-toggle", forkXio dunstToggle)
      , ("redshift-toggle", redShiftToggle)
      -- Expose some parts of startup as commands so that they can be
      -- iterated on without doing a restart.
      , ("startup-log-terminals", forkXio startupLogTerminals)
      , ("startup-wireless-terminals", forkXio startupWirelessTerminals)
      , ("startup-initial-applications", forkXio startupInitialApplications)
      , ("startup-misc", forkXio startupMisc)
      , ("tops", forkXio topTerminals)
      , ("invert-screen", forkXio $ spawn "xrandr" ["--output", "eDP-1", "--rotate", "inverted"])
      , ("normal-screen", forkXio $ spawn "xrandr" ["--output", "eDP-1", "--rotate", "normal"])
      , ("normal-dpi", liftIO $ do
          setEnv "GDK_SCALE" "1"
          setEnv "GDK_DPI_SCALE" "1")
      , ("medium-dpi", liftIO $ do
          setEnv "GDK_SCALE" "1.5"
          setEnv "GDK_DPI_SCALE" "0.75")
      , ("high-dpi", liftIO $ do
          setEnv "GDK_SCALE" "2"
          setEnv "GDK_DPI_SCALE" "0.75")
      , ("lock", lockWorkspaceSwitching)
      , ("unlock", unlockWorkspaceSwitching)
      , ("spotify-clear-cache", spotifyClearCache)
      -- Synonym for usb-reset so that I remember
      , ("bluetooth-reset", usbReset)
      , ("usb-reset", usbReset)
      , ("gist-hs", gistFromClipboard "paste.hs")
      , ("gist-md", gistFromClipboard "paste.md")
      , ("gist-txt", gistFromClipboard "paste.txt")
      ] ++ roamTemplates)

  -- NOTE: Following keys taken by other things in this config:
  --
  -- * M-v taken by keynav, to simulate middle click paste.
  --
  -- * M-n and M-S-n taken by dunst, for clearing notifications.
  --
  -- * M-` taken by dunst, for viewing notification history.
  ]

usbReset :: XX ()
usbReset = do
  homeDir <- view envHomeDir
  spawn (homeDir </> ".local/bin/usb-reset.sh") []
