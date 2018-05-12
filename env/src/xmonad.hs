{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

-- import Network.Hoggl
-- import Network.Hoggl.Types hiding (WorkspaceId)
-- import Servant.Client
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch, IOException, throwIO, bracket)
import Control.Monad
import Control.Monad.Loops (whileM)
import Data.Char (isSpace)
import Data.IORef
import Data.List (intercalate, isInfixOf)
import Data.Maybe
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
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.TrackFloating
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad hiding (cmd)
import XMonad.Util.Run
import XMonad.Util.SessionStart
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import Control.Concurrent.Async

-- Modules defined in this repo (and not in dependencies / submodules)
import Byzanz
import Constants
import DoOnce
import Misc
import RedShift
import Synaptics
import TallWheel

-- TODO:
-- * Utility to remember paste buffers / middle click
-- * Automatic journal starter with date
-- * Spotify integration

main :: IO ()
main = do
  xmonad $ gnomeConfig
    { borderWidth = 0 -- Focus indicated and determined by mouse.
    , modMask = mod4Mask
    , terminal = terminalSh
    , workspaces = workspaceNames
    , startupHook = startup
    , layoutHook = trackFloating $ TallWheel 1 (phi / 8) phi ||| Full
    , manageHook = manageHooks
    -- No default key or mouse bindings
    , keys = const M.empty
    , mouseBindings = const M.empty
    -- FIXME: make this work better
    -- , handleRecompile = customRecompile
    }
    `additionalMouseBindings` mouse
    `additionalKeysP` keymap

warpMid :: X () -> X ()
warpMid = (>> warpToWindow (1/2) (1/2))

-- FIXME: Startup seems to be waiting for everything to start. Figure
-- out how to start everything async and in parallel. Tricky because
-- spawnOnce uses xmonad state. Can't just use (io $ forkIO $ ...)
--
-- Note, not sure that's actually what's happening here.

-- | Startup Hook
startup :: X ()
startup = do
  isRestart <- not <$> isSessionStart
  if isRestart
    then notify "Restarted"
    else notify "Started"
  setTouch Inactive
  when (not isRestart) $ do
    -- Set mouse acceleration to 4x with no threshold
    spawnOnce "xset m 4/1 0"
    -- Start keynav
    -- FIXME: have restart daemon
    spawnOnce "keynav"
    spawnOnce "redshift"
    -- TODO: Figure out if this is needed for notify-send to work
    -- spawnOnce "/usr/lib/x86_64-linux-gnu/notify-osd"
    spawnOnceOn "1" emacs
    spawnOnceOn "1" browser
    spawnOnceOn "1" terminalSh
    spawnOnceOn "9" "spotify"
    setSessionStarted

  -- FIXME: This is for scrot. However, it seems that ~ doesn't get
  -- interpreted correctly.
  --
  -- io $ createDirectoryIfMissing True "~/pics/screenshots/"

manageHooks :: ManageHook
manageHooks
  = composeAll
  $ [ className =? "XClock"   --> doCenterFloat
    , className =? "xmessage" --> doCenterFloat
    , className =? "Unity-fallback-mount-helper" --> doCenterFloat
    , appName =? "eog" --> doCenterFloat
    , namedScratchpadManageHook scratchpads
    , resource =? "gnome-panel" --> doShift "0"
    , className =? "desktop_window" --> doShift "0"
    , className =? "spotify" --> doShift "9"
    , manageSpawn
    ]

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
mouse =
    [ ((mod4Mask, button1), mouseWindow discrete)
    -- Removed to enable M-v paste binding in keynav
    -- , ((mod4Mask, button2), mouseWindow (const 0.5)) -- Position , ((mod4Mask, button3), mouseWindow (const 1)) -- Resize
    ]

keymap :: [(String, X ())]
keymap =
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
  -- Bindings from the default XMonad configuration
    ("M-S-c", kill)
  , ("M-S-q", io exitSuccess)

  -- Recompile and restart XMonad
  , ("M-q", do
     -- FIXME: This should check if the current program exists, and
     -- invoke that. Also should avoid multiple invocations of the build
     -- script.
     --
     -- FIXME: also, this should use the correct program name
     notify "Recompile + restart"
     closeRecompileWindows
     spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  , ("M-S-<Return>", spawn terminalSh)
  , ("M-<Space>", warpMid $ sendMessage NextLayout)

   -- Focus / switch windows between screens
  , ("M-i", warpMid $ viewScreen def $ P 0)
  , ("M-o", warpMid $ viewScreen def $ P 1)
  , ("M-u", warpMid $ viewScreen def $ P 2)
  , ("M-S-i", warpMid $ sendToScreen def $ P 0)
  , ("M-S-o", warpMid $ sendToScreen def $ P 1)
  , ("M-S-u", warpMid $ sendToScreen def $ P 2)

  -- Window navigation / manipulation
  , ("M-k",   warpMid $ windows W.focusDown)
  , ("M-j",   warpMid $ windows W.focusUp)
  , ("M-S-k", warpMid $ windows W.swapDown)
  , ("M-S-j", warpMid $ windows W.swapUp)

  -- Focus / switch master
  , ("M-h",   warpMid $ windows W.focusMaster)
  , ("M-S-h", warpMid dwmpromote)

  -- Add more
  , (("M-,"), warpMid . sendMessage $ IncMasterN 1)
  , (("M-."), warpMid . sendMessage $ IncMasterN (-1))

  -- Sink floating windows
  , ("M-t", withFocused $ windows . W.sink) -- from default
  , ("M-S-t", sinkAll)

  -- Change size of master region
  , ("M-l", sendMessage Shrink)
  , ("M-;", sendMessage Expand)

  -- Start programs or navigate to them
  , ("M-p", shellPrompt $ xpconfig False)

  -- Activate or deactivate touchpad (I use the thinkpad nub)
  , ("M-b", cycleTouch)

  -- Either take a screen snip and view it, or full screen snapshot.
  -- http://code.google.com/p/xmonad/issues/detail?id=476
  , ("M-r", spawn "sleep 0.2; scrot '/home/mgsloan/pics/screenshots/%Y-%m-%d_$wx$h_scrot.png' -s -e 'eog $f'")
  , ("M-S-r", byzanzPrompt (xpconfig False))

  -- Clipboard gists via https://github.com/defunkt/gist
  , ("M-g M-h", runGist "paste.hs")
  , ("M-g M-m", runGist "paste.md")
  , ("M-g M-p", runGist "paste.txt")

  -- Start common programs with one key-press
  -- TODO: rather close to M-S-c
  , ("M-c", spawn terminalSh)
  , ("M-e", spawn emacs)
  , ("M-s", spawn "slock")

  , ("M-a M-a", openScratch "term")
  , ("M-a M-s", openScratch "sound")
  , ("M-a M-d", openScratch "display")
  , ("M-a M-h", openScratch "htop")
  , ("M-a M-g", openScratch "ghci")
  , ("M-a M-p", openScratch "power")
  , ("M-a M-n", openScratch "notes")

  , ("M-n", promptTodoistTask "TODO today: " "today")
  , ("M-S-n", promptTodoistTaskWithDate)

  {-
  -- Shortcuts for common screen layouts
  , ("M-d M-l", lvdsauto >> restartKeynav)
  , ("M-d M-d", dpabove >> restartKeynav)
  , ("M-d M-v", vgaleft >> restartKeynav)
  -}

  , ("M-m M-m", spotify "PlayPause")
  , ("M-m M-n", spotify "Next")
  , ("M-m M-p", spotify "Previous")

  -- invert screen
  , ("M-w", spawn "xcalib -i -a")
  -- toggle redshift
  , ("M-S-w", cycleRedShift)

  -- TODO: Don't use dzen, instead maybe notify?
  , ("M-d M-d", date)
  -- , ("M-d M-", )

  {-
  , ("M-f M-f", promptTogglTimer)
  , ("M-f M-s", stopTogglTimer)
  -}
  ]

xpconfig :: Bool -> XPConfig
xpconfig auto
    | auto = res { autoComplete = Just 1000 }
    | otherwise = res
  where
    res = def
        { bgColor           = "black"
        , fgColor           = "white"
        , bgHLight          = "gray"
        , fgHLight          = "black"
        , borderColor       = "orange"
        , promptBorderWidth = 1
        , position          = Bottom
        , height            = 20
        , historySize       = 1000
        , promptKeymap      = km
        }
    km =
      M.insert (controlMask, xK_b) (moveWord Next) $
      M.insert (controlMask, xK_b) (moveWord Prev) $
      defaultXPKeymap

--------------------------------------------------------------------------------
-- Adding tasks to todoist

-- FIXME: Actually use this / add more todoist support

data GenericPrompt = GenericPrompt String

instance XPrompt GenericPrompt where
  showXPrompt (GenericPrompt x) = x

promptTodoistTaskWithDate :: X ()
promptTodoistTaskWithDate =
  mkXPrompt (GenericPrompt "Date: ") (xpconfig False) (const $ return []) $ \time ->
    promptTodoistTask "TODO: " time

promptTodoistTask :: String -> String -> X ()
promptTodoistTask msg time =
  mkXPrompt (GenericPrompt msg) (xpconfig False) (const $ return []) $ \content ->
    addTodoistTask time content

addTodoistTask :: String -> String -> X ()
addTodoistTask time content = do
  uid <- liftIO $ fmap show getCurrentTime
  let commandsArg = "commands='[{\"type\": \"item_add\", " ++
        "\"temp_id\":\"" ++ uid ++ "\", " ++
        "\"uuid\":\"" ++ uid ++ "\", " ++
        "\"args\":{\"content\":" ++ show content ++ ", " ++
                  "\"date_string\":" ++ show time ++ "}}]'"
  token <- readToken "/home/mgsloan/.xmonad/todoist-token"
  liftIO $ putStrLn $ "Sending todoist request with " ++ commandsArg
  spawn $ unwords $ "curl" :
    [ "--show-error"
    , "'https://todoist.com/API/v7/sync'"
    , "-d", "token='" ++ token ++ "'"
    , "-d", commandsArg]
  --FIXME: error handling
  -- when ("{\"error" `isPrefixOf` output) $
  --   spawn ("xmessage 'Todoist failed with:\n\n" ++ output ++ "'")
  -- liftIO $ putStrLn $ "Todoist response: " ++ output

--------------------------------------------------------------------------------
-- Toggl time tracking

-- FIXME: Actually use this / add more toggl support

{-
globalManager :: IORef Manager
globalManager = unsafePerformIO $ newIORef =<< newManager tlsManagerSettings
{-# NOINLINE globalManager #-}

withToggl :: (Token -> ClientM a) -> X a
withToggl f = do
  mgr <- liftIO $ readIORef globalManager
  token <- readToken "/home/mgsloan/.xmonad/toggl-token"
  let clientEnv = ClientEnv mgr togglBaseUrl
  eres <- liftIO $ runClientM (f (Api token)) clientEnv
  case eres of
    Left err -> liftIO $ do
      print err
      throwIO err
    Right res -> return res

promptTogglTimer :: X ()
promptTogglTimer =
  mkXPrompt (GenericPrompt "FIXME") (xpconfig False) (const $ return []) $ \msg -> do
    timerInfo <- withToggl $ \token -> startTimer token TES
      { tesDescription = Just (T.pack msg)
      , tesTags = []
      , tesPid = Nothing
      , tesCreatedWith = "mgsloan's xmonad.hs + the hoggl library"
      }
    notify "toggl" (T.unpack (fromMaybe "" (teDescription timerInfo)))

stopTogglTimer :: X ()
stopTogglTimer = do
  currentTimer <- withToggl $ \token -> do
    currentTimer <- currentTimeEntry token
    case currentTimer of
      Just te -> void $ stopTimer token (teId te)
      Nothing -> return ()
    return currentTimer
  case currentTimer of
    Just te -> notify summary (maybe "" T.unpack (teDescription te))
      where
        summary =
          maybe (++ " - NO CLIENT") (\c -> (++ (" - " ++ T.unpack c))) (teClient te) $
          maybe "toggl stopped - NO PROJECT " (("toggl stopped - " ++) . T.unpack) (teProject te)
    Nothing -> notify "toggl" "No timer running"
-}

{-
listTogglProjects :: X ()
listTogglProjects = do
-}

--------------------------------------------------------------------------------
-- Spotify

spotify :: String -> X ()
spotify cmd = spawn $
  "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++
  cmd

--------------------------------------------------------------------------------
-- Creating gists

runGist :: String -> X ()
runGist filename =
  runProcessWithInput "gist" (words "-P -p -f" ++ [filename]) "" >>=
  \url -> spawn (browser ++ " " ++ url)

--------------------------------------------------------------------------------
-- Managing screen setup

-- FIXME: Figure out why these crash my computer..
--
-- It is probably https://wiki.archlinux.org/index.php/xrandr#Avoid_X_crash_with_xrasengan

-- FIXME: Instead use some variety of autorandr

{-
dpabove :: X ()
dpabove = do
  lvdsauto
  xrandr ["--output", "DP-0", "--auto", "--above", "LVDS-0"]

vgaleft :: X ()
vgaleft = do
  lvdsauto
  xrandr ["--output", "VGA-0", "--auto", "--left", "LVDS-0"]

lvdsauto :: X ()
lvdsauto = do
  xrandr []
  xrandr ["--output", "VGA-0", "--off"]
  xrandr ["--output", "DP-0", "--off"]
  xrandr ["--output", "LVDS-0", "--auto", "--panning", "0x0"]

xrandr :: [String] -> X ()
xrandr = runSync "xrandr"

restartKeynav :: X ()
restartKeynav = do
  runSync "killall" ["keynav"]
  spawn "keynav"


runSync :: String -> [String] -> X ()
runSync cmd args = liftIO $ do
  void (rawSystem cmd args) `catch`
      -- Handle "does not exist (no child processes)"
      \(_ :: IOException) -> return ()
-}

customRecompile :: ForceRecompile -> IO RecompileStatus
customRecompile _ = do
    cfgdir  <- getXMonadDir
    datadir <- getXMonadDataDir
    let binn = "xmonad-"++arch++"-"++os
        bin = datadir </> binn
        statusFile = cfgdir </> "recompile_status"
    bracket uninstallSignalHandlers (\() -> installSignalHandlers) $ \() -> do
      removeFile statusFile `catch` \(_ :: IOError) -> return ()
      ph <- runProcess
        terminalCmd
        ("-title" : recompileTitle : "-e" : (cfgdir </> "build-in-terminal.sh") : [bin])
        -- FIXME: Figure out why it won't run in tmux.
        -- ("-e" : "tmux" : "-c" : (cfgdir </> "build-in-terminal.sh") : [bin])
        (Just cfgdir)
        Nothing
        Nothing
        Nothing
        Nothing
      -- FIXME: Better implementation of blocking on file existence
      let blockOnStatusFile = do
            whileM (not <$> doesFileExist statusFile) $
              -- Delay 50ms
              threadDelay $ 50 * 1000
            trace "Found status file"
            status <- readFile statusFile
            case lines status of
              ["success"] -> return RecompileSuccess
              ["failure"] -> return RecompileFailure
              _ -> do
                trace ("Unexpected status: " ++ show status)
                return RecompileFailure
      eres <- race (waitForProcess ph) blockOnStatusFile
      return $ case eres of
        Left ExitSuccess -> RecompileSuccess
        Left ExitFailure{} -> RecompileFailure
        Right res -> res

closeRecompileWindows :: X ()
closeRecompileWindows =
  ifWindows (title =? recompileTitle) (mapM_ killWindow) (return ())

recompileTitle :: String
recompileTitle = "XMonad recompilation terminal"
