{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Exception (catch, IOException, throwIO)
import           Control.Monad (void)
import           Data.Char (isSpace)
import           Data.List (intercalate, isInfixOf)
import           Data.IORef
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Debug.Trace (trace)
-- import           Network.Hoggl
-- import           Network.Hoggl.Types hiding (WorkspaceId)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Servant.Client
import           System.Exit
import           System.Process (rawSystem)
import           System.IO.Unsafe (unsafePerformIO)
import           XMonad hiding (trace)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.FlexibleManipulate hiding (position)
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.SimpleDate (date)
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Warp
import           XMonad.Actions.WithAll
import           XMonad.Config.Gnome
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.TrackFloating
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as State
import           XMonad.Util.NamedScratchpad hiding (cmd)
import           XMonad.Util.Run
import           XMonad.Util.SessionStart (isSessionStart, setSessionStarted)

-- TODO:
-- * Utility to remember paste buffers / middle click
-- * Automatic journal starter with date
-- * Spotify integration

main :: IO ()
main = do
  xmonad $ gnomeConfig
    { borderWidth   = 0 -- Focus indicated and determined by mouse.
    , modMask       = mod4Mask
    , terminal      = urxvt
    , workspaces    = workspaceNames
    , startupHook   = startup
    , layoutHook    = trackFloating $ Tall 1 (phi / 8) phi ||| Full
    , manageHook    = manageHooks
    -- No default key or mouse bindings
    , keys          = const M.empty
    , mouseBindings = const M.empty
    }
    `additionalMouseBindings` mouse
    `additionalKeysP` keymap

urxvtArgs :: [String]
urxvtArgs = ["-fg", "lightgrey", "-bg", "black", "+sb"]

urxvt, browser, emacs :: String
urxvt = unwords ("urxvt" : urxvtArgs)
browser = "chromium-browser"
emacs = "emacs"

workspaceNames :: [String]
workspaceNames = map show $ [1..9 :: Int] ++ [0]

phi :: Rational
phi = 0.61803

warpMid :: X () -> X ()
warpMid = (>> warpToWindow (1/2) (1 - phi))

-- | Startup Hook
startup :: X ()
startup = do
  isRestart <- not <$> isSessionStart
  if isRestart
    then notify "XMonad" "Restarted"
    else notify "XMonad" "started"
  setTouch Inactive
  -- Set mouse acceleration to 4x with no threshold
  spawnOnce "xset m 4/1 0"
  spawnOnceOn "1" emacs
  spawnOnceOn "1" browser
  spawnOnceOn "1" (urxvt ++ " -e irssi")
  spawnOnceOn "2" emacs
  spawnOnceOn "2" "firefox"
  spawnOnceOn "9" "spotify"
  spawnOnceOn "0" "stalonetray"
  -- setSessionStarted

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
      args' = ["urxvt"] ++ urxvtArgs ++ args
  controlCenter n t = NS n
                         ("unity-control-center " ++ n)
                         (title =? t <&&> stringProperty "_GTK_APPLICATION_ID" =? "org.gnome.ControlCenter")
                         flt
  emacsOpen n t args = NS n
                          (unwords ("emacs -title" : t : args))
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
  , ("M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  , ("M-S-<Return>", spawn urxvt)
  , ("M-<Space>", warpMid $ sendMessage NextLayout)

   -- Focus / switch windows between screens
  , ("M-i", warpMid $ viewScreen $ P 0)
  , ("M-o", warpMid $ viewScreen $ P 1)
  , ("M-u", warpMid $ viewScreen $ P 2)
  , ("M-S-i", warpMid $ sendToScreen $ P 0)
  , ("M-S-o", warpMid $ sendToScreen $ P 1)
  , ("M-S-u", warpMid $ sendToScreen $ P 2)

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
  , ("M-r", spawn "sleep 0.2; scrot '/home/mgsloan/user/Pictures/screenshots/%Y-%m-%d_$wx$h_scrot.png' -s -e 'eog $f'")
  , ("M-S-r", byzanzPrompt (xpconfig False))

  -- Clipboard gists via https://github.com/defunkt/gist
  , ("M-g M-h", runGist "paste.hs")
  , ("M-g M-m", runGist "paste.md")
  , ("M-g M-p", runGist "paste.txt")

  -- Start common programs with one key-press
  , ("M-c", spawn browser)
  , ("M-e", spawn "emacs")
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

  -- Hotkeys for common screen layouts
  , ("M-d M-l", lvdsauto >> restartKeynav)
  , ("M-d M-d", dpabove >> restartKeynav)
  , ("M-d M-v", vgaleft >> restartKeynav)

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
  mkXPrompt (GenericPrompt "(1) Test\n(2) Test\nWhat are you working on for Alphasheets? ") (xpconfig False) (const $ return []) $ \msg -> do
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


--------------------------------------------------------------------------------
-- Prompt for running byzanz

data ByzanzPrompt = ByzanzPrompt

instance XPrompt ByzanzPrompt where
  showXPrompt ByzanzPrompt = "Byzanz arguments: "

byzanzPrompt :: XPConfig -> X ()
byzanzPrompt c = mkXPrompt ByzanzPrompt c (const $ return []) $ \args ->
  let args' = if null args
                 then "10"
                 else args
  in spawn $ "~/.xmonad/byzanz-record-region.sh " ++ args' ++ " /tmp/recorded.gif; " ++ browser ++ " /tmp/recorded.gif"

-- Could be made into an extension.  Generalized from "XMonad.Util.SpawnOnce"

data DoOnce = DoOnce { unDoOnce :: S.Set String }
    deriving (Read, Show, Typeable)

instance ExtensionClass DoOnce where
    initialValue = DoOnce S.empty
    extensionType = PersistentExtension

-- | The first time 'spawnOnce' is executed on a particular command, that
-- command is executed.  Subsequent invocations for a command do nothing.
doOnce :: String -> X () -> X ()
doOnce k action = do
    whenX (not <$> State.gets (S.member k . unDoOnce)) $ do
        action
        State.modify (DoOnce . S.insert k . unDoOnce)

spawnOnce :: String -> X ()
spawnOnce cmd = doOnce (cmd ++ " # spawnOnce") $ spawn cmd

-- This command was the reason I needed to generalize the SpawnOnce module.
spawnOnceOn :: WorkspaceId -> String -> X ()
spawnOnceOn wid cmd = doOnce (cmd ++ " # spawnOnceOn") $ spawnOn wid cmd

--------------------------------------------------------------------------------
-- Could be made into an extension.  Probably not very useful.

data TouchMode = Inactive {- | Scroll -} | Normal
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass TouchMode where
  initialValue = Inactive
  extensionType = PersistentExtension

nxt :: (Eq a, Enum a, Bounded a) => a -> a
nxt x | x == maxBound = minBound
      | otherwise = succ x

cycleTouch :: X ()
cycleTouch = do
  x <- State.get
  let x' = nxt x
  setTouch x'
  State.put x'

setTouch :: TouchMode -> X ()
setTouch Inactive = spawn "synclient TouchpadOff=1"
--TODO: fix this one.
--setTouch Scroll = spawn $ unwords
--  [ "synclient TouchpadOff=2 HorizTwoFingerScroll=1"
--  , "TapButton1=0 TapButton2=0 ClickFinger1=0 ClickFinger2=0"
--  , "LeftEdge=0 RightEdge=1 TopEdge=0 BottomEdge=1"
--  ]
setTouch Normal = spawn $ unwords
  [ "synclient TouchpadOff=2 HorizTwoFingerScroll=0"
  , "TapButton1=1 TapButton2=3 ClickFinger1=1 ClickFinger2=1"
  , "LeftEdge=1751 RightEdge=5191 TopEdge=1624 BottomEdge=4282"
  ]

--------------------------------------------------------------------------------
-- Redshift extension?

data RedShift = RedShiftEnabled | RedShiftDisabled
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass RedShift where
  initialValue = RedShiftEnabled
  extensionType = PersistentExtension

cycleRedShift :: X ()
cycleRedShift = do
  x <- State.get
  let x' = nxt x
  updateRedShift x'
  State.put x'

updateRedShift :: RedShift -> X ()
updateRedShift RedShiftEnabled = do
  -- TODO: make this configurable
  spawn "redshift -l 47:-122 -t 6500:3700 -r"
updateRedShift RedShiftDisabled = do
  spawn "killall redshift"

--------------------------------------------------------------------------------
-- Misc utilities

debug :: Show a => a -> a
debug x = trace ("xmonad debug: " ++ show x) x

{- TODO: figure out initial manage hook
runManageHookOnAll :: ManageHook -> X ()
runManageHookOnAll mh = void $ withWindowSet $ \s -> do
  mapM
    (\w -> runQuery mh w)
    (W.allWindows s)
-}

readToken :: FilePath -> X String
readToken = liftIO . fmap (takeWhile (not . isSpace)) . readFile

notify :: String -> String -> X ()
-- FIXME: This is broken. Mostly works, but assumes haskell string
-- escaping == bash string escaping
notify title msg = spawn $ "notify-send " ++ show title ++ " " ++ show msg
