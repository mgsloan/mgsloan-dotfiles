{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

-- Based on Brent Yorgey's configuration (maintaining import numbering):
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey%27s_darcs_xmonad.hs

import           Control.Applicative
import           Control.Exception.Extensible (bracket)
import           Control.Monad (when)
import           Data.List (find, isPrefixOf, intercalate)
import qualified Data.Map as M -- (0b) map creation
import           Data.Maybe (catMaybes, isNothing)
import           Data.Monoid (Endo)
import qualified Data.Set as S
import           Data.Time (getCurrentTime)
import           System.Exit
import           System.IO (IOMode(..), openFile, hClose)
import           System.Process (readProcess)
import           System.Environment (setEnv, unsetEnv, lookupEnv)
import           XMonad -- (0) core xmonad libraries
import           XMonad.Actions.CycleWS -- (16) general workspace-switching goodness
import           XMonad.Actions.DwmPromote -- swaps focused window with the master window
import           XMonad.Actions.FlexibleManipulate hiding (position) -- allows windows to be resized and moved with a single mouse click
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.SpawnOn -- (22a) start programs on a particular WS
import           XMonad.Actions.Warp -- (18) warp the mouse pointer
import           XMonad.Actions.WithAll -- (22) do something with all windows on a workspace
import           XMonad.Config.Gnome
import           XMonad.Hooks.ManageHelpers -- (4)  for doCenterFloat, put floating windows in the middle of the screen
import           XMonad.Layout.TrackFloating
import           XMonad.Prompt -- (23) general prompt stuff.
import           XMonad.Prompt.RunOrRaise
import qualified XMonad.StackSet as W -- (0a) window stack manipulation
import           XMonad.Util.Dzen hiding (x)
import           XMonad.Util.EZConfig -- (29) "M-C-x" style keybindings
import qualified XMonad.Util.ExtensibleState as State
import           XMonad.Util.NamedScratchpad hiding (cmd) -- (30) 'scratchpad' terminal
import           XMonad.Util.Run -- (31)

main :: IO ()
main = do
  malreadySet <- lookupEnv "GHC_PACKAGE_PATH"
  when (isNothing malreadySet) $ do
    pkgPath <- ghcPkgPath
    putStrLn pkgPath
    setEnv "GHC_PACKAGE_PATH" pkgPath
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

urxvt, browser :: String
urxvt = "urxvt -fg lightgrey -bg black +sb"
browser = "chromium-browser --disable-hang-monitor"

workspaceNames :: [String]
workspaceNames = map show $ [1..9 :: Int] ++ [0]

phi :: Rational
phi = 0.61803

warpMid :: X () -> X ()
warpMid = (>> warpToWindow (1/2) (1 - phi))

-- | Startup Hook
startup :: X ()
startup = do
  liftIO $ unsetEnv "GHC_PACKAGE_PATH"
  cycleTouch
  spawnOnce "cd"
  spawnOnce "xmodmap .xmonad/.kbd"
  spawnOnce "xrdb -load .xmonad/.Xresources"
  initSittingStanding
  -- spawnOnce "redshift -t 6100:6100 -g 0.8:0.8:0.8"
  spawnOnce "notify-listener.py"
  -- Set mouse acceleration to 4x with no threshold
  spawnOnce "xset m 4/1 0"
  spawnOnce "xinput set-button-map \"CSR8510 A10\" 2 3 1 4 5 6 7"
  spawnOnce "imwheel"
  spawnOnceOn "3" browser
  restartVlc

manageHooks :: Query (Endo WindowSet)
manageHooks
  = composeAll
  $ [ className =? "XClock"   --> doCenterFloat
    , className =? "XMessage" --> doCenterFloat
    , appName =? "eog" --> doCenterFloat
    , namedScratchpadManageHook scratchpads
    , resource =? "gnome-panel" --> doShift "0"
    , resource =? "desktop_window" --> doShift "0"
    , manageSpawn
--    , resource >>= io . appendFile "/home/mgsloan/xmonad_debug" . (++"\n") >> idHook
    ]

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"    (urxvt ++ " -title term")      (title =? "term")         flt
  , NS "sound"   "unity-control-center sound"   (title =? "Sound")        flt
  , NS "display" "unity-control-center display" (title =? "Displays")     flt
  , NS "ghci"    (urxvt ++ " -e ghci")          (title =? "ghci")         flt
  , NS "htop"    (urxvt ++ " -e htop")          (title =? "htop")         flt
  , NS "hamster" "hamster-time-tracker"         (title =? "Time Tracker") flt
  ]
 where
  flt = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)

openScratch :: String -> X ()
openScratch = namedScratchpadAction scratchpads             -- (30)

mouse :: [((KeyMask, Button), Window -> X ())]
mouse =
    [ ((mod4Mask, button1), mouseWindow discrete)
    , ((mod4Mask, button2), mouseWindow (const 0.5)) -- Position
    , ((mod4Mask, button3), mouseWindow (const 1)) -- Resize
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
  -- mod-{i,o}, switch to physical/Xinerama screens 1 and 2
  -- mod-shift-{i,o}, move window to screen 1 and 2
--  [ ((m ++ "M-" ++ key), warpMid $ screenWorkspace sc >>= flip whenJust (windows . f))
--  | (key, sc) <- zip ["o", "i"] [0..]
--  , (f, m) <- [(W.view, ""), (W.shift, "S-")]
--  ] ++
  [
  -- Bindings from the default XMonad configuration
    ("M-S-c", kill)
  , ("M-S-q", io exitSuccess) -- %! Quit xmonad
  , ("M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
  , ("M-S-<Return>", spawn urxvt)
  , ("M-<Space>", warpMid $ sendMessage NextLayout)

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

  , ("M-/", spawnOn "2" urxvt)

  -- Focus / switch master
  , ("M-h",   warpMid $ windows W.focusMaster)
  , ("M-S-h", warpMid dwmpromote)

  , (("M-,"), warpMid . sendMessage $ IncMasterN 1)
  , (("M-."), warpMid . sendMessage $ IncMasterN (-1))

  -- Sink floating windows
  , ("M-t", withFocused $ windows . W.sink) -- from default
  , ("M-S-t", sinkAll)

  -- Change size of master region
  , ("M-l", sendMessage Shrink)
  , ("M-;", sendMessage Expand)

  -- Start programs or navigate to them
  , ("M-p", runOrRaisePrompt $ xpconfig False)

  -- Activate or deactivate touchpad (I use the thinkpad nub)
  , ("M-b", cycleTouch)

  -- Either take a screen snip and view it, or full screen snapshot.
  -- http://code.google.com/p/xmonad/issues/detail?id=476
  , ("M-r", spawn "sleep 0.2; scrot -s -e 'eog $f'")
  , ("M-S-r", byzanzPrompt (xpconfig False))

  -- Clipboard gists
  -- https://github.com/defunkt/gist
  , ("M-g", runProcessWithInput "gist" (words "-P -p -f paste.hs") "" >>= \url -> spawn (browser ++ " " ++ url))

  , ("M-c", spawn browser)

-- TODO: utility to remember paste buffers / middle click

  , ("M-a M-a", openScratch "term")
  , ("M-a M-s", openScratch "sound")
  , ("M-a M-d", openScratch "display")
  , ("M-a M-h", openScratch "htop")
  , ("M-a M-g", openScratch "ghci")

  , ("M-n", contextAppend (xpconfig False) "notes")
  , ("M-e", contextEmail (xpconfig False) "mgsloan@gmail.com")
  , ("M-f M-f", contextHamster)
  , ("M-f M-r", hamster "start")
  , ("M-f M-d", hamster "stop")
  , ("M-f M-t", openScratch "hamster")

  , ("M-v", spawn "xdotool type --delay 2 \"$(xsel)\"")

-- VLC music
--, ("M-,", music "prev")
--, ("M-.", music "next")
  , ("M-m b", music "add ~/.xmonad/BassDrive.pls")
  , ("M-m d", music "add ~/.xmonad/dronezone.pls")
  , ("M-m p", music "pause")

  -- Reapply keybindings (TODO: figure out why this is necessary)
  , ("M-y", spawn "xmodmap .xmonad/.kbd")

  -- cycle workspaces in most-recently-used order                (17)
  -- , ("M-S-<Tab>", cycleRecentWS [xK_Super_L, xK_Shift_L] xK_Tab xK_grave)

  -- invert screen
  , ("M-w", spawn "xcalib -i -a")
  -- toggle redshift
  , ("M-S-w", cycleRedShift)
  -- toggle sitting / standing
  , ("M-S-s", cycleSittingStanding)
  ]

xpconfig :: Bool -> XPConfig
xpconfig auto
    | auto = res { autoComplete = Just 1000 }
    | otherwise = res
  where
    res = defaultXPConfig
        { bgColor           = "black"
        , fgColor           = "white"
        , bgHLight          = "gray"
        , fgHLight          = "black"
        , borderColor       = "orange"
        , promptBorderWidth = 1
        , position          = Bottom
        , height            = 20
        , historySize       = 1000 }

ghcPkgPath :: MonadIO m => m String
ghcPkgPath = do
  let stackYaml = "/home/mgsloan/oss/xmonad/stack.yaml"
      stackPath p = fmap lines $ runProcessWithInput
        "stack"
        ["--stack-yaml", stackYaml, "path", p]
        ""
  (local:_) <- stackPath "--local-pkg-db"
  (snapshot:_) <- stackPath "--snapshot-pkg-db"
  let global = "/usr/local/lib/ghc-7.10.2/package.conf.d"
  return $ local ++ ":" ++ snapshot ++ ":" ++ global

data ByzanzPrompt = ByzanzPrompt

instance XPrompt ByzanzPrompt where
  showXPrompt ByzanzPrompt = "Byzanz arguments: "

byzanzPrompt :: XPConfig -> X ()
byzanzPrompt c = mkXPrompt ByzanzPrompt c (const $ return []) $ \args ->
  let args' = if null args
                 then "10"
                 else args
  in spawn $ "~/.xmonad/byzanz-record-region.sh " ++ args' ++ " /tmp/recorded.gif; " ++ browser ++ " /tmp/recorded.gif"

-- Contextualized notes and timers

data ContextPrompt = ContextPrompt String

instance XPrompt ContextPrompt where
  showXPrompt (ContextPrompt txt) = txt

contextAppend :: XPConfig -> String -> X ()
contextAppend c fn
  = mkXPrompt (ContextPrompt $ "Add to " ++ fn ++ ": ") c (const $ return [])
  $ \msg -> getContext Nothing >>= doAppend fn . show . (msg,)

 -- http://www.klenwell.com/is/UbuntuCommandLineGmail
contextEmail :: XPConfig -> String -> X ()
contextEmail c addr
  = mkXPrompt (ContextPrompt $ "Email to " ++ addr ++ ": ") c (const $ return [])
  $ \msg -> getContext Nothing >>= sendEmail addr msg . show

getContext :: Maybe Int -> X [(String, String)]
getContext mtimerId = withWindowSet $ \ws -> do
  focusContext <- case W.peek ws of
    Nothing -> return []
    Just w -> do
      winTitle <- runQuery title w
      winAppName <- runQuery appName w
      return [("title", winTitle), ("appName", winAppName)]
  timestamp <- io $ getCurrentTime
  let timeContext = catMaybes
        [("timerId",) . show <$> mtimerId, Just ("timestamp", show timestamp)]
  return $ focusContext ++ timeContext

-- | Append a string to a file.
--   From XMonad.Prompt.AppendFile
doAppend :: FilePath -> String -> X ()
doAppend fn = io . bracket (openFile fn AppendMode) hClose . flip hPutStrLn

sendEmail :: String -> String -> String -> X ()
sendEmail addr subject body
  = void $ runProcessWithInput "mail" ["-s", subject, addr] body

contextHamster :: X ()
contextHamster =
  withWindowSet $ \ws -> case W.peek ws of
    Nothing -> myDzen "No window selected"
    Just w -> do
      t <- runQuery title w
      safeSpawn "hamster-cli" ["start", t]
      myDzen $ "Started task " ++ t

myDzen :: String -> X ()
myDzen = dzenConfig (timeout 5 >=> onCurr xScreen)

data HamsterPrompt = HamsterPrompt String

instance XPrompt HamsterPrompt where
  showXPrompt (HamsterPrompt cmd) = cmd

hamster :: String -> X ()
hamster subcmd = do
    activities <- fmap lines $ runProcessWithInput "hamster-cli" ["list-activities"] ""
    mkXPrompt (HamsterPrompt cmd) (xpconfig False) (mkComplFunFromList' activities) (spawn . (cmd ++))
  where
    cmd = "hamster-cli " ++ subcmd ++ " "

-- VLC Extension?

data VLCState = VLCState Bool
  deriving Typeable

instance ExtensionClass VLCState where
    initialValue = VLCState False

---- VLC config / util

vlcPort1 :: String
vlcPort1 = "6543"
-- vlcPort2 = "6544"

-- | Start VLC in remote mode
restartVlc :: X ()
restartVlc = unsafeSpawn $ unlines
  [ "killall vlc;"
  , "sleep 0.2;"
  , "vlc -I rc --rc-host=localhost:" ++ vlcPort1 ++ " &"
--  , "vlc -I rc --rc-host=localhost:" ++ vlcPort2 ++ " &"
  ]

-- | VLC remote command
music :: String -> X ()
music x = runProcessWithInput "nc" ["localhost", vlcPort1] (x ++ "\n")
       >>= liftIO . putStrLn

void :: Monad m => m a -> m ()
void f = f >> return ()

{- Attempt to pause one player when the other starts.

runVlc which x = runProcessWithInput "nc" ["localhost", port] (x ++ "\n")
  where
    port = if which then vlcPort1 else vlcPort2

switchMusic file = do
  VLCState current <- State.get
  State.put . VLCState $ not current
  runVlc (not current) $ "add " ++ file
  void . io . forkIO . void . sequence . replicate 10 $ do
    response <- runVlc (not current) "get_time"
    let secs :: Maybe Int
        secs = readMay =<< return . drop 2 =<< find ("> " `isPrefixOf`) (lines response)
    print response
    print secs
    let pause = runVlc current "pause" >> exitSuccess
    case secs of
      Just x -> if x <= 0 then return () else pause
      Nothing -> pause
    -- Wait half a second
    threadDelay 500000

-}

-- Smart promote

-- data SmartPromote = SmartPromote

{-
smartPromote :: X ()
smartPromote = windows $ modify' $
  \c -> case c of
    Stack _ [] []     -> c
    Stack t [] (x:rs) -> c
-}

-- Cycle Screens

-- cycleScreens :: X ()
-- cycleScreens =

-- TOOD: switch other screen without stealing this one.

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

-- Enable / disable thinkpad nub

data NubMode = NubEnabled | NubDisabled
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass NubMode where
  initialValue = NubEnabled
  extensionType = PersistentExtension

cycleNub :: X ()
cycleNub = do
  x <- State.get
  let x' = nxt x
  setNub x'
  State.put x'

setNub :: NubMode -> X ()
setNub mode = spawn $ "xinput set-prop \"TPPS/2 IBM TrackPoint\" \"Device Enabled\"" ++
  case mode of
    NubEnabled -> "1"
    NubDisabled -> "0"

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

-- Switch between sitting and standing

data SittingStanding = Sitting | Standing
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass SittingStanding where
  initialValue = Sitting
  extensionType = PersistentExtension

cycleSittingStanding :: X ()
cycleSittingStanding = do
  x <- State.get
  let x' = nxt x
  updateSittingStanding x'
  State.put x'

initSittingStanding :: X ()
initSittingStanding = do
  x <- State.get
  updateSittingStanding x

updateSittingStanding :: SittingStanding -> X ()
updateSittingStanding Standing = do
  dpn <- getDPName
  spawn $ intercalate " & "
    [ "xrandr --output VGA-1 --off"
    , "xrandr --output " ++ dpn ++ " --off"
    , "xrandr --output " ++ dpn ++ " --auto"
    , "xrandr --output " ++ dpn ++ " --above LVDS-1"
    , "sleep 1"
    , "feh --bg-fill .xmonad/cassini.jpg"
    ]
updateSittingStanding Sitting = do
  dpn <- getDPName
  spawn $ intercalate " & "
    [ "xrandr --output VGA-1 --off"
    , "xrandr --output " ++ dpn ++ " --off"
    , "xrandr --output VGA-1 --auto"
    , "xrandr --output VGA-1 --left-of LVDS-1"
    , "sleep 1"
    , "feh --bg-fill .xmonad/cassini.jpg"
    ]

getDPName :: X String
getDPName = io $ do
  output <- readProcess "xrandr" [] ""
  let dpLines = filter ("DP-" `isPrefixOf`) (lines output)
      dpConnected = find (("connected" ==) . (!! 1) . words) dpLines
  return $ maybe "DP-3" (head . words) dpConnected
