{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Applicative
import           Control.Exception (bracket)
import           Control.Monad (when)
import           Data.Foldable (forM_)
import           Data.List (find, isPrefixOf, intercalate, isInfixOf)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, isNothing)
import           Data.Monoid (Endo)
import qualified Data.Set as S
import           Data.Time (getCurrentTime)
import           Debug.Trace (trace)
import           System.Environment (setEnv, unsetEnv)
import           System.Exit
import           System.IO (IOMode(..), openFile, hClose)
import           XMonad hiding (trace)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.FlexibleManipulate hiding (position)
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Warp
import           XMonad.Actions.WithAll
import           XMonad.Config.Gnome
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.TrackFloating
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import           XMonad.Util.Dzen hiding (x)
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as State
import           XMonad.Util.NamedScratchpad hiding (cmd)
import           XMonad.Util.Run

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

manageHooks :: Query (Endo WindowSet)
manageHooks
  = composeAll
  $ [ className =? "XClock"   --> doCenterFloat
    , className =? "xmessage" --> doCenterFloat
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
  , emacs "notes" "emacs-notes" ["~/notes.md"]
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
  emacs n t args = NS n
                      (unwords ("emacs -title" : t : args))
                      (title =? t)
                      flt
  flt = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)

openScratch :: String -> X ()
openScratch = namedScratchpadAction scratchpads

mouse :: [((KeyMask, Button), Window -> X ())]
mouse =
    [ ((mod4Mask, button1), mouseWindow discrete)
    , ((mod4Mask, button2), mouseWindow (const 0.5)) -- Position , ((mod4Mask, button3), mouseWindow (const 1)) -- Resize
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
  , ("M-r", spawn "sleep 0.2; scrot '~/user/Pictures/screenshots/%Y-%m-%d_$wx$h_scrot.png' -s -e 'eog $f'")
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
  , ("M-n", openScratch "notes")

  -- invert screen
  , ("M-w", spawn "xcalib -i -a")
  -- toggle redshift
  , ("M-S-w", cycleRedShift)
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

runGist :: String -> X ()
runGist filename = runProcessWithInput "gist" (words "-P -p -f" ++ [filename]) "" >>= \url -> spawn (browser ++ " " ++ url)

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
