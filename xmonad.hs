-- Michael Sloan's XMonad Configuration
-- 
-- Notable Features:
--    * Use of VLC as a remote-controllable media player.
--        m4 m d dronezone radio
--        m4 m b bassdrive radio
--        m4 m g restart vlc, open grooveshark
--        m3 ,   previous song
--        m3 .   next song
--        m3 /   pause
--
--    * Uses mod3 as an alias for mod4-shift, in cases where the
--      action is non-destructive (black-listed).
--
--    * Flexible manipulate for mouse-window interaction.
--
--    * Usage of prompt for run-or-raise, search, and notes.
--
--    * Commented out half-complete system for logging events for
--    post-mortem analyis.


import XMonad
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowBringer
import XMonad.Actions.Warp
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.RunOrRaise
import XMonad.Util.Run

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutHints
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders

import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Types
import Data.Monoid
import Data.IORef
import System.Exit
import System.IO

--TODO:  XMonad.Util.NamedScratchpad

xmonadPath = "/home/mgsloan/.xmonad/xmonad-x86_64-linux"

---- VLC config / util
vlcPort = "5503"

-- | Start VLC in remote mode
startVlc = spawn "vlc -I rc --rc-host=localhost:" ++ vlcPort

-- | VLC remote command
vlcr x = runProcessWithInput "nc" ["localhost", vlcPort] (x ++ "\n")
     >>= liftIO . putStrLn

-- | Startup Hook
startup host = do
    spawn "xmodmap .xmonad/.kbd && xset r 66"
    spawn "xrdb -load .xmonad/.Xresources"
    spawn "google-chrome"
    spawn "pidgin"
    spawn "feh --bg-fill .xmonad/cassini.jpg"
    startVlc
    layoutScreens 2 (TwoPane 0.5 0.5)
--    spawn "xsetroot -cursor_name left_ptr"

main = do
{-  fileRef <- openFile "xevents" WriteMode >>= newIORef
  file2Ref <- openFile "yevents" WriteMode >>= newIORef
-}
  --loggerState <- newIORef (LoggerState M.empty M.empty M.empty M.empty (0, 0))
  host <- runProcessWithInput "uname" ["-n"] ""
  xmonad $ withUrgencyHook NoUrgencyHook $ XConfig
    { XMonad.borderWidth        = 1
    , XMonad.workspaces         = map show [1..9 :: Int]
    , XMonad.terminal           = "urxvt"
    , XMonad.normalBorderColor  = "#000000"
    , XMonad.focusedBorderColor = "#ff0000"
    , XMonad.focusFollowsMouse  = True
    , XMonad.modMask            = mod4Mask
    , XMonad.handleEventHook    = const $ return (All True) --myEventHook fileRef file2Ref --loggerState
    , XMonad.logHook            = return ()
    , XMonad.startupHook        = startup host
    , XMonad.layoutHook         = layout
    , XMonad.manageHook         = manage
    , XMonad.mouseBindings      = mouse
    , XMonad.keys               = myKeys host
    }


phi = 0.61803
layout = smartBorders . layoutHintsToCenter
             $ Full ||| Tall 1 (phi / 8) phi

manage = composeAll
    [ className =? "MPlayer"  --> doFloat
    , className =? "Gimp"     --> doFloat
    , className =? "guake"    --> doFloat
    , className =? "Xmessage" --> doCenterFloat
    , className =? "Zenity"   --> doCenterFloat
    , className =? "feh"      --> doCenterFloat
    , className =? "pidgin"   --> doShift "1"
    , className =? "skype"    --> doShift "1"
    ]

mouse (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
    , ((modm, button3), (\w -> focus w >> windows W.swapMaster))
    ]

searchEngineMap method = M.fromList $
    [ ((0, xK_s), method S.google )
    , ((0, xK_y), method S.youtube )
    , ((0, xK_m), method S.maps )
    , ((0, xK_d), method S.dictionary )
    , ((0, xK_w), method S.wikipedia )
    , ((0, xK_h), method S.hoogle )
    , ((0, xK_i), method S.isohunt )
    ]


musicMap = M.fromList $
    [ ((0, xK_b), vldl "BassDrive.pls")
    , ((0, xK_d), vldl "dronezone.pls")
    , ((0, xK_g), spawn "killall vlc"
               >> startVlc
               >> spawn "google-chrome www.grooveshark.com")
    ]
  where vldl = vlcr . (("add .xmonad/")++)

-- shell prompt theme
mySP auto | auto = res { autoComplete = Just 1000 }
          | otherwise = res where
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

warpMid = withFocused $ \win -> do
    name <- runQuery appName win
    warpToWindow (1/2) (1/2)
    {-
    case name of 
      "urxvt" -> warpToWindow (1/2) (15/16)
      "firefox" -> warpToWindow (1/2) (1/2)
      "gvim" -> warpToWindow (1/2) (1/2)
      _ -> warpToWindow (1/2) (1/16)
    -}

mapM3 x@((a, b), c) | a == mod3Mask = ((mod4Mask .|. shiftMask, b), c)
                    | otherwise     = x

myKeys comp conf@(XConfig {XMonad.modMask = modMask}) =
    foldr ($) keymap $ map M.delete
    [ (m3, xK_q), (m3, xK_c) ] where
  m3 = mod3Mask
  m4 = mod4Mask
  keymap = M.fromList $ map mapM3
    [ ((m3, xK_Return), spawn $ XMonad.terminal conf)
    , ((m4, xK_Return), windows W.swapMaster)
    , ((m4, xK_space), sendMessage NextLayout)

    , ((m4, xK_p), runOrRaisePrompt $ mySP False)

    , ((m4, xK_f), runInTerm "" "google-chrome")

    -- Music controls
    , ((m3, xK_comma),  vlcr "prev")
    , ((m3, xK_period), vlcr "next")
    , ((m3, xK_slash),  vlcr "pause")
    , ((m4, xK_m), SM.submap musicMap)

    , ((m3, xK_c), kill)
    , ((m4, xK_t), withFocused $ windows . W.sink)

    , ((m4, xK_k), windows W.focusDown >> warpMid)
    , ((m4, xK_j), windows W.focusUp   >> warpMid)
    , ((m3, xK_k), windows W.swapDown  >> warpMid)
    , ((m3, xK_j), windows W.swapUp    >> warpMid)
    , ((m4, xK_l),         sendMessage Shrink)
    , ((m4, xK_semicolon), sendMessage Expand)

    , ((m4, xK_comma ), sendMessage (IncMasterN 1))
    , ((m4, xK_period), sendMessage (IncMasterN (-1)))

    , ((m3, xK_q), io (exitWith ExitSuccess))
    , ((m4, xK_q), spawn $ xmonadPath ++ " --recompile && "
                        ++ xmonadPath ++ " --restart")
 
    --, ((m4, xK_g), gotoMenu)
    , ((m4, xK_n), appendFilePrompt (mySP False) "/home/mgsloan/pile")

    , ((0,  xK_Print), unsafeSpawn "scrot -e 'mv $f ~/pics'")
    , ((m4, xK_Print), spawn "xvidcap")
    --TODO: video cap

    -- XMonad.Layout.Mosaic
    , ((m4, xK_a), sendMessage Taller)
    , ((m4, xK_z), sendMessage Wider)

    , ((m3, xK_x), spawn "xlock")

    , ((m3 .|. controlMask, xK_space), layoutScreens 2 (TwoPane 0.5 0.5))
    , ((m3, xK_space), rescreen)

    --TODO: pop up bar giving options for subselection
    , ((m4, xK_g), SM.submap $ searchEngineMap $ promptSearch $ mySP False)
    , ((m4, xK_s), SM.submap $ searchEngineMap $ selectSearch)
    ]
    ++
    [((m, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, m4), (W.shift, m3)]]
    ++
    [((m, key), screenWorkspace sc >>= flip whenJust (windows . f) >> warpMid)
        | (key, sc) <- zip [xK_i, xK_o] [0..]
        , (f, m) <- [(W.view, m4), (W.shift, m3)]]

{-
data LoggerState = LoggerState {
    keyHist :: (M.Map KeyCode Int),
    winHist :: (M.Map String Int),
    winMotion :: (M.Map String Double),
    winClicks :: (M.Map String [(Button, Double, Double)]),
    oldPos :: (Int, Int) }

myEventHook stateRef (ButtonEvent {ev_x=x, ev_y=y, ev_window=w, ev_event_type=t}) | t == buttonPress = do
    cname <- runQuery className w
    io $ modifyIORef stateRef (\ls -> ls {
        winClicks = M.insertWith (++) cname [] $ winClicks ls })
    return (All True)

myEventHook stateRef (MotionEvent {ev_x=x, ev_y=y, ev_window=w}) = do
    cname <- runQuery className w
    io $ modifyIORef stateRef (\ls -> ls {
        oldPos = (fromIntegral x, fromIntegral y),
        winMotion = M.insertWith (+) cname (mouseDist x y ls) $ winMotion ls })
    return (All True)
  where mouseDist x y (LoggerState {oldPos=(ox,oy)}) =
            sqrt $ (fromIntegral x - ox)^2 + (fromIntegral y - oy)^2

myEventHook stateRef (KeyEvent {ev_keycode=k, ev_window=w}) = do
    cname <- runQuery className w
    io $ modifyIORef stateRef (\ls -> ls {
        keyHist = M.insertWith (+) k 1 $ keyHist ls,
        winHist = M.insertWith (+) cname 1 $ winHist ls })
    return (All True)

myEventHook fileRef file2Ref ev = do
    dpy <- asks display
    withFocused (\win -> io $
        do  handle <- readIORef file2Ref
            pointer <- queryPointer dpy win
            hPutStrLn handle (show pointer))
    io $ do handle <- readIORef fileRef
            hPutStrLn handle (show ev)
    return (All True)
-}

--myEventMask = substructureRedirectMask .|. substructureNotifyMask
--           .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
--           .|. buttonPressMask .|. pointerMotionMask
