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
import Graphics.X11.Xlib.Event
import Data.Monoid
import Data.IORef
import System.Exit
import System.IO

--TODO:  XMonad.Util.NamedScratchpad

main = do
  fileRef <- openFile "xevents" WriteMode >>= newIORef
  file2Ref <- openFile "yevents" WriteMode >>= newIORef
  --loggerState <- newIORef (LoggerState M.empty M.empty M.empty M.empty (0, 0))
  xmonad $ withUrgencyHook NoUrgencyHook $ XConfig
    { XMonad.borderWidth        = 2
    , XMonad.workspaces         = map show [1..9 :: Int]
    , XMonad.terminal           = "gnome-terminal" --TODO: urxvt
    , XMonad.normalBorderColor  = "#000000"
    , XMonad.focusedBorderColor = "#ff0000"
    , XMonad.focusFollowsMouse  = True
    , XMonad.numlockMask        = mod2Mask
    , XMonad.modMask            = mod4Mask
    , XMonad.handleEventHook    = myEventHook fileRef file2Ref --loggerState
    , XMonad.logHook            = return ()
    , XMonad.startupHook        = myStartupHook
    , XMonad.layoutHook         = myLayoutHook
    , XMonad.manageHook         = myManageHook
    , XMonad.mouseBindings      = myMouseBindings
    , XMonad.keys               = myKeys
    }

phi = 0.61803
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
{-
myEventHook stateRef (MotionEvent {ev_x=x, ev_y=y, ev_window=w}) = do
    cname <- runQuery className w
    io $ modifyIORef stateRef (\ls -> ls {
        oldPos = (fromIntegral x, fromIntegral y),
        winMotion = M.insertWith (+) cname (mouseDist x y ls) $ winMotion ls })
    return (All True)
  where mouseDist x y (LoggerState {oldPos=(ox,oy)}) =
            sqrt $ (fromIntegral x - ox)^2 + (fromIntegral y - oy)^2
-}

myEventHook stateRef (KeyEvent {ev_keycode=k, ev_window=w}) = do
    cname <- runQuery className w
    io $ modifyIORef stateRef (\ls -> ls {
        keyHist = M.insertWith (+) k 1 $ keyHist ls,
        winHist = M.insertWith (+) cname 1 $ winHist ls })
    return (All True)
-}

myEventHook fileRef file2Ref ev = do
    dpy <- asks display
    withFocused (\win -> io $
        do  handle <- readIORef file2Ref
            pointer <- queryPointer dpy win
            hPutStrLn handle (show pointer))
    io $ do handle <- readIORef fileRef
            hPutStrLn handle (show ev)
    return (All True)

--myEventMask = substructureRedirectMask .|. substructureNotifyMask
--           .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
--           .|. buttonPressMask .|. pointerMotionMask

myStartupHook = do
    spawn "xmodmap .xmonad/.kbd && xset r 66"
    spawn "google-chrome"
    spawn "pidgin"
    spawn "ln -s .xmonad/.vimrc"
    layoutScreens 2 (TwoPane 0.5 0.5)

myLayoutHook =
        layoutHints (Tall 1 (3/100) phi)
    ||| layoutHints (ThreeCol 1 (3/100) phi)
    ||| noBorders Full

myManageHook = composeAll
                [ className =? "MPlayer"        --> doFloat
                , className =? "Gimp"           --> doFloat
                , className =? "Xmessage"       --> doCenterFloat
                , className =? "Zenity"         --> doCenterFloat
                , className =? "feh"            --> doCenterFloat
                , className =? "pidgin"         --> doShift "1"
                , className =? "skype"          --> doShift "1"
                ]

myMouseBindings (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
    , ((modm, button3), (\w -> focus w >> windows W.swapMaster))
    ]

searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google )
       , ((0, xK_y), method S.youtube )
       , ((0, xK_m), method S.maps )
       , ((0, xK_d), method S.dictionary )
       , ((0, xK_w), method S.wikipedia )
       , ((0, xK_h), method S.hoogle )
       , ((0, xK_i), method S.isohunt )
       ]

-- shell prompt theme
mySP = defaultXPConfig
    { bgColor           = "black"
    , fgColor           = "white"
    , bgHLight          = "gray"
    , fgHLight          = "black"
    , borderColor       = "orange"
    , promptBorderWidth = 1
    , position          = Bottom
    , height            = 20
    --, autoComplete      = Just 1000
    , historySize       = 1000 }


dmenu sudo = spawn $ "exe=`dmenu_path | dmenu` && eval \"" ++
    (if sudo then "gksu " else "") ++ "exec $exe\""

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((m4s,xK_Return), spawn $ XMonad.terminal conf)
    , ((m3, xK_Return), spawn $ XMonad.terminal conf)
    , ((m4, xK_Return), windows W.swapMaster)
    , ((m4, xK_space), sendMessage NextLayout)

    , ((m4s, xK_p), dmenu True)
    , ((m3, xK_p),  dmenu True)
    , ((m4, xK_p),  dmenu False)

    , ((m4s,xK_c), kill)
    , ((m4, xK_t), withFocused $ windows . W.sink)

    , ((m4, xK_k), windows W.focusDown)
    , ((m4, xK_j), windows W.focusUp)
    , ((m3, xK_k), windows W.swapDown)
    , ((m4s,xK_k), windows W.swapDown)
    , ((m3, xK_j), windows W.swapUp)
    , ((m4s,xK_j), windows W.swapUp)
    , ((m4, xK_l),         sendMessage Shrink)
    , ((m4, xK_semicolon), sendMessage Expand)

    , ((m4, xK_comma ), sendMessage (IncMasterN 1))
    , ((m4, xK_period), sendMessage (IncMasterN (-1)))

    , ((m4s, xK_q), io (exitWith ExitSuccess))
    , ((m4, xK_q), spawn "xmonad --recompile && xmonad --restart")

    --, ((m4, xK_g), gotoMenu)
    , ((m4, xK_n), appendFilePrompt defaultXPConfig "/home/mgsloan/pile")

    , ((0, xK_Print), unsafeSpawn "scrot -e 'mv $f ~/Pictures'")
    --TODO: video cap

    -- XMonad.Layout.Mosaic
    , ((m4, xK_a), sendMessage Taller)
    , ((m4, xK_z), sendMessage Wider)

    , ((m4, xK_u), warpToWindow (1/2) (1/2))

    , ((m4, xK_x), spawn "xlock")

    , ((m4, xK_s), SM.submap $ searchEngineMap $ promptSearch mySP)
    , ((m4, xK_m), SM.submap $ searchEngineMap $ selectSearch)
    ]
    ++
    [((m, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, m4), (W.shift, m4s), (W.shift, m3)]]
    ++
    [((m, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_i, xK_o] [0..]
        , (f, m) <- [(W.view, m4), (W.shift, m4s), (W.shift, m3)]]
  where m4 = mod4Mask
        m4s = mod4Mask .|. shiftMask
        m3 = mod3Mask
