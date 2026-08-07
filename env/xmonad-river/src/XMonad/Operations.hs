-- | Operations on the window set: the counterparts of xmonad's
-- @XMonad.Operations@.
module XMonad.Operations
  ( -- * Window set
    windows
  , refresh
  , withFocused
  , kill
  , killWindow
    -- * Messages
  , sendMessage
  , broadcastMessage
  , sendMessageWithNoRefresh
  , setLayout
    -- * Floating
  , float
  , mouseMoveWindow
  , mouseResizeWindow
    -- * Screens
  , screenWorkspace
    -- * Pointer
  , warpPointer
    -- * Lifecycle
  , restart
  , sendRestart
  , exitSession
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.IORef (readIORef, writeIORef)
import Data.Ratio ((%))
import qualified Data.Map.Strict as M

import XMonad.Core
import XMonad.River.Protocol.WindowManagement
import XMonad.River.X11Compat
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Window set

-- | Modify the window set and arrange for the result to be applied.
--
-- Unlike xmonad's version this does not itself run the layout. river only
-- permits window management state to change during a manage sequence, so
-- layout is run once at the end of the current sequence — after every action
-- has had its say. When called from outside a sequence (a timer, a @dbus@
-- callback) it asks river to start one.
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
  modify $ \st -> st { windowset = f (windowset st) }
  inSeq <- io . readIORef =<< asks inManageSeq
  unless inSeq requestManageSequence
  _ <- userCode =<< asks (logHook . config)
  pure ()

-- | Ask river to start a manage sequence, because state it cannot observe has
-- changed.
requestManageSequence :: X ()
requestManageSequence = do
  conn <- asks riverConn
  manager <- asks riverManager
  io (riverWindowManagerV1ManageDirty conn manager)

-- | Re-run the layout. Under river this is just a request for another manage
-- sequence; the layout runs there.
refresh :: X ()
refresh = windows id

withFocused :: (Window -> X ()) -> X ()
withFocused f = withWindowSet $ \ws -> whenJust (W.peek ws) f

-- | Close the focused window. Politely — this is @xdg_toplevel.close@, which
-- the client may ignore or prompt about.
kill :: X ()
kill = withFocused killWindow

killWindow :: Window -> X ()
killWindow w = do
  conn <- asks riverConn
  known <- io . readIORef =<< asks riverWindows
  when (M.member w known) $ io (riverWindowV1Close conn w)

--------------------------------------------------------------------------------
-- Messages

-- | Send a message to the current workspace's layout.
sendMessage :: Message a => a -> X ()
sendMessage a = do
  w <- W.workspace . W.current <$> gets windowset
  ml' <- handleMessage (W.layout w) (SomeMessage a) `catchX` pure Nothing
  whenJust ml' $ \l' -> do
    windows $ \ws -> ws
      { W.current = (W.current ws)
          { W.workspace = (W.workspace (W.current ws)) { W.layout = l' } } }

-- | Send a message to every workspace's layout.
broadcastMessage :: Message a => a -> X ()
broadcastMessage a = withWindowSet $ \ws -> do
  let c = W.workspace (W.current ws)
      v = map W.workspace (W.visible ws)
      h = W.hidden ws
  mapM_ (sendMessageWithNoRefresh a) (c : v ++ h)

-- | Send a message to one workspace's layout without triggering a refresh.
sendMessageWithNoRefresh :: Message a => a -> WindowSpace -> X ()
sendMessageWithNoRefresh a w =
  handleMessage (W.layout w) (SomeMessage a)
    `catchX` pure Nothing
    >>= \ml' -> whenJust ml' $ \l' ->
      modify $ \st -> st
        { windowset = updateLayoutOf (W.tag w) l' (windowset st) }

updateLayoutOf :: WorkspaceId -> Layout Window -> WindowSet -> WindowSet
updateLayoutOf i l = W.mapWorkspace $ \wsp ->
  if W.tag wsp == i then wsp { W.layout = l } else wsp

-- | Replace the current workspace's layout.
setLayout :: Layout Window -> X ()
setLayout l = do
  ss@W.StackSet { W.current = c@W.Screen { W.workspace = ws } } <- gets windowset
  _ <- handleMessage (W.layout ws) (SomeMessage ReleaseResources)
  windows $ const $
    ss { W.current = c { W.workspace = ws { W.layout = l } } }

--------------------------------------------------------------------------------
-- Floating

-- | Float a window, giving it the fraction of the screen it currently
-- occupies.
float :: Window -> X ()
float w = do
  known <- io . readIORef =<< asks riverWindows
  ws <- gets windowset
  let SD screen = W.screenDetail (W.current ws)
  forM_ (M.lookup w known) $ \rw -> do
    let (width, height) = rwDimensions rw
        sw = max 1 (fromIntegral (rect_width screen))
        sh = max 1 (fromIntegral (rect_height screen))
    windows $ W.float w $ W.RationalRect
      0 0 (fromIntegral width % sw) (fromIntegral height % sh)

-- | Interactive move by pointer drag.
--
-- Not implemented. river drives these through the seat's pointer operation
-- cycle (@op_start_pointer@, @op_delta@, @op_release@) rather than by the
-- window manager grabbing the pointer, and those events are not yet routed
-- into the 'X' monad.
mouseMoveWindow :: Window -> X ()
mouseMoveWindow _ = warnUnimplemented "mouseMoveWindow"
  "Dragging windows with the mouse will do nothing. Needs river's seat \
  \pointer operation cycle to be wired into XConf."

-- | Interactive resize by pointer drag. Not implemented; see
-- 'mouseMoveWindow'.
mouseResizeWindow :: Window -> X ()
mouseResizeWindow _ = warnUnimplemented "mouseResizeWindow"
  "Resizing windows with the mouse will do nothing. Needs river's seat \
  \pointer operation cycle to be wired into XConf." 

--------------------------------------------------------------------------------
-- Screens

-- | The workspace currently on the given screen, if that screen exists.
screenWorkspace :: ScreenId -> X (Maybe WorkspaceId)
screenWorkspace s = withWindowSet $ pure . W.lookupWorkspace s

--------------------------------------------------------------------------------
-- Pointer

-- | Move the pointer, in river's global coordinate space.
--
-- Wayland forbids clients from warping the pointer, but the window manager is
-- not an ordinary client: @river_seat_v1.pointer_warp@ exists precisely for
-- this. It is what makes this config's @warpMid@ work.
warpPointer :: Position -> Position -> X ()
warpPointer x y = do
  conn <- asks riverConn
  seats <- io . readIORef =<< asks riverSeats
  forM_ (M.keys seats) $ \seat -> io (riverSeatV1PointerWarp conn seat x y)

--------------------------------------------------------------------------------
-- Lifecycle

-- | Restart the window manager, replacing this process with @cmd@.
--
-- This is the river analogue of xmonad's @restart@, and the reason @M-q@
-- survives the move to Wayland. The compositor owns the windows, not the
-- window manager, so tearing down the window manager disturbs nothing: river
-- supports hot-swapping window managers without restarting itself or any
-- client.
--
-- The sequence is @stop@, wait for @finished@, then exec. Overlapping the two
-- window managers is not allowed — river answers the second connection with
-- @unavailable@ — so the handover has to be ordered this way.
restart :: String -> Bool -> X ()
restart cmd _resume = do
  conn <- asks riverConn
  manager <- asks riverManager
  ref <- asks riverRestart
  io (writeIORef ref (Just cmd))
  io (riverWindowManagerV1Stop conn manager)

-- | End the Wayland session, taking the compositor with it.
exitSession :: X ()
exitSession = do
  conn <- asks riverConn
  manager <- asks riverManager
  io (riverWindowManagerV1ExitSession conn manager)
