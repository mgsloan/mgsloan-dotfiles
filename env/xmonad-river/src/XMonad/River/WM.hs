-- | The river window management state machine, driving an xmonad 'WindowSet'.
--
-- river splits state into two disjoint categories, and the split shapes this
-- module:
--
-- * __Window management state__ — dimensions, keyboard focus, keyboard
--   bindings — may only be modified during a /manage sequence/, between the
--   @manage_start@ event and the @manage_finish@ request.
-- * __Rendering state__ — position, stacking order, borders, hide\/show — may
--   be modified during either sequence, but is only applied at
--   @render_finish@.
--
-- So layout runs during the manage sequence (it must, to propose dimensions),
-- its results are stashed, and positions are applied during the following
-- render sequence.
--
-- Bindings fire outside any sequence. Their actions are queued and run at the
-- start of the next manage sequence, which river is asked to schedule with
-- @manage_dirty@. This is the same deferral river's own reference window
-- manager uses.
module XMonad.River.WM
  ( riverMain
  , applyLayout
  , queueAction
  ) where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.Bits ((.&.))
import Data.IORef
import Data.List (sortOn)
import Data.Monoid (All(..), appEndo)
import Data.Word (Word32)
import Control.Exception (catch)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure, exitSuccess)
import System.Posix.Process (executeFile)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.Core
import XMonad.River.Connection
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.LayerShell
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Wire (ObjectId, isNullObject)
import XMonad.River.X11Compat
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Mutable plumbing shared with the event callbacks

-- | State that the Wayland event callbacks write and the manage sequence
-- reads. Kept outside 'XState' because callbacks run in 'IO', below the 'X'
-- monad.
data Runtime = Runtime
  { rtPending     :: !(IORef [X ()])
    -- ^ Binding actions awaiting the next manage sequence, newest first.
  , rtBindings    :: !(IORef (M.Map ObjectId (X ())))
  , rtPointerBind :: !(IORef (M.Map ObjectId (Window -> X ())))
  , rtPlacements  :: !(IORef [(Window, Rectangle)])
    -- ^ Layout output from the last manage sequence, applied during render.
  , rtVisible     :: !(IORef (S.Set Window))
  , rtBoundSeats  :: !(IORef (S.Set ObjectId))
  , rtHovered     :: !(IORef (Maybe Window))
  , rtManager     :: !ObjectId
    -- ^ Needed by binding callbacks, which run outside the 'X' monad and must
    -- request a manage sequence with @manage_dirty@.
  , rtStartupDone :: !(IORef Bool)
  , rtLayerShell  :: !(Maybe ObjectId)
    -- ^ The @river_layer_shell_v1@ global, when the compositor offers it.
    -- Binding it is what tells river to let clients map layer surfaces at all;
    -- without it river closes every one on sight.
  , rtLayerDefault :: !(IORef (Maybe ObjectId))
    -- ^ The output most recently nominated as the default for layer surfaces
    -- that do not name one. Held so the request is only reissued when the
    -- choice actually changes.
  }

--------------------------------------------------------------------------------
-- Entry point

-- | Connect to river and run the window manager. Does not return.
riverMain :: XConfig Layout -> IO ()
riverMain userConfig = do
  conn <- connect
  (registry, globals) <- getRegistry conn
  mManager <- bindGlobal conn registry globals
                riverWindowManagerV1Interface 4 riverWindowManagerV1Version
  mBindings <- bindGlobal conn registry globals
                 riverXkbBindingsV1Interface 1 riverXkbBindingsV1Version
  -- Optional, and river only advertises it alongside the window manager
  -- global. Binding it is not merely how we learn about exclusive zones: it is
  -- the signal that this window manager supports layer shell, without which
  -- river refuses to map layer surfaces. Everything that draws outside the
  -- window layout depends on it — fuzzel prompts, notification daemons,
  -- wallpaper setters, bars and lock screens.
  mLayerShell <- bindGlobal conn registry globals
                   riverLayerShellV1Interface 1 riverLayerShellV1Version
  case (mManager, mBindings) of
    (Just (manager, _), Just (bindings, _)) -> do
      when (mLayerShell == Nothing) $ hPutStrLn stderr
        "xmonad-river: river_layer_shell_v1 is unavailable; layer surfaces \
        \(fuzzel prompts, notifications, wallpaper, bars) will not be shown"
      run conn manager bindings (fmap fst mLayerShell) userConfig
    _ -> do
      hPutStrLn stderr
        "xmonad-river: river_window_manager_v1 (>= 4) or \
        \river_xkb_bindings_v1 not supported by the compositor"
      exitFailure

run :: Connection -> ObjectId -> ObjectId -> Maybe ObjectId -> XConfig Layout
    -> IO ()
run conn manager bindings layerShell userConfig = do
  windowsRef <- newIORef M.empty
  outputsRef <- newIORef M.empty
  seatsRef   <- newIORef M.empty
  dirtyRef   <- newIORef False
  manageRef  <- newIORef False
  restartRef <- newIORef Nothing
  rt <- Runtime
          <$> newIORef []
          <*> newIORef M.empty
          <*> newIORef M.empty
          <*> newIORef []
          <*> newIORef S.empty
          <*> newIORef S.empty
          <*> newIORef Nothing
          <*> pure manager
          <*> newIORef False
          <*> pure layerShell
          <*> newIORef Nothing

  -- Already existentially wrapped: XMonad.xmonad does that before calling
  -- here, so that every workspace can hold a different layout.
  let layout = layoutHook userConfig
      -- One workspace per name, and a single screen to begin with. Screens are
      -- reconciled against river's outputs at the start of every manage
      -- sequence, so the placeholder here is replaced before anything is laid
      -- out.
      initialWorkspaces =
        [ W.Workspace i layout Nothing | i <- workspaces userConfig ]
      placeholder = W.Screen (head initialWorkspaces) 0 (SD (Rectangle 0 0 0 0))
      -- The fourth field is the floating window map, empty at startup.
      initialSet = W.StackSet placeholder [] (drop 1 initialWorkspaces) M.empty

      xconf = XConf
        { config = userConfig
        , riverConn = conn
        , riverManager = manager
        , riverBindings = bindings
        , riverWindows = windowsRef
        , riverOutputs = outputsRef
        , riverSeats = seatsRef
        , riverDirty = dirtyRef
        , inManageSeq = manageRef
        , riverRestart = restartRef
        , normalBorder = parseColor (normalBorderColor userConfig)
        , focusedBorder = parseColor (focusedBorderColor userConfig)
        , keyActions = keys userConfig userConfig
        , buttonActions = mouseBindings userConfig userConfig
        , mouseFocused = False
        , mousePosition = Nothing
        , currentEvent = Nothing
        }
      xstate = XState
        { windowset = initialSet
        , mapped = S.empty
        , waitingUnmap = M.empty
        , dragging = Nothing
        , extensibleState = M.empty
        , numberlockMask = 0
        }

  stateRef <- newIORef xstate
  let runX' :: X a -> IO a
      runX' act = do
        st <- readIORef stateRef
        (a, st') <- runX xconf st act
        writeIORef stateRef st'
        pure a

  riverWindowManagerV1Listen conn manager $
    onManagerEvent conn manager restartRef rt runX'

  riverWindowManagerV1ManageDirty conn manager

  setMainThread
  -- The startup hook is deliberately *not* run here. river holds a watchdog
  -- over the manage sequence, and this config's startup hook spawns upwards of
  -- a dozen processes; doing that before the event loop starts means river
  -- waits on a manage_finish that cannot come, and logs
  -- "timeout occurred, some imperfect frames may be shown". It runs at the end
  -- of the first manage sequence instead, once manage_finish is already on its
  -- way. See 'runStartupHook'.
  let loop = dispatch conn >> loop
  -- A restart request arrives as an async exception thrown into this thread,
  -- which interrupts the blocking read. Ask river to release us, then keep
  -- dispatching: the 'finished' event does the exec.
  loop `catch` \RestartRequested -> do
    exe <- getExecutablePath
    args <- getArgs
    writeIORef restartRef (Just (unwords (map shellQuote (exe : args))))
    riverWindowManagerV1Stop conn manager
    loop

-- | Quote an argument for the @sh -c@ used to exec the successor.
shellQuote :: String -> String
shellQuote a = "'" ++ concatMap escape a ++ "'"
  where escape c = if c == '\'' then "'\\''" else [c]

--------------------------------------------------------------------------------
-- Manager events

onManagerEvent
  :: Connection -> ObjectId -> IORef (Maybe String) -> Runtime
  -> (forall a. X a -> IO a)
  -> RiverWindowManagerV1Event -> IO ()
onManagerEvent conn manager restartRef rt runX' = \case
  RiverWindowManagerV1Unavailable -> do
    hPutStrLn stderr "xmonad-river: another window manager is already running"
    exitFailure
  -- river has confirmed this window manager is no longer active, so a
  -- successor may now connect. Exec it in place, which is what makes M-q a
  -- restart rather than a logout.
  RiverWindowManagerV1Finished -> readIORef restartRef >>= \case
    Nothing  -> exitSuccess
    Just cmd -> executeFile "/bin/sh" False ["-c", cmd] Nothing
  RiverWindowManagerV1ManageStart -> do
    runX' (manageSequence rt)
    riverWindowManagerV1ManageFinish conn manager
    -- Deliver manage_finish before running anything slow. Requests are only
    -- buffered until the event loop flushes, and the startup hook spawns
    -- enough processes to trip river's watchdog in the meantime.
    flush conn
    runX' (runStartupHook rt)
  RiverWindowManagerV1RenderStart -> do
    runX' (renderSequence rt)
    riverWindowManagerV1RenderFinish conn manager
  RiverWindowManagerV1Window win -> runX' (addWindow win)
  RiverWindowManagerV1Output out -> runX' (addOutput rt out)
  RiverWindowManagerV1Seat seat  -> runX' (addSeat rt seat)
  RiverWindowManagerV1SessionLocked   -> void (runX' (broadcastEvent SessionLocked))
  RiverWindowManagerV1SessionUnlocked -> void (runX' (broadcastEvent SessionUnlocked))
  _ -> pure ()

broadcastEvent :: RiverEvent -> X All
broadcastEvent ev = do
  hook <- asks (handleEventHook . config)
  userCodeDef (All True) (hook ev)

--------------------------------------------------------------------------------
-- Object tracking

addWindow :: ObjectId -> X ()
addWindow win = do
  conn <- asks riverConn
  node <- io (riverWindowV1GetNode conn win)
  ref <- asks riverWindows
  io $ modifyIORef' ref $ M.insert win RiverWindow
    { rwObject = win, rwNode = node
    , rwAppId = Nothing, rwTitle = Nothing, rwPid = Nothing
    , rwIdentifier = Nothing, rwParent = Nothing
    , rwDimensions = (0, 0)
    , rwNew = True, rwClosed = False, rwHidden = False
    }
  io $ riverWindowV1Listen conn win $ \case
    RiverWindowV1Closed        -> adjust ref win $ \w -> w { rwClosed = True }
    RiverWindowV1AppId a       -> adjust ref win $ \w -> w { rwAppId = a }
    RiverWindowV1Title t       -> adjust ref win $ \w -> w { rwTitle = t }
    RiverWindowV1UnreliablePid p -> adjust ref win $ \w -> w { rwPid = Just p }
    RiverWindowV1Identifier i  -> adjust ref win $ \w -> w { rwIdentifier = Just i }
    RiverWindowV1Parent p      -> adjust ref win $ \w ->
      w { rwParent = if isNullObject p then Nothing else Just p }
    RiverWindowV1Dimensions width height ->
      adjust ref win $ \w -> w { rwDimensions = (width, height) }
    RiverWindowV1PointerMoveRequested _ -> pure ()
    _ -> pure ()

adjust :: IORef (M.Map ObjectId a) -> ObjectId -> (a -> a) -> IO ()
adjust ref k f = modifyIORef' ref (M.adjust f k)

addOutput :: Runtime -> ObjectId -> X ()
addOutput rt out = do
  conn <- asks riverConn
  ref <- asks riverOutputs

  mLayer <- forM (rtLayerShell rt) $ \shell -> io $ do
    lo <- riverLayerShellV1GetOutput conn shell out
    riverLayerShellOutputV1Listen conn lo $ \case
      RiverLayerShellOutputV1NonExclusiveArea x y width height ->
        adjust ref out $ \o -> o { roLayerArea = Just (Rectangle x y
          (fromIntegral width) (fromIntegral height)) }
      _ -> pure ()
    pure lo

  io $ modifyIORef' ref $ M.insert out RiverOutput
    { roObject = out, roPosition = (0, 0), roSize = (0, 0), roRemoved = False
    , roLayerObject = mLayer, roLayerArea = Nothing }
  io $ riverOutputV1Listen conn out $ \case
    RiverOutputV1Removed -> adjust ref out $ \o -> o { roRemoved = True }
    RiverOutputV1Position x y -> adjust ref out $ \o -> o { roPosition = (x, y) }
    RiverOutputV1Dimensions width height ->
      adjust ref out $ \o -> o { roSize = (width, height) }
    _ -> pure ()

addSeat :: Runtime -> ObjectId -> X ()
addSeat rt seat = do
  conn <- asks riverConn
  ref <- asks riverSeats

  mLayer <- forM (rtLayerShell rt) $ \shell -> io $ do
    ls <- riverLayerShellV1GetSeat conn shell seat
    riverLayerShellSeatV1Listen conn ls $ \ev -> do
      let set f = adjust ref seat $ \s -> s { rsLayerFocus = f }
      case ev of
        RiverLayerShellSeatV1FocusExclusive    -> set LayerFocusExclusive
        RiverLayerShellSeatV1FocusNonExclusive -> set LayerFocusNonExclusive
        RiverLayerShellSeatV1FocusNone         -> set LayerFocusNone
        _ -> pure ()
    pure ls

  io $ modifyIORef' ref $ M.insert seat RiverSeat
    { rsObject = seat, rsRemoved = False
    , rsLayerObject = mLayer, rsLayerFocus = LayerFocusNone }
  io $ riverSeatV1Listen conn seat $ \case
    RiverSeatV1Removed -> adjust ref seat $ \s -> s { rsRemoved = True }
    RiverSeatV1PointerEnter win -> writeIORef (rtHovered rt) (Just win)
    RiverSeatV1PointerLeave -> writeIORef (rtHovered rt) Nothing
    _ -> pure ()

--------------------------------------------------------------------------------
-- The manage sequence

manageSequence :: Runtime -> X ()
manageSequence rt = do
  asks inManageSeq >>= \r -> io (writeIORef r True)
  reapClosed
  syncScreens
  nominateLayerOutput rt
  createBindings rt
  adoptNewWindows
  runPending rt
  applyLayout rt
  asks inManageSeq >>= \r -> io (writeIORef r False)

-- | Drop windows river has told us are gone, and destroy the protocol objects.
reapClosed :: X ()
reapClosed = do
  conn <- asks riverConn
  ref <- asks riverWindows
  ws <- io (readIORef ref)
  let closed = [ w | w <- M.elems ws, rwClosed w ]
  forM_ closed $ \w -> do
    modify $ \st -> st { windowset = W.delete (rwObject w) (windowset st) }
    io $ do
      riverNodeV1Destroy conn (rwNode w)
      riverWindowV1Destroy conn (rwObject w)
      modifyIORef' ref (M.delete (rwObject w))

  outRef <- asks riverOutputs
  outs <- io (readIORef outRef)
  -- The layer shell objects are inert once removed is sent, but destroying
  -- them is still what completes destruction of the output.
  forM_ [ o | o <- M.elems outs, roRemoved o ] $ \o -> io $ do
    forM_ (roLayerObject o) (riverLayerShellOutputV1Destroy conn)
    riverOutputV1Destroy conn (roObject o)
    modifyIORef' outRef (M.delete (roObject o))

  seatRef <- asks riverSeats
  seats <- io (readIORef seatRef)
  forM_ [ s | s <- M.elems seats, rsRemoved s ] $ \s -> io $ do
    forM_ (rsLayerObject s) (riverLayerShellSeatV1Destroy conn)
    riverSeatV1Destroy conn (rsObject s)
    modifyIORef' seatRef (M.delete (rsObject s))

-- | Nominate an output for layer surfaces that do not pick one themselves.
--
-- Until this is done the default output is undefined, and a client like fuzzel
-- that names no output has nowhere to be placed. @set_default@ modifies window
-- management state, so it belongs in the manage sequence.
--
-- The choice follows the current screen, so that a prompt opens on the output
-- being worked on. It is reissued whenever that changes, which also covers the
-- case of the previous default being unplugged.
nominateLayerOutput :: Runtime -> X ()
nominateLayerOutput rt = forM_ (rtLayerShell rt) $ \_ -> do
  outs <- io . readIORef =<< asks riverOutputs
  ws <- gets windowset
  let SD current = W.screenDetail (W.current ws)
      live = filter (not . roRemoved) (M.elems outs)
      -- Match by position: that is the only thing tying a StackSet screen back
      -- to the output it was built from in 'syncScreens'.
      onScreen o = let (x, y) = roPosition o
                   in x == rect_x current && y == rect_y current
      chosen = case filter onScreen live of
        (o:_) -> Just o
        []    -> case sortOn roPosition live of
          (o:_) -> Just o
          []    -> Nothing
  forM_ chosen $ \o -> forM_ (roLayerObject o) $ \lo -> do
    prev <- io (readIORef (rtLayerDefault rt))
    unless (prev == Just (roObject o)) $ do
      conn <- asks riverConn
      io (riverLayerShellOutputV1SetDefault conn lo)
      io (writeIORef (rtLayerDefault rt) (Just (roObject o)))

-- | Reconcile the 'WindowSet'\'s screens with river's outputs.
--
-- This is xmonad's @rescreen@, driven by the output list rather than xinerama.
-- Outputs are ordered by position so that screen ids are stable across
-- reconnects, which is what @XMonad.Actions.PhysicalScreens@ relies on.
syncScreens :: X ()
syncScreens = do
  outs <- io . readIORef =<< asks riverOutputs
  let rects =
        [ rect
        | o <- sortOn roPosition (filter (not . roRemoved) (M.elems outs))
        , let (x, y) = roPosition o
        , let (width, height) = roSize o
        , width > 0 && height > 0
          -- Prefer the area layer shell reports, so a bar or dock that claims
          -- an exclusive zone shrinks the tiling area instead of being tiled
          -- over. It is only a hint, but honouring it is what users expect and
          -- it costs nothing to do so.
        , let rect = case roLayerArea o of
                Just a | rect_width a > 0 && rect_height a > 0 -> a
                _ -> Rectangle x y (fromIntegral width) (fromIntegral height)
        ]
  unless (null rects) $ modify $ \st ->
    st { windowset = rescreen rects (windowset st) }

-- | Lay the given screen rectangles over the current workspaces, preserving
-- which workspace is on which screen where possible.
rescreen :: [Rectangle] -> WindowSet -> WindowSet
rescreen rects ws = ws
    { W.current = (W.current ws) { W.screen = 0, W.screenDetail = SD firstRect }
    , W.visible = zipWith reseat [1 ..] restRects
    , W.hidden = newHidden
    }
  where
    (firstRect, restRects) = case rects of
      (r:rs) -> (r, rs)
      []     -> (Rectangle 0 0 0 0, [])
    -- Workspaces that were on now-absent screens fall back to hidden.
    oldVisible = W.visible ws
    reseat i r = case drop (i - 1) oldVisible of
      (s:_) -> s { W.screen = fromIntegral i, W.screenDetail = SD r }
      [] -> case newHidden of
        (h:_) -> W.Screen h (fromIntegral i) (SD r)
        []    -> W.Screen (W.workspace (W.current ws)) (fromIntegral i) (SD r)
    surplus = drop (length restRects) oldVisible
    newHidden = map W.workspace surplus ++ W.hidden ws

-- | Run the manage hook for windows river has just told us about, and insert
-- them into the 'WindowSet'.
--
-- This happens during a manage sequence, before the window has been rendered,
-- which is the same ordering guarantee xmonad's manage hook has — and the one
-- sway's IPC cannot provide.
adoptNewWindows :: X ()
adoptNewWindows = do
  ref <- asks riverWindows
  ws <- io (readIORef ref)
  let fresh = [ w | w <- M.elems ws, rwNew w, not (rwClosed w) ]
  forM_ fresh $ \w -> do
    io $ adjust ref (rwObject w) $ \x -> x { rwNew = False }
    mh <- asks (manageHook . config)
    g <- userCodeDef (mempty) (runQuery mh (rwObject w))
    ws' <- gets windowset
    let placed = W.insertUp (rwObject w) ws'
    modify $ \st -> st { windowset = appEndo g placed }
    void (broadcastEvent (WindowAdded (rwObject w)))

-- | Run the user's startup hook exactly once, after the first manage sequence
-- has been finished.
--
-- Anything it does that changes window management state goes through
-- 'XMonad.Operations.windows', which requests another manage sequence, so
-- deferring costs nothing but keeps a slow hook from tripping river's
-- watchdog.
runStartupHook :: Runtime -> X ()
runStartupHook rt = do
  done <- io (readIORef (rtStartupDone rt))
  unless done $ do
    io (writeIORef (rtStartupDone rt) True)
    hook <- asks (startupHook . config)
    _ <- userCode hook
    pure ()

--------------------------------------------------------------------------------
-- Bindings

-- | Create river bindings for any seat that does not have them yet.
createBindings :: Runtime -> X ()
createBindings rt = do
  seats <- io . readIORef =<< asks riverSeats
  bound <- io (readIORef (rtBoundSeats rt))
  let new = [ s | s <- M.keys seats, not (S.member s bound) ]
  forM_ new $ \seat -> do
    bindSeat rt seat
    io $ modifyIORef' (rtBoundSeats rt) (S.insert seat)

bindSeat :: Runtime -> ObjectId -> X ()
bindSeat rt seat = do
  conn <- asks riverConn
  bindingsGlobal <- asks riverBindings
  ks <- asks keyActions
  bs <- asks buttonActions

  forM_ (M.toList ks) $ \((mask, keysym), action) -> do
    b <- io (riverXkbBindingsV1GetXkbBinding conn bindingsGlobal seat keysym
               (riverModifiers mask))
    io $ modifyIORef' (rtBindings rt) (M.insert b action)
    io $ riverXkbBindingV1Listen conn b $ \case
      RiverXkbBindingV1Pressed -> do
        acts <- readIORef (rtBindings rt)
        forM_ (M.lookup b acts) $ \a -> do
          modifyIORef' (rtPending rt) (a :)
          riverWindowManagerV1ManageDirty conn (rtManager rt)
      _ -> pure ()
    io (riverXkbBindingV1Enable conn b)

  forM_ (M.toList bs) $ \((mask, button), action) -> do
    b <- io (riverSeatV1GetPointerBinding conn seat (linuxButton button)
               (riverModifiers mask))
    io $ modifyIORef' (rtPointerBind rt) (M.insert b action)
    io $ riverPointerBindingV1Listen conn b $ \case
      RiverPointerBindingV1Pressed -> do
        acts <- readIORef (rtPointerBind rt)
        forM_ (M.lookup b acts) $ \a -> do
          mHover <- readIORef (rtHovered rt)
          forM_ mHover $ \win -> do
            modifyIORef' (rtPending rt) (a win :)
            riverWindowManagerV1ManageDirty conn (rtManager rt)
      _ -> pure ()
    io (riverPointerBindingV1Enable conn b)

-- | The compositor resolves modifiers itself, and river's modifier bits are
-- numerically X11's, so the mask passes through unchanged apart from dropping
-- bits river has no entry for (lock and mod2).
riverModifiers :: KeyMask -> Word32
riverModifiers mask = mask .&. supported
  where
    supported = riverSeatV1ModifiersShift
            + riverSeatV1ModifiersCtrl
            + riverSeatV1ModifiersMod1
            + riverSeatV1ModifiersMod3
            + riverSeatV1ModifiersMod4
            + riverSeatV1ModifiersMod5

-- | X11 button numbers to Linux input event codes, which is what river's
-- pointer bindings take.
linuxButton :: Button -> Word32
linuxButton = \case
  1 -> 0x110  -- BTN_LEFT
  2 -> 0x112  -- BTN_MIDDLE
  3 -> 0x111  -- BTN_RIGHT
  4 -> 0x113  -- BTN_SIDE
  5 -> 0x114  -- BTN_EXTRA
  n -> 0x110 + fromIntegral n

-- | Queue an action to run at the start of the next manage sequence.
queueAction :: Runtime -> X () -> IO ()
queueAction rt act = modifyIORef' (rtPending rt) (act :)

runPending :: Runtime -> X ()
runPending rt = do
  acts <- io (atomicModifyIORef' (rtPending rt) (\as -> ([], reverse as)))
  mapM_ (userCode) acts

--------------------------------------------------------------------------------
-- Layout

-- | Run the layout for every visible screen, propose the resulting dimensions,
-- set keyboard focus, and stash the rectangles for the render sequence.
applyLayout :: Runtime -> X ()
applyLayout rt = do
  ws <- gets windowset
  let screens = W.current ws : W.visible ws
  placements <- fmap concat $ forM screens $ \scr -> do
    let wsp = W.workspace scr
        SD rect = W.screenDetail scr
    (rs, mLayout) <- userCodeDef ([], Nothing) (runLayout wsp rect)
    forM_ mLayout $ \l' -> modify $ \st ->
      st { windowset = updateLayout (W.tag wsp) l' (windowset st) }
    pure rs

  io $ writeIORef (rtPlacements rt) placements
  io $ writeIORef (rtVisible rt) (S.fromList (map fst placements))

  conn <- asks riverConn
  winRef <- asks riverWindows
  known <- io (readIORef winRef)

  -- Dimensions are window management state, so they go here rather than in
  -- the render sequence.
  forM_ placements $ \(win, r) -> when (M.member win known) $
    io $ riverWindowV1ProposeDimensions conn win
           (fromIntegral (rect_width r)) (fromIntegral (rect_height r))

  -- Keyboard focus, likewise. A seat whose keyboard has gone to a layer
  -- surface is left alone: river discards focus requests outright while focus
  -- is exclusive, and in the non-exclusive case setting focus in this same
  -- manage sequence would silently steal the keyboard back — which is the
  -- difference between a fuzzel prompt you can type into and one you cannot.
  seats <- io . readIORef =<< asks riverSeats
  forM_ (M.elems seats) $ \s ->
    unless (layerHasFocus (rsLayerFocus s)) $
      case W.peek ws of
        Just win | M.member win known ->
          io (riverSeatV1FocusWindow conn (rsObject s) win)
        _ -> io (riverSeatV1ClearFocus conn (rsObject s))

updateLayout :: WorkspaceId -> Layout Window -> WindowSet -> WindowSet
updateLayout i l = W.mapWorkspace $ \wsp ->
  if W.tag wsp == i then wsp { W.layout = l } else wsp

--------------------------------------------------------------------------------
-- The render sequence

renderSequence :: Runtime -> X ()
renderSequence rt = do
  conn <- asks riverConn
  placements <- io (readIORef (rtPlacements rt))
  visible <- io (readIORef (rtVisible rt))
  winRef <- asks riverWindows
  known <- io (readIORef winRef)
  bw <- asks (borderWidth . config)
  focusedCol <- asks focusedBorder
  normalCol <- asks normalBorder
  mFocus <- W.peek <$> gets windowset

  forM_ placements $ \(win, r) -> forM_ (M.lookup win known) $ \w -> do
    io $ riverNodeV1SetPosition conn (rwNode w) (rect_x r) (rect_y r)
    when (bw > 0) $ do
      let (red, green, blue, alpha) =
            if Just win == mFocus then focusedCol else normalCol
      io $ riverWindowV1SetBorders conn win allEdges (fromIntegral bw)
             red green blue alpha
    when (rwHidden w) $ do
      io (riverWindowV1Show conn win)
      io $ adjust winRef win $ \x -> x { rwHidden = False }

  -- Anything not placed by the layout belongs to a workspace that is not on
  -- screen. river has no concept of workspaces, so this is what implements
  -- them.
  forM_ (M.elems known) $ \w ->
    unless (S.member (rwObject w) visible || rwHidden w) $ do
      io (riverWindowV1Hide conn (rwObject w))
      io $ adjust winRef (rwObject w) $ \x -> x { rwHidden = True }

  -- Stacking order: the layout list is in the desired bottom-to-top order.
  forM_ placements $ \(win, _) -> forM_ (M.lookup win known) $ \w ->
    io (riverNodeV1PlaceTop conn (rwNode w))

allEdges :: Word32
allEdges = riverWindowV1EdgesTop + riverWindowV1EdgesBottom
         + riverWindowV1EdgesLeft + riverWindowV1EdgesRight

--------------------------------------------------------------------------------
-- Colours

-- | Parse @\"#rrggbb\"@ into the 32-bit channel values river's @set_borders@
-- takes. Unparseable colours become opaque black rather than an error, since a
-- typo in a config should not stop the window manager starting.
parseColor :: String -> (Word32, Word32, Word32, Word32)
parseColor ('#':r1:r2:g1:g2:b1:b2:_) =
  case mapM hexPair [[r1,r2],[g1,g2],[b1,b2]] of
    Just [r, g, b] -> (scale r, scale g, scale b, maxBound)
    _              -> (0, 0, 0, maxBound)
  where
    -- river takes 32-bit channels; 8-bit values are widened by replication so
    -- that 0xff maps to 0xffffffff rather than 0xff000000.
    scale v = v * 0x01010101
    hexPair [a, b] = (\x y -> x * 16 + y) <$> hexDigit a <*> hexDigit b
    hexPair _ = Nothing
    hexDigit c
      | c >= '0' && c <= '9' = Just (fromIntegral (fromEnum c - fromEnum '0'))
      | c >= 'a' && c <= 'f' = Just (fromIntegral (fromEnum c - fromEnum 'a' + 10))
      | c >= 'A' && c <= 'F' = Just (fromIntegral (fromEnum c - fromEnum 'A' + 10))
      | otherwise = Nothing
parseColor _ = (0, 0, 0, maxBound)
