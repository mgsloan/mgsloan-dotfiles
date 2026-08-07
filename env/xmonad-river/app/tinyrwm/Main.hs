-- | A Haskell port of river's reference window manager, @tinyrwm@.
--
-- This exists to prove the protocol stack — wire codec, generated bindings,
-- and the manage/render state machine — independently of the xmonad
-- compatibility layer. If this runs correctly under river, any bug in
-- @custom-xmonad@ is in the xmonad layer rather than underneath it.
--
-- It implements the same feature set as the other tinyrwm implementations:
--
-- * @Super+Space@ spawns a terminal
-- * @Super+q@ closes the focused window
-- * @Super+n@ cycles focus
-- * @Super+Escape@ exits the session
-- * @Super+Left drag@ moves, @Super+Right drag@ resizes
--
-- Windows are stacked bottom-to-top in focus order, newest on top.
module Main (main) where

import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.Int (Int32)
import Data.IORef
import Data.Word (Word32)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Process (spawnCommand)
import qualified Data.Map.Strict as M

import XMonad.River.Connection
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Wire

--------------------------------------------------------------------------------
-- Keysyms and button codes

-- | xkbcommon keysyms, numerically identical to X11 keysyms — which is what
-- lets the xmonad compatibility layer reuse xmonad's key names unchanged.
xkSpace, xkQ, xkN, xkEscape :: Word32
xkSpace  = 0x0020
xkQ      = 0x0071
xkN      = 0x006e
xkEscape = 0xff1b

-- | Linux input event codes, from @linux/input-event-codes.h@.
btnLeft, btnRight :: Word32
btnLeft  = 0x110
btnRight = 0x111

--------------------------------------------------------------------------------
-- State

data Action = ASpawnTerminal | AClose | AFocusNext | AMove | AResize | AExit
  deriving (Eq, Show)

data Window = Window
  { winNode   :: !ObjectId
  , winNew    :: !Bool
  , winClosed :: !Bool
  , winX, winY, winWidth, winHeight :: !Int32
  , winMoveRequested   :: !(Maybe ObjectId)
  , winResizeRequested :: !(Maybe (ObjectId, Word32))
  }

-- | An interactive pointer operation in progress.
data Op
  = OpNone
  | OpMove   { opWindow :: !ObjectId, opStartX, opStartY :: !Int32 }
  | OpResize { opWindow :: !ObjectId, opStartX, opStartY :: !Int32
             , opStartW, opStartH :: !Int32, opEdges :: !Word32 }

data Seat = Seat
  { seatNew        :: !Bool
  , seatRemoved    :: !Bool
  , seatFocused    :: !(Maybe ObjectId)
  , seatHovered    :: !(Maybe ObjectId)
  , seatInteracted :: !(Maybe ObjectId)
  , seatPending    :: !(Maybe Action)
  , seatOp         :: !Op
  , seatOpDx, seatOpDy :: !Int32
  , seatOpRelease  :: !Bool
  }

data WM = WM
  { wmConn     :: !Connection
  , wmManager  :: !ObjectId
  , wmBindings :: !ObjectId
  , wmOrder    :: !(IORef [ObjectId])
    -- ^ Bottom-to-top stacking order; the last entry has focus.
  , wmWindows  :: !(IORef (M.Map ObjectId Window))
  , wmSeats    :: !(IORef (M.Map ObjectId Seat))
  , wmOutputs  :: !(IORef (M.Map ObjectId Bool))
    -- ^ Output object to its "removed" flag.
  , wmActions  :: !(IORef (M.Map ObjectId Action))
    -- ^ Binding object to the action it triggers.
  }

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  conn <- connect
  (registry, globals) <- getRegistry conn
  mManager <- bindGlobal conn registry globals
                riverWindowManagerV1Interface 4 riverWindowManagerV1Version
  mBindings <- bindGlobal conn registry globals
                 riverXkbBindingsV1Interface 1 riverXkbBindingsV1Version
  case (mManager, mBindings) of
    (Just (manager, _), Just (bindings, _)) -> do
      wm <- WM conn manager bindings
              <$> newIORef [] <*> newIORef M.empty <*> newIORef M.empty
              <*> newIORef M.empty <*> newIORef M.empty
      riverWindowManagerV1Listen conn manager (onManagerEvent wm)
      let loop = dispatch conn >> loop
      loop
    _ -> do
      hPutStrLn stderr
        "river_window_manager_v1 (>= 4) or river_xkb_bindings_v1 not supported \
        \by the Wayland server"
      exitFailure

onManagerEvent :: WM -> RiverWindowManagerV1Event -> IO ()
onManagerEvent wm = \case
  RiverWindowManagerV1Unavailable -> do
    hPutStrLn stderr "error: another window manager is already running"
    exitFailure
  RiverWindowManagerV1Finished    -> exitSuccess
  RiverWindowManagerV1ManageStart -> manageSequence wm
  RiverWindowManagerV1RenderStart -> renderSequence wm
  RiverWindowManagerV1Window win  -> addWindow wm win
  RiverWindowManagerV1Output out  -> addOutput wm out
  RiverWindowManagerV1Seat seat   -> addSeat wm seat
  _ -> pure ()

--------------------------------------------------------------------------------
-- Object tracking

addWindow :: WM -> ObjectId -> IO ()
addWindow wm win = do
  node <- riverWindowV1GetNode (wmConn wm) win
  modifyIORef' (wmWindows wm) . M.insert win $ Window
    { winNode = node, winNew = True, winClosed = False
    , winX = 0, winY = 0, winWidth = 0, winHeight = 0
    , winMoveRequested = Nothing, winResizeRequested = Nothing
    }
  modifyIORef' (wmOrder wm) (++ [win])
  riverWindowV1Listen (wmConn wm) win $ \case
    RiverWindowV1Closed -> adjustWindow wm win $ \w -> w { winClosed = True }
    RiverWindowV1Dimensions width height ->
      adjustWindow wm win $ \w -> w { winWidth = width, winHeight = height }
    RiverWindowV1PointerMoveRequested seat ->
      adjustWindow wm win $ \w -> w { winMoveRequested = Just seat }
    RiverWindowV1PointerResizeRequested seat edges ->
      adjustWindow wm win $ \w -> w { winResizeRequested = Just (seat, edges) }
    _ -> pure ()

addOutput :: WM -> ObjectId -> IO ()
addOutput wm out = do
  modifyIORef' (wmOutputs wm) (M.insert out False)
  riverOutputV1Listen (wmConn wm) out $ \case
    RiverOutputV1Removed -> modifyIORef' (wmOutputs wm) (M.insert out True)
    _ -> pure ()

addSeat :: WM -> ObjectId -> IO ()
addSeat wm seat = do
  modifyIORef' (wmSeats wm) . M.insert seat $ Seat
    { seatNew = True, seatRemoved = False
    , seatFocused = Nothing, seatHovered = Nothing, seatInteracted = Nothing
    , seatPending = Nothing, seatOp = OpNone
    , seatOpDx = 0, seatOpDy = 0, seatOpRelease = False
    }
  riverSeatV1Listen (wmConn wm) seat $ \case
    RiverSeatV1Removed -> adjustSeat wm seat $ \s -> s { seatRemoved = True }
    RiverSeatV1PointerEnter win ->
      adjustSeat wm seat $ \s -> s { seatHovered = Just win }
    RiverSeatV1PointerLeave ->
      adjustSeat wm seat $ \s -> s { seatHovered = Nothing }
    RiverSeatV1WindowInteraction win ->
      adjustSeat wm seat $ \s -> s { seatInteracted = Just win }
    RiverSeatV1OpDelta dx dy ->
      adjustSeat wm seat $ \s -> s { seatOpDx = dx, seatOpDy = dy }
    RiverSeatV1OpRelease ->
      adjustSeat wm seat $ \s -> s { seatOpRelease = True }
    _ -> pure ()

adjustWindow :: WM -> ObjectId -> (Window -> Window) -> IO ()
adjustWindow wm win f = modifyIORef' (wmWindows wm) (M.adjust f win)

adjustSeat :: WM -> ObjectId -> (Seat -> Seat) -> IO ()
adjustSeat wm seat f = modifyIORef' (wmSeats wm) (M.adjust f seat)

withWindow :: WM -> ObjectId -> (Window -> IO ()) -> IO ()
withWindow wm win f = readIORef (wmWindows wm) >>= \ws -> forM_ (M.lookup win ws) f

withSeat :: WM -> ObjectId -> (Seat -> IO ()) -> IO ()
withSeat wm seat f = readIORef (wmSeats wm) >>= \ss -> forM_ (M.lookup seat ss) f

setPosition :: WM -> ObjectId -> Int32 -> Int32 -> IO ()
setPosition wm win x y = withWindow wm win $ \w -> do
  riverNodeV1SetPosition (wmConn wm) (winNode w) x y
  adjustWindow wm win $ \w' -> w' { winX = x, winY = y }

--------------------------------------------------------------------------------
-- Bindings

createBindings :: WM -> ObjectId -> IO ()
createBindings wm seat = do
  forM_ keyBindings $ \(keysym, action) -> do
    b <- riverXkbBindingsV1GetXkbBinding (wmConn wm) (wmBindings wm) seat keysym super
    register b action
    riverXkbBindingV1Listen (wmConn wm) b $ \case
      RiverXkbBindingV1Pressed -> setPending wm seat b
      _ -> pure ()
    riverXkbBindingV1Enable (wmConn wm) b
  forM_ pointerBindings $ \(button, action) -> do
    b <- riverSeatV1GetPointerBinding (wmConn wm) seat button super
    register b action
    riverPointerBindingV1Listen (wmConn wm) b $ \case
      RiverPointerBindingV1Pressed -> setPending wm seat b
      _ -> pure ()
    riverPointerBindingV1Enable (wmConn wm) b
  where
    super = riverSeatV1ModifiersMod4
    register b action = modifyIORef' (wmActions wm) (M.insert b action)
    keyBindings =
      [ (xkSpace, ASpawnTerminal), (xkQ, AClose)
      , (xkN, AFocusNext), (xkEscape, AExit) ]
    pointerBindings = [ (btnLeft, AMove), (btnRight, AResize) ]

-- | Bindings fire outside a manage sequence, so the action is recorded and
-- carried out when the next one starts.
setPending :: WM -> ObjectId -> ObjectId -> IO ()
setPending wm seat binding = do
  actions <- readIORef (wmActions wm)
  forM_ (M.lookup binding actions) $ \action ->
    adjustSeat wm seat $ \s -> s { seatPending = Just action }

--------------------------------------------------------------------------------
-- The manage sequence

manageSequence :: WM -> IO ()
manageSequence wm = do
  reap wm
  mapM_ (manageWindow wm) . M.keys =<< readIORef (wmWindows wm)
  mapM_ (manageSeat wm) . M.keys =<< readIORef (wmSeats wm)
  riverWindowManagerV1ManageFinish (wmConn wm) (wmManager wm)

-- | Destroy objects the server has told us are gone. Done at the start of a
-- manage sequence so policy never observes a half-dead window.
reap :: WM -> IO ()
reap wm = do
  outs <- readIORef (wmOutputs wm)
  forM_ (M.toList outs) $ \(out, removed) -> when removed $ do
    riverOutputV1Destroy (wmConn wm) out
    modifyIORef' (wmOutputs wm) (M.delete out)

  ws <- readIORef (wmWindows wm)
  forM_ (M.toList ws) $ \(win, w) -> when (winClosed w) $ do
    -- Clear the window out of every seat before destroying it.
    seats <- readIORef (wmSeats wm)
    forM_ (M.toList seats) $ \(seat, s) -> do
      when (seatFocused s == Just win) $
        adjustSeat wm seat $ \s' -> s' { seatFocused = Nothing }
      when (opWindowOf (seatOp s) == Just win) $ do
        riverSeatV1OpEnd (wmConn wm) seat
        adjustSeat wm seat $ \s' -> s' { seatOp = OpNone }
    riverWindowV1Destroy (wmConn wm) win
    modifyIORef' (wmWindows wm) (M.delete win)
    modifyIORef' (wmOrder wm) (filter (/= win))

  seats <- readIORef (wmSeats wm)
  forM_ (M.toList seats) $ \(seat, s) -> when (seatRemoved s) $ do
    riverSeatV1Destroy (wmConn wm) seat
    modifyIORef' (wmSeats wm) (M.delete seat)

opWindowOf :: Op -> Maybe ObjectId
opWindowOf OpNone = Nothing
opWindowOf op     = Just (opWindow op)

manageWindow :: WM -> ObjectId -> IO ()
manageWindow wm win = withWindow wm win $ \w -> do
  when (winNew w) $ do
    adjustWindow wm win $ \w' -> w' { winNew = False }
    setPosition wm win 0 0
    -- Zero dimensions let the window choose its own size.
    riverWindowV1ProposeDimensions (wmConn wm) win 0 0
  forM_ (winMoveRequested w) $ \seat -> do
    adjustWindow wm win $ \w' -> w' { winMoveRequested = Nothing }
    startMove wm seat win
  forM_ (winResizeRequested w) $ \(seat, edges) -> do
    adjustWindow wm win $ \w' -> w' { winResizeRequested = Nothing }
    startResize wm seat win edges

manageSeat :: WM -> ObjectId -> IO ()
manageSeat wm seat = do
  withSeat wm seat $ \s -> do
    when (seatNew s) $ do
      adjustSeat wm seat $ \s' -> s' { seatNew = False }
      createBindings wm seat
    -- Passing the interacted window (or Nothing) every sequence is what keeps
    -- the topmost window focused when a new one appears.
    focus wm seat (seatInteracted s)
    adjustSeat wm seat $ \s' -> s' { seatInteracted = Nothing }
    forM_ (seatPending s) $ \action -> do
      adjustSeat wm seat $ \s' -> s' { seatPending = Nothing }
      runAction wm seat action
  -- Re-read: the action above may have started or ended an op.
  withSeat wm seat (updateOp wm seat)

updateOp :: WM -> ObjectId -> Seat -> IO ()
updateOp wm seat s = case seatOp s of
  OpNone -> pure ()
  OpMove{} -> when (seatOpRelease s) (endOp wm seat)
  op@OpResize{}
    | seatOpRelease s -> do
        riverWindowV1InformResizeEnd (wmConn wm) (opWindow op)
        endOp wm seat
    | otherwise -> do
        let edges = opEdges op
            adjust startSize lowEdge highEdge delta =
              startSize
                - (if edges .&. lowEdge  /= 0 then delta else 0)
                + (if edges .&. highEdge /= 0 then delta else 0)
            width  = adjust (opStartW op) riverWindowV1EdgesLeft
                            riverWindowV1EdgesRight (seatOpDx s)
            height = adjust (opStartH op) riverWindowV1EdgesTop
                            riverWindowV1EdgesBottom (seatOpDy s)
        riverWindowV1ProposeDimensions (wmConn wm) (opWindow op)
          (max 1 width) (max 1 height)

endOp :: WM -> ObjectId -> IO ()
endOp wm seat = do
  riverSeatV1OpEnd (wmConn wm) seat
  adjustSeat wm seat $ \s -> s { seatOp = OpNone, seatOpRelease = False }

--------------------------------------------------------------------------------
-- Focus and actions

-- | Focus a window, raising it to the top of the stacking order. Given
-- 'Nothing', focuses whatever is currently topmost.
focus :: WM -> ObjectId -> Maybe ObjectId -> IO ()
focus wm seat mWin = do
  order <- readIORef (wmOrder wm)
  let target = case mWin of
        Just w                -> Just w
        Nothing | null order  -> Nothing
                | otherwise   -> Just (last order)
  withSeat wm seat $ \s -> unless (seatFocused s == target) $ do
    case target of
      Nothing -> riverSeatV1ClearFocus (wmConn wm) seat
      Just win -> withWindow wm win $ \w -> do
        riverSeatV1FocusWindow (wmConn wm) seat win
        riverNodeV1PlaceTop (wmConn wm) (winNode w)
        modifyIORef' (wmOrder wm) ((++ [win]) . filter (/= win))
    adjustSeat wm seat $ \s' -> s' { seatFocused = target }

runAction :: WM -> ObjectId -> Action -> IO ()
runAction wm seat = \case
  ASpawnTerminal -> void (spawnCommand "foot")
  AClose -> withSeat wm seat $ \s ->
    forM_ (seatFocused s) (riverWindowV1Close (wmConn wm))
  AFocusNext -> readIORef (wmOrder wm) >>= \case
    -- Focusing the bottom window rotates the stack.
    (bottom:_) -> focus wm seat (Just bottom)
    []         -> pure ()
  AMove -> withHovered wm seat (startMove wm seat)
  AResize -> withHovered wm seat $ \win -> startResize wm seat win
    (riverWindowV1EdgesBottom .|. riverWindowV1EdgesRight)
  AExit -> riverWindowManagerV1ExitSession (wmConn wm) (wmManager wm)

withHovered :: WM -> ObjectId -> (ObjectId -> IO ()) -> IO ()
withHovered wm seat f = withSeat wm seat $ \s -> case seatOp s of
  OpNone -> forM_ (seatHovered s) f
  _      -> pure ()

startMove :: WM -> ObjectId -> ObjectId -> IO ()
startMove wm seat win = do
  focus wm seat (Just win)
  withWindow wm win $ \w -> do
    riverSeatV1OpStartPointer (wmConn wm) seat
    adjustSeat wm seat $ \s -> s
      { seatOp = OpMove win (winX w) (winY w)
      , seatOpDx = 0, seatOpDy = 0, seatOpRelease = False }

startResize :: WM -> ObjectId -> ObjectId -> Word32 -> IO ()
startResize wm seat win edges = do
  focus wm seat (Just win)
  withWindow wm win $ \w -> do
    riverWindowV1InformResizeStart (wmConn wm) win
    riverSeatV1OpStartPointer (wmConn wm) seat
    adjustSeat wm seat $ \s -> s
      { seatOp = OpResize win (winX w) (winY w) (winWidth w) (winHeight w) edges
      , seatOpDx = 0, seatOpDy = 0, seatOpRelease = False }

--------------------------------------------------------------------------------
-- The render sequence

renderSequence :: WM -> IO ()
renderSequence wm = do
  seats <- readIORef (wmSeats wm)
  mapM_ (renderSeat wm) (M.toList seats)
  riverWindowManagerV1RenderFinish (wmConn wm) (wmManager wm)

renderSeat :: WM -> (ObjectId, Seat) -> IO ()
renderSeat wm (_, s) = case seatOp s of
  OpNone -> pure ()
  op@OpMove{} ->
    setPosition wm (opWindow op) (opStartX op + seatOpDx s) (opStartY op + seatOpDy s)
  op@OpResize{} -> withWindow wm (opWindow op) $ \w -> do
    -- Resizing from a top or left edge moves the origin by however much the
    -- size actually changed, which differs from the pointer delta when the
    -- window refuses the proposed size.
    let originFor startPos startSize currentSize edge
          | opEdges op .&. edge /= 0 = startPos + startSize - currentSize
          | otherwise                = startPos
    setPosition wm (opWindow op)
      (originFor (opStartX op) (opStartW op) (winWidth w) riverWindowV1EdgesLeft)
      (originFor (opStartY op) (opStartH op) (winHeight w) riverWindowV1EdgesTop)
