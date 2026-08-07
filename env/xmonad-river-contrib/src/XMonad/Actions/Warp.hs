-- | Moving the pointer under program control.
--
-- Ordinary Wayland clients cannot warp the pointer. The window manager is not
-- an ordinary client: @river_seat_v1.pointer_warp@ exists for exactly this,
-- so this config's @warpMid@ — which fires on nearly every focus change —
-- works unchanged.
module XMonad.Actions.Warp
  ( warpToWindow
  , warpToScreen
  , banish
  , Corner(..)
  ) where

import XMonad.Core
import XMonad.Operations (warpPointer)
import XMonad.River.X11Compat
import qualified XMonad.StackSet as W

-- | Warp to a point within the focused window, given as fractions of its
-- width and height.
warpToWindow :: Rational -> Rational -> X ()
warpToWindow h v = withWindowSet $ \ws -> whenJust (W.peek ws) $ \w -> do
  -- river reports a window's dimensions but not its position, since position
  -- is something the window manager assigned. So the rectangle comes from
  -- re-running the layout rather than from the compositor.
  placements <- currentPlacements
  whenJust (lookup w placements) $ \r ->
    warpPointer
      (rect_x r + round (fromIntegral (rect_width r) * h))
      (rect_y r + round (fromIntegral (rect_height r) * v))

-- | Warp to a point within the given screen.
warpToScreen :: ScreenId -> Rational -> Rational -> X ()
warpToScreen n h v = withWindowSet $ \ws ->
  whenJust (lookup n [ (W.screen s, W.screenDetail s)
                     | s <- W.current ws : W.visible ws ]) $ \(SD r) ->
    warpPointer
      (rect_x r + round (fromIntegral (rect_width r) * h))
      (rect_y r + round (fromIntegral (rect_height r) * v))

data Corner = UpperLeft | UpperRight | LowerLeft | LowerRight

banish :: Corner -> X ()
banish c = uncurry warpToWindow $ case c of
  UpperLeft  -> (0, 0)
  UpperRight -> (1, 0)
  LowerLeft  -> (0, 1)
  LowerRight -> (1, 1)

-- | Rectangles assigned by the most recent layout run.
currentPlacements :: X [(Window, Rectangle)]
currentPlacements = do
  ws <- withWindowSet pure
  let scr = W.current ws
      SD r = W.screenDetail scr
  fst <$> runLayout (W.workspace scr) r
