-- | Move and resize a window in one gesture.
module XMonad.Actions.FlexibleManipulate
  ( mouseWindow
  , discrete
  , linear
  , resize
  , position
  ) where

import XMonad.Core
import XMonad.River.X11Compat (Window)

-- | Drag a window, with the grab point deciding whether the gesture moves or
-- resizes it.
--
-- Not implemented. river expresses interactive pointer gestures through the
-- seat's operation cycle (@op_start_pointer@, @op_delta@, @op_release@) rather
-- than by letting the window manager grab the pointer, and those events are
-- not yet routed into the 'X' monad. The gesture model itself translates
-- cleanly once they are.
mouseWindow :: (Double -> Double) -> Window -> X ()
mouseWindow _ _ = warnUnimplemented "mouseWindow (FlexibleManipulate)"
  "Mod+drag to move or resize a window does nothing. Needs river's seat \
  \pointer operation cycle to be wired into XConf."

-- | Snap to move, resize, or both, depending on which third of the window was
-- grabbed.
discrete :: Double -> Double
discrete = fromIntegral . round' where round' = round :: Double -> Integer

linear :: Double -> Double
linear = id

resize :: Double -> Double
resize = const 1

position :: Double -> Double
position = const 0
