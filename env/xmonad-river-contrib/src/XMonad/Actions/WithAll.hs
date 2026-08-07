-- | Act on every window in the current workspace.
module XMonad.Actions.WithAll
  ( withAll
  , withAll'
  , sinkAll
  , killAll
  ) where

import Data.Foldable (for_)

import XMonad.Core
import XMonad.River.X11Compat (Window)
import XMonad.Operations (killWindow, windows)
import qualified XMonad.StackSet as W

-- | Apply a function to every window in the current workspace.
withAll' :: (Window -> WindowSet -> WindowSet) -> X ()
withAll' f = windows $ \ws ->
  foldr f ws (W.integrate' (W.stack (W.workspace (W.current ws))))

withAll :: (Window -> X ()) -> X ()
withAll f = withWindowSet $ \ws ->
  for_ (W.integrate' (W.stack (W.workspace (W.current ws)))) f

-- | Unfloat every window in the current workspace.
sinkAll :: X ()
sinkAll = withAll' W.sink

killAll :: X ()
killAll = withAll killWindow
