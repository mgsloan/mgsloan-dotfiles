-- | dwm-like promotion of the focused window to master.
module XMonad.Actions.DwmPromote (dwmpromote) where

import XMonad.Core
import XMonad.Operations (windows)
import qualified XMonad.StackSet as W

-- | Move the focused window to master, keeping focus on it.
--
-- If the focused window already /is/ master, swap it with the next window
-- down instead. That is what makes repeated presses toggle between two
-- windows rather than doing nothing.
--
-- In a 'W.Stack', @up@ is stored nearest-first, so the master window is the
-- last element of it and restoring document order means reversing.
dwmpromote :: X ()
dwmpromote = windows $ W.modify' $ \c -> case c of
  W.Stack _ [] []     -> c
  W.Stack t [] (x:rs) -> W.Stack x [] (t:rs)
  W.Stack t ls rs     -> W.Stack t [] (reverse ls ++ rs)
