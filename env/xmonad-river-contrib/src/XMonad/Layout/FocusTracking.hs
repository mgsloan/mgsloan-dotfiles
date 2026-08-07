-- | Keeps the focused window on top.
module XMonad.Layout.FocusTracking (focusTracking, FocusTracking) where

import XMonad.Core
import qualified XMonad.StackSet as W

-- | A layout modifier that raises the focused window above the others.
--
-- Under xmonad this exists because a floating window could otherwise be
-- obscured by tiled ones after a focus change. Under river the window
-- manager sets the render list explicitly every render sequence, placing the
-- focused window last — so the behaviour this provides is already
-- unconditional, and the modifier is a pass-through kept for source
-- compatibility.
newtype FocusTracking l a = FocusTracking (l a) deriving (Read, Show)

focusTracking :: l a -> FocusTracking l a
focusTracking = FocusTracking

instance LayoutClass l a => LayoutClass (FocusTracking l) a where
  runLayout (W.Workspace i (FocusTracking l) ms) r = do
    (rs, ml') <- runLayout (W.Workspace i l ms) r
    pure (rs, FocusTracking <$> ml')
  handleMessage (FocusTracking l) = fmap (fmap FocusTracking) . handleMessage l
  description (FocusTracking l) = description l
