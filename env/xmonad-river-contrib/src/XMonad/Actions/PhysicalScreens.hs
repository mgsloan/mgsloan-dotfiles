-- | Address screens by physical position rather than by the order river
-- happens to report them.
module XMonad.Actions.PhysicalScreens
  ( PhysicalScreen(..)
  , ScreenComparator(..)
  , screenComparatorByRectangle
  , screenComparatorByScreenId
  , getScreen
  , viewScreen
  , sendToScreen
  ) where

import Data.List (sortBy)

import XMonad.Core
import XMonad.Operations (windows)
import XMonad.River.X11Compat (Rectangle)
import qualified XMonad.StackSet as W

-- | A screen index in the order imposed by a 'ScreenComparator'.
newtype PhysicalScreen = P Int deriving (Eq, Ord, Show, Read)

newtype ScreenComparator =
  ScreenComparator (Rectangle -> Rectangle -> Ordering)

-- | Order screens by their geometry, which under river is their position in
-- the compositor's global coordinate space.
screenComparatorByRectangle :: (Rectangle -> Rectangle -> Ordering) -> ScreenComparator
screenComparatorByRectangle = ScreenComparator

-- | Order screens by river's own numbering.
screenComparatorByScreenId :: ScreenComparator
screenComparatorByScreenId = ScreenComparator (\_ _ -> EQ)

-- | Resolve a physical index to the 'ScreenId' currently at that position.
getScreen :: ScreenComparator -> PhysicalScreen -> X (Maybe ScreenId)
getScreen (ScreenComparator cmp) (P i) = withWindowSet $ \ws -> do
  let screens = W.current ws : W.visible ws
      ordered = sortBy (\a b -> cmp (rectOf a) (rectOf b)) screens
      rectOf s = case W.screenDetail s of SD r -> r
  pure $ case drop i ordered of
    (s:_) -> Just (W.screen s)
    []    -> Nothing

viewScreen :: ScreenComparator -> PhysicalScreen -> X ()
viewScreen sc p = getScreen sc p >>= \ms -> whenJust ms $ \sid ->
  withWindowSet $ \ws -> whenJust (W.lookupWorkspace sid ws) $ \i ->
    windows (W.view i)

sendToScreen :: ScreenComparator -> PhysicalScreen -> X ()
sendToScreen sc p = getScreen sc p >>= \ms -> whenJust ms $ \sid ->
  withWindowSet $ \ws -> whenJust (W.lookupWorkspace sid ws) $ \i ->
    windows (W.shift i)
