module TallWheel where

import qualified XMonad.StackSet as W

import Imports

-- | Identical to XMonad's builtin tiling mode, 'Tall', except that the
-- order is reversed for the left hand stack. This makes it feel like
-- you're rotating through a wheel of windows, rather than manipulating
-- stacks. Supports 'Shrink', 'Expand' and 'IncMasterN'.
data TallWheel a = TallWheel
  { tallWheelNMaster :: !Int
  -- ^ The default number of windows in the master pane (default: 1)
  , tallWheelRatioIncrement :: !Rational
  -- ^ Percent of screen to increment by when resizing panes (default: 3/100)
  , tallWheelRatio :: !Rational
  -- ^ Default proportion of screen occupied by master pane (default: 1/2)
  }
  deriving (Show, Read)

instance LayoutClass TallWheel a where
    pureLayout (TallWheel nmaster _ frac) r s = zip ws rs
      where ws = W.integrate s
            rs = tileWheel frac r nmaster (length ws)

    pureMessage (TallWheel nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink = TallWheel nmaster delta (max 0 $ frac-delta)
            resize Expand = TallWheel nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = TallWheel (max 0 (nmaster+d)) delta frac

    description _ = "TallWheel"

-- | Compute the positions for windows using the default two-pane tiling
-- algorithm.
--
-- The screen is divided into two panes. All clients are
-- then partioned between these two panes. One pane, the master, by
-- convention has the least number of windows in it.
tileWheel
    :: Rational  -- ^ @frac@, what proportion of the screen to devote to the master area
    -> Rectangle -- ^ @r@, the rectangle representing the screen
    -> Int       -- ^ @nmaster@, the number of windows in the master pane
    -> Int       -- ^ @n@, the total number of windows to tile
    -> [Rectangle]
tileWheel f r nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically n r
    else reverse (splitVertically nmaster r1) ++
         splitVertically (n-nmaster) r2
  where (r1,r2) = splitHorizontallyBy f r
