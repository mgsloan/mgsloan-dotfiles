-- | The pure geometry helpers from xmonad's "XMonad.Layout".
--
-- Vendored because they are the only part of that module a custom layout
-- normally needs, and because they depend on nothing but 'Rectangle'. Upstream
-- imports @Graphics.X11@ for that type and @Graphics.X11.Xlib.Extras@ for
-- 'DestroyWindowEvent'; here the type comes from "XMonad.River.X11Compat" and
-- the event handling has no analogue, so only the geometry is carried over.
module XMonad.Layout
  ( -- * Geometry
    splitVertically
  , splitHorizontally
  , splitHorizontallyBy
  , splitVerticallyBy
  , mirrorRect
  , tile
    -- * Layouts
  , Tall(..)
  , Full(..)
  , Mirror(..)
  , Choose
  , (|||)
  , ChangeLayout(..)
    -- * Messages
  , Resize(..)
  , IncMasterN(..)
  ) where

import Control.Monad (msum)
import Data.Maybe (fromMaybe)

import XMonad.Core
import XMonad.River.X11Compat (Rectangle(..))
import qualified XMonad.StackSet as W

-- | Divide a rectangle into @n@ equal horizontal bands, top to bottom. Any
-- remainder from the integer division lands in the last band, so the pieces
-- always exactly tile the original.
splitVertically :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) =
    Rectangle sx sy sw smallh
      : splitVertically (n - 1)
          (Rectangle sx (sy + fromIntegral smallh) sw (sh - smallh))
  where smallh = sh `div` fromIntegral n

-- | As 'splitVertically', but into vertical columns, left to right.
splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitHorizontally n = map mirrorRect . splitVertically n . mirrorRect

-- | Split into left and right at the given fraction of the width.
splitHorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw - leftw) sh
    )
  where leftw = floor (fromIntegral sw * f)

-- | Split into top and bottom at the given fraction of the height.
splitVerticallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitVerticallyBy f r =
    let (a, b) = splitHorizontallyBy f (mirrorRect r)
    in (mirrorRect a, mirrorRect b)

-- | Transpose a rectangle about the leading diagonal.
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) =
  Rectangle (fromIntegral ry) (fromIntegral rx) rh rw

--------------------------------------------------------------------------------
-- Messages

-- | Messages that resize the master area.
data Resize = Shrink | Expand deriving (Eq, Show)
instance Message Resize

-- | Message that changes how many windows occupy the master area.
newtype IncMasterN = IncMasterN Int deriving (Eq, Show)
instance Message IncMasterN

-- | Messages that switch between layouts.
data ChangeLayout = NextLayout | FirstLayout deriving (Eq, Show)
instance Message ChangeLayout

--------------------------------------------------------------------------------
-- Layouts

-- | The classic two-pane tiling algorithm.
tile
  :: Rational   -- ^ fraction of the screen given to the master pane
  -> Rectangle
  -> Int        -- ^ number of windows in the master pane
  -> Int        -- ^ total number of windows
  -> [Rectangle]
tile f r nmaster n
  | n <= nmaster || nmaster == 0 = splitVertically n r
  | otherwise = splitVertically nmaster r1 ++ splitVertically (n - nmaster) r2
  where (r1, r2) = splitHorizontallyBy f r

data Tall a = Tall
  { tallNMaster        :: !Int
  , tallRatioIncrement :: !Rational
  , tallRatio          :: !Rational
  } deriving (Show, Read)

instance LayoutClass Tall a where
  pureLayout (Tall nmaster _ frac) r s = zip ws rs
    where
      ws = W.integrate s
      rs = tile frac r nmaster (length ws)

  pureMessage (Tall nmaster delta frac) m =
    msum [fmap resize (fromMessage m), fmap incmastern (fromMessage m)]
    where
      resize Shrink = Tall nmaster delta (max 0 (frac - delta))
      resize Expand = Tall nmaster delta (min 1 (frac + delta))
      incmastern (IncMasterN d) = Tall (max 0 (nmaster + d)) delta frac

  description _ = "Tall"

-- | Every window gets the whole screen; only the focused one is placed, so the
-- rest are hidden by the render sequence.
data Full a = Full deriving (Show, Read)

instance LayoutClass Full a

-- | Transpose another layout.
newtype Mirror l a = Mirror (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Mirror l) a where
  runLayout (W.Workspace i (Mirror l) ms) r = do
    (rs, ml') <- runLayout (W.Workspace i l ms) (mirrorRect r)
    pure (map (fmap mirrorRect) rs, Mirror <$> ml')
  handleMessage (Mirror l) = fmap (fmap Mirror) . handleMessage l
  description (Mirror l) = "Mirror " ++ description l

--------------------------------------------------------------------------------
-- Choosing between layouts

-- | Which side of a 'Choose' is active.
data LR = L | R deriving (Eq, Read, Show)

data Choose l r a = Choose !LR (l a) (r a) deriving (Read, Show)

infixr 5 |||

-- | Compose two layouts, switched between with 'NextLayout'.
(|||) :: l a -> r a -> Choose l r a
(|||) = Choose L

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
  runLayout (W.Workspace i (Choose L l r) ms) rect = do
    (rs, ml') <- runLayout (W.Workspace i l ms) rect
    pure (rs, (\l' -> Choose L l' r) <$> ml')
  runLayout (W.Workspace i (Choose R l r) ms) rect = do
    (rs, mr') <- runLayout (W.Workspace i r ms) rect
    pure (rs, Choose R l <$> mr')

  description (Choose L l _) = description l
  description (Choose R _ r) = description r

  handleMessage c@(Choose d l r) m
    | Just NextLayout <- fromMessage m = Just <$> swap c
    | Just FirstLayout <- fromMessage m = case d of
        L -> pure Nothing
        R -> Just <$> swap c
    | otherwise = case d of
        L -> fmap (\l' -> Choose L l' r) <$> handleMessage l m
        R -> fmap (Choose R l) <$> handleMessage r m

-- | Move to the other branch, telling the outgoing one to hide.
swap :: (LayoutClass l a, LayoutClass r a) => Choose l r a -> X (Choose l r a)
swap (Choose d l r) = case d of
  L -> do
    ml' <- handleMessage l (SomeMessage Hide)
    pure (Choose R (fromMaybe l ml') r)
  R -> do
    mr' <- handleMessage r (SomeMessage Hide)
    pure (Choose L l (fromMaybe r mr'))
