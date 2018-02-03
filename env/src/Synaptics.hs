-- Could be made into an xmonad-contrib extension, but this seems pretty
-- idiosyncratic, so maybe not.
module Synaptics where

import XMonad
import qualified XMonad.Util.ExtensibleState as State

import Misc

data TouchMode = Inactive {- | Scroll -} | Normal
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass TouchMode where
  initialValue = Inactive
  extensionType = PersistentExtension

cycleTouch :: X ()
cycleTouch = do
  x <- State.get
  let x' = nxt x
  setTouch x'
  State.put x'

setTouch :: TouchMode -> X ()
setTouch Inactive = spawn "synclient TouchpadOff=1"
--TODO: fix this one.
--setTouch Scroll = spawn $ unwords
--  [ "synclient TouchpadOff=2 HorizTwoFingerScroll=1"
--  , "TapButton1=0 TapButton2=0 ClickFinger1=0 ClickFinger2=0"
--  , "LeftEdge=0 RightEdge=1 TopEdge=0 BottomEdge=1"
--  ]
setTouch Normal = spawn $ unwords
  [ "synclient TouchpadOff=2 HorizTwoFingerScroll=0"
  , "TapButton1=1 TapButton2=3 ClickFinger1=1 ClickFinger2=1"
  , "LeftEdge=1751 RightEdge=5191 TopEdge=1624 BottomEdge=4282"
  ]
