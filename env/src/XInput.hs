module XInput where

import XMonad
import qualified XMonad.Util.ExtensibleState as State

import Misc

data TouchMode = Inactive | Normal
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
setTouch x = spawn $
  "xinput set-prop 'SynPS/2 Synaptics TouchPad' 'Device Enabled' " ++
  case x of
    Inactive -> "0"
    Normal -> "1"
