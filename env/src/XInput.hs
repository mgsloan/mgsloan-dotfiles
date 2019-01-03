module XInput where

import XMonad
import qualified XMonad.Util.ExtensibleState as State

import Misc
import Monad

data TouchMode = Inactive | Normal
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass TouchMode where
  initialValue = Inactive
  extensionType = PersistentExtension

cycleTouch :: MX ()
cycleTouch = do
  x <- toMX State.get
  let x' = nxt x
  setTouch x'
  toMX $ State.put x'

setTouch :: TouchMode -> MX ()
setTouch x = spawn $
  "xinput set-prop 'SynPS/2 Synaptics TouchPad' 'Device Enabled' " ++
  case x of
    Inactive -> "0"
    Normal -> "1"
