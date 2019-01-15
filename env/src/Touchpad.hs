module Touchpad where

import qualified XMonad.Util.ExtensibleState as State

import Imports
import Misc

data TouchpadMode = TouchpadInactive | TouchpadNormal
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass TouchpadMode where
  initialValue = TouchpadInactive
  extensionType = PersistentExtension

touchpadToggle :: XX ()
touchpadToggle = do
  x <- toXX State.get
  let x' = nxt x
  setTouchpad x'
  toXX $ State.put x'

setTouchpad :: (MonadIO m, MonadReader Env m) => TouchpadMode -> m ()
setTouchpad x =
  spawn
    "xinput"
    [ "set-prop"
    , "SynPS/2 Synaptics TouchPad"
    , "Device Enabled"
    , case x of
        TouchpadInactive -> "0"
        TouchpadNormal -> "1"
    ]
