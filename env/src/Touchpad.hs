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
    "synclient"
    [ "TouchPadOff=" ++
      case x of
        TouchpadInactive -> "1"
        TouchpadNormal -> "0"
    ]
