-- | Utilities to aid in focusing on tasks.

module Focus where

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as State
import Imports

data WorkspaceSwitchLock = SwitchingNotLocked | SwitchingLocked
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass WorkspaceSwitchLock where
  initialValue = SwitchingNotLocked
  extensionType = PersistentExtension

focusWorkspace :: String -> XX ()
focusWorkspace workspaceName = do
  x <- toXX $ State.get
  case x of
    SwitchingNotLocked -> toXX $ windows (W.greedyView workspaceName)
    SwitchingLocked -> spawn "notify-send" ["-t", "1000", "FOCUS!"]

lockWorkspaceSwitching :: XX ()
lockWorkspaceSwitching = toXX $ State.put SwitchingLocked

unlockWorkspaceSwitching :: XX ()
unlockWorkspaceSwitching = toXX $ State.put SwitchingNotLocked
