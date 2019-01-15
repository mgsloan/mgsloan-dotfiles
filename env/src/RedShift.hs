module RedShift
  ( redShiftStart
  , redShiftToggle
  ) where

import qualified XMonad.Util.ExtensibleState as State

import Imports
import Misc

data RedShift = RedShiftEnabled | RedShiftDisabled
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass RedShift where
  initialValue = RedShiftEnabled
  extensionType = PersistentExtension

redShiftStart :: XX ()
redShiftStart = redShiftUpdate =<< toXX State.get

redShiftToggle :: XX ()
redShiftToggle = do
  x <- toXX State.get
  let x' = nxt x
  redShiftUpdate x'
  toXX $ State.put x'

redShiftUpdate :: (MonadIO m, MonadReader Env m) => RedShift -> m ()
redShiftUpdate RedShiftEnabled =
  spawn "redshift" ["-l", "47:-120", "-t", "6500:3700", "-r"]
redShiftUpdate RedShiftDisabled =
  spawn "killall" ["redshift"]
