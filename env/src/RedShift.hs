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
  -- TODO: Ideally this brightness would affect the laptop screen
  -- settings to save power, *unless* an external monitor is plugged
  -- in. For now preferring automatic brightness of external.
  spawn "redshift" ["-l", "47:-120", "-t", "6500:3700", "-b", "1:0.4", "-r"]
redShiftUpdate RedShiftDisabled =
  spawn "killall" ["redshift"]
