module RedShift
  ( startRedShift
  , cycleRedShift
  ) where

import qualified XMonad.Util.ExtensibleState as State

import Imports
import Misc

data RedShift = RedShiftEnabled | RedShiftDisabled
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass RedShift where
  initialValue = RedShiftEnabled
  extensionType = PersistentExtension

startRedShift :: XX ()
startRedShift = updateRedShift =<< toXX State.get

cycleRedShift :: XX ()
cycleRedShift = do
  x <- toXX State.get
  let x' = nxt x
  updateRedShift x'
  toXX $ State.put x'

updateRedShift :: (MonadIO m, MonadReader Env m) => RedShift -> m ()
updateRedShift RedShiftEnabled = do
  -- TODO: make this configurable
  spawn "redshift" ["-l", "47:-120", "-t", "6500:3700", "-r"]
updateRedShift RedShiftDisabled = do
  spawn "killall" ["redshift"]
