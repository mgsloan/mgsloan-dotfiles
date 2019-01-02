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

startRedShift :: MX ()
startRedShift = updateRedShift =<< toMX State.get

cycleRedShift :: MX ()
cycleRedShift = do
  x <- toMX State.get
  let x' = nxt x
  updateRedShift x'
  toMX $ State.put x'

updateRedShift :: RedShift -> MX ()
updateRedShift RedShiftEnabled = do
  -- TODO: make this configurable
  spawn "redshift" ["-l", "47:-120", "-t", "6500:3700", "-r"]
updateRedShift RedShiftDisabled = do
  spawn "killall" ["redshift"]
