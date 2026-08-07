-- | Urgency hints.
module XMonad.Hooks.UrgencyHook
  ( doAskUrgent
  , withUrgencyHook
  , focusUrgent
  ) where

import Data.Monoid (Endo)

import XMonad.Core

-- | Mark a window urgent rather than focusing it.
--
-- Not implemented: river's window management protocol has no urgency concept,
-- and Wayland has no @_NET_WM_STATE_DEMANDS_ATTENTION@. Since river also does
-- not focus windows on activation, the behaviour this config wanted — do not
-- yank me away from my workspace — holds anyway; what is missing is only the
-- visual hint that something wants attention.
doAskUrgent :: Query (Endo s)
doAskUrgent = do
  liftQuery $ warnUnimplemented "doAskUrgent"
    "Wayland has no urgency hint, so no visual indication is given. Focus is \
    \not stolen either way."
  pure mempty

withUrgencyHook :: h -> XConfig a -> XConfig a
withUrgencyHook _ = id

focusUrgent :: X ()
focusUrgent = warnUnimplemented "focusUrgent"
  "No urgency state is tracked, so there is never an urgent window to focus."
