-- | EWMH desktop integration.
--
-- EWMH is an X11 convention built on root window properties, and has no
-- Wayland counterpart. The pieces of it this config actually uses map onto
-- river as follows:
--
-- * Publishing the workspace list and current desktop, so panels can show
--   them, has no river equivalent — a Wayland status bar reads
--   @ext-workspace-v1@ or talks to the window manager directly.
-- * @_NET_ACTIVE_WINDOW@ — a client asking to be focused — /does/ have a
--   counterpart in @xdg-activation-v1@, which river handles. It does not
--   currently surface activation requests to the window manager, so
--   'setEwmhActivateHook' cannot be honoured yet.
module XMonad.Hooks.EwmhDesktops
  ( ewmh
  , ewmhFullscreen
  , setEwmhActivateHook
  ) where

import XMonad.Core

-- | Pass-through. Nothing to publish without root window properties.
ewmh :: XConfig a -> XConfig a
ewmh = id

ewmhFullscreen :: XConfig a -> XConfig a
ewmhFullscreen = id

-- | Set the action taken when a client asks to be activated.
--
-- Not implemented. This config uses it to /prevent/ Chrome yanking focus at
-- startup, marking the window urgent instead. Under river that request is
-- handled by the compositor without consulting the window manager, so the
-- hook never runs — but the good news is that river does not steal focus on
-- activation either, so the outcome is closer to what this config wanted than
-- to the X11 default it was working around.
setEwmhActivateHook :: ManageHook -> XConfig a -> XConfig a
setEwmhActivateHook _ conf = conf
  { startupHook = do
      warnUnimplemented "setEwmhActivateHook"
        "Activation requests are not surfaced by river's window management \
        \protocol, so this hook never runs. river does not focus windows on \
        \activation, so nothing steals focus regardless."
      startupHook conf
  }
