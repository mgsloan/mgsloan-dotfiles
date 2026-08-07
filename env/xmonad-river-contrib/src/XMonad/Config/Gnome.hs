-- | GNOME session integration.
module XMonad.Config.Gnome (gnomeConfig, gnomeRegister) where

import XMonad.Core

gnomeConfig :: XConfig a -> XConfig a
gnomeConfig = id

-- | Register with a GNOME session manager.
--
-- Not implemented, and unlikely to be: this spoke the XSMP session management
-- protocol over X11. The modern equivalent under a Wayland compositor is to
-- let systemd own the session, which belongs in the session startup script
-- rather than in the window manager. Note that @graphical-session.target@
-- cannot be started directly — it sets @RefuseManualStart@ — so the session
-- starts @river-session.target@, which pulls it in via @BindsTo@.
gnomeRegister :: X ()
gnomeRegister = warnUnimplemented "gnomeRegister"
  "XSMP session registration is X11-only. river/init starts \
  \river-session.target instead, which is already handled."
