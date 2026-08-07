-- | Distinguishes the first run of a session from a restart.
--
-- Under xmonad this reads a flag out of the resume state handed to the new
-- process. Under river the window manager is restarted by exec, so the flag
-- travels in an environment variable instead — set on the way out, read and
-- cleared on the way in.
module XMonad.Util.SessionStart
  ( isSessionStart
  , setSessionStarted
  ) where

import System.Environment (lookupEnv, setEnv)

import XMonad.Core

sessionVar :: String
sessionVar = "XMONAD_RIVER_SESSION_STARTED"

-- | 'True' on the first run of a session, 'False' after a restart.
isSessionStart :: X Bool
isSessionStart = io ((== Nothing) <$> lookupEnv sessionVar)

-- | Mark the session as started, so that a subsequent restart is recognised as
-- one. The variable is inherited by the exec'd successor and by every child
-- process, which is harmless.
setSessionStarted :: X ()
setSessionStarted = io (setEnv sessionVar "1")
