module Logs where

import Imports

showLogsOfFocusedWindow :: XX ()
showLogsOfFocusedWindow = do
  mpid <- getPidOfFocus
  case mpid of
    Nothing -> return ()
    Just pid -> forkXio $ do
      allPids <- (pid :) <$> getParentPids pid
      spawn terminalCmd $ terminalArgs ++ ["new-session", unwords $
        ["bash -c \"journalctl --boot --follow"]
        ++ map (\p -> "_PID=" ++ show p) allPids ++
        ["| ccze -A | less -R\""]]
