module Tmux where

import Imports

-- | Creates a new named tmux session an a particular workspace, and
-- runs the specified command within a subshell (see
-- 'interactiveShellCommand').
spawnOrReplaceInteractiveTmuxShellOn :: String -> String -> String -> Xio ()
spawnOrReplaceInteractiveTmuxShellOn workspace name cmd = do
  killTmuxSession name
  spawnOn workspace terminalCmd $
    [ "-e", "tmux", "new-session", "-s", name, "-n", name
    , interactiveShellCommand cmd
    ]

-- | Creates a bash invocation which runs the specified command in a
-- shell which does not exit when the program exits.
--
-- The command will also be added to the bash history, so after
-- exiting, you can press the up arrow in bash to get the command.
interactiveShellCommand:: String -> String
interactiveShellCommand cmd =
  "bash --init-file <(echo " ++ stringToBashLiteral cmdWithHistory ++ ")"
  where
    cmdWithHistory =
      "history -s " ++ stringToBashLiteral cmd ++ "; " ++ cmd

killTmuxSession :: String -> Xio ()
killTmuxSession name =
  void $ tryAny $ spawn "tmux" ["kill-session", "-t", name]
