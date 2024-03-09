module Constants where

import Prelude

phi :: Rational
phi = 0.61803

terminalCmd :: String
terminalCmd = "alacritty"

terminalArgs :: [String]
terminalArgs = ["-e", "tmux"]

browser, emacs :: String
browser = "google-chrome"
emacs = "emacs"

workspaceNames :: [String]
workspaceNames = map show $ [1..9 :: Int] ++ [0]

-- Note that stderr-prefix is added via my patch
-- https://github.com/systemd/systemd/pull/11336
systemdCatArgs :: [String]
systemdCatArgs = ["--level-prefix=false", "--stderr-priority=err"]

systemdCatStderrInfoArgs :: [String]
systemdCatStderrInfoArgs = ["--level-prefix=false"]
