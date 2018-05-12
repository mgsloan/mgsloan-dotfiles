module Constants where

urxvtArgs :: [String]
urxvtArgs = ["-fg", "lightgrey", "-bg", "black", "+sb"]

terminalSh :: String
terminalSh = unwords (terminalCmd : terminalArgs)

terminalCmd :: String
terminalCmd = "urxvt"

terminalArgs :: [String]
terminalArgs = ["-e", "tmux"]

browser, emacs :: String
browser = "google-chrome"
emacs = "alacritty -e emacs -nw"

workspaceNames :: [String]
workspaceNames = map show $ [1..9 :: Int] ++ [0]

phi :: Rational
phi = 0.61803
