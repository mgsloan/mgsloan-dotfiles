module Prompt where

import XMonad.Prompt
import qualified Data.Map as M
import qualified XMonad.Prompt.Shell as Shell

import Imports

shellPrompt :: XX ()
shellPrompt = do
  cmds <- io Shell.getCommands
  let completion = Shell.getShellCompl cmds $ searchPredicate xpconfig
  env <- ask
  toXX $ mkXPrompt Shell.Shell xpconfig completion $ \input ->
    withEnv env $ spawn "sh" ["-c", input]

xpconfig :: XPConfig
xpconfig = def
  { font              = "xft:Hack:pixelsize=18"
  , bgColor           = "black"
  , fgColor           = "white"
  , bgHLight          = "gray"
  , fgHLight          = "black"
  , borderColor       = "orange"
  , promptBorderWidth = 1
  , position          = Bottom
  , height            = 32
  , historySize       = 1000
  , promptKeymap      = km
  }
  where
    km =
      M.insert (controlMask, xK_Right) (moveWord Next) $
      M.insert (controlMask, xK_Left) (moveWord Prev) $
      emacsLikeXPKeymap
