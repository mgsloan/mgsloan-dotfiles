module Prompt where

import XMonad.Prompt
import qualified Data.Map as M
import qualified XMonad.Prompt.Shell as Shell

import Imports

shellPrompt :: MX ()
shellPrompt = do
  cmds <- io Shell.getCommands
  let config = xpconfig True
      completion = Shell.getShellCompl cmds $ searchPredicate config
  env <- ask
  toMX $ mkXPrompt Shell.Shell config completion $ \input ->
    withEnv env $ spawn "sh" ["-c", input]

xpconfig :: Bool -> XPConfig
xpconfig auto
    | auto = res { autoComplete = Just 1000 }
    | otherwise = res
  where
    res = def
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
    km =
      M.insert (controlMask, xK_Right) (moveWord Next) $
      M.insert (controlMask, xK_Left) (moveWord Prev) $
      emacsLikeXPKeymap
