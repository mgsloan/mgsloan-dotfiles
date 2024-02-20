module Prompt where

import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import XMonad.Prompt
import qualified Data.Map as M
import qualified XMonad.Prompt.Shell as Shell

import Imports
import Misc

data GenericPrompt = GenericPrompt String

instance XPrompt GenericPrompt where
  showXPrompt (GenericPrompt x) = x

shellPrompt :: XX ()
shellPrompt = do
  cmds <- io Shell.getCommands
  let completion = Shell.getShellCompl cmds $ searchPredicate xpconfig
  env <- ask
  toXX $ mkXPrompt Shell.Shell xpconfig completion $ \input ->
    withEnv env $ spawn "sh" ["-c", input]

data ActionsPrompt = ActionsPrompt

instance XPrompt ActionsPrompt where
  showXPrompt ActionsPrompt = "M-x "

actionPrompt :: M.Map String (XX ()) -> XX ()
actionPrompt actions = do
  let completion = mkComplFunFromList' xpconfig (M.keys actions)
  env <- ask
  toXX $ mkXPrompt ActionsPrompt xpconfig completion $ \input ->
    withEnv env $
      case M.lookup input actions of
        Nothing -> forkXio $ notify $ "No action matching " <> input
        Just action -> do
          logDebug $ "Running action " <> fromString input
          action
          logDebug $ "Finished running action " <> fromString input

xpconfig :: XPConfig
xpconfig = def
  { font              = if isHiDpi
                          then "xft:Hack:pixelsize=18"
                          else "xft:Hack:pixelsize=10"
  , bgColor           = "black"
  , fgColor           = "white"
  , bgHLight          = "gray"
  , fgHLight          = "black"
  , borderColor       = "orange"
  , promptBorderWidth = 1
  , position          = Bottom
  , height            = if isHiDpi then 32 else 18
  , historySize       = 1000
  , promptKeymap      = km
  }
  where
    km =
      M.insert (controlMask, xK_Right) (moveWord Next) $
      M.insert (controlMask, xK_Left) (moveWord Prev) $
      -- C-y makes no sense for paste??
      M.delete (controlMask, xK_y) $
      M.insert (controlMask, xK_v) pasteString $
      M.insert (mod4Mask, xK_v) pasteString $
      emacsLikeXPKeymap

isHiDpi :: Bool
isHiDpi = isJust $ unsafePerformIO $ lookupEnv "HIDPI"
