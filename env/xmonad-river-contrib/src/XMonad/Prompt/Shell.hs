-- | Shell command completion for prompts.
module XMonad.Prompt.Shell
  ( Shell(..)
  , shellPrompt
  , getCommands
  , getShellCompl
  ) where

import Control.Monad (filterM)
import Data.List (nub, sort)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (lookupEnv)

import XMonad.Core
import XMonad.Prompt

data Shell = Shell

instance XPrompt Shell where
  showXPrompt Shell = "Run: "
  completionToCommand _ = id

shellPrompt :: XPConfig -> X ()
shellPrompt c = do
  cmds <- io getCommands
  mkXPrompt Shell c (getShellCompl cmds (searchPredicate c)) spawnShell
  where
    spawnShell cmd = warnUnimplemented "XMonad.Prompt.Shell.shellPrompt"
      ("Would have run: " ++ cmd ++ ". This config supplies its own spawn, \
       \so this default path is unused.")

-- | Every executable on @PATH@.
--
-- Unchanged from the X11 version: this only ever read the filesystem.
getCommands :: IO [String]
getCommands = do
  p <- maybe "/usr/local/bin:/usr/bin:/bin" id <$> lookupEnv "PATH"
  let dirs = splitOn ':' p
  existing <- filterM doesDirectoryExist dirs
  entries <- concat <$> mapM getDirectoryContents existing
  pure (sort (nub (filter (`notElem` [".", ".."]) entries)))
  where
    splitOn c s = case break (== c) s of
      (w, [])     -> [w]
      (w, _:rest) -> w : splitOn c rest

getShellCompl :: [String] -> (String -> String -> Bool) -> String -> IO [String]
getShellCompl cmds p s
  | null s = pure cmds
  | otherwise = pure (filter (p s) cmds)
