-- | Prompt for running byzanz
module Byzanz where

import UnliftIO.Directory
import XMonad.Prompt

import Imports
import Prompt

data ByzanzPrompt = ByzanzPrompt

instance XPrompt ByzanzPrompt where
  showXPrompt ByzanzPrompt = "Byzanz arguments: "

byzanzPrompt :: XX ()
byzanzPrompt = do
  env <- ask
  toXX $ mkXPrompt ByzanzPrompt xpconfig (const $ return []) $ \args -> do
    let args' = if null args then "10" else args
    withEnv env $ forkXio $ do
      let output = "/tmp/recorded.gif"
      liftIO $ removeFile output `catchAny` \_ -> return ()
      homeDir <- view envHomeDir
      syncSpawn (homeDir </> "env/byzanz-record-region.sh") [args', output]
      spawn browser [output]
