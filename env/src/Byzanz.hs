-- | Prompt for running byzanz
module Byzanz where

import Data.Time
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
      now <- liftIO (utcToLocalZonedTime =<< getCurrentTime)
      homeDir <- view envHomeDir
      let name = formatTime defaultTimeLocale "%F_%T.gif" now
          output = homeDir </> "pics/screencaps" </> name
      removeFile output `catchAny` \_ -> return ()
      syncSpawn (homeDir </> "env/byzanz-record-region.sh") [args', output]
      spawn browser [output]
