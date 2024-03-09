module Notes where

import Data.Tuple (Solo(MkSolo))
import Text.Regex.PCRE.Rex
import XMonad.Prompt
import Imports
import Prompt
import Misc

addNote :: FilePath -> XX ()
addNote path = do
  xpConfig <- getXpConfig
  env <- ask
  toXX $ mkXPrompt (GenericPrompt ("Add to " ++ path ++ ": ")) xpConfig (const $ return []) $ \content -> withEnv env $ do
    maybeTitle <- runQuery title
    forkXio $ notify $ "title = " ++ show maybeTitle
    let context = case maybeTitle of
          Nothing -> ""
          Just ([rex|^(?{}.+) \s+ - \s+ (?{}\S+) \s+ - \s+ Google \s Chrome$|] -> Just (pageTitle, url)) ->
            -- TODO: escape title?
            "While browsing [" ++ pageTitle ++ "](" ++ url ++ ")"
          Just ([rex|^(?{}\S+) \s+ - \s+ Google \s Chrome$|] -> Just (MkSolo url)) ->
            "While browsing " ++ url
          Just windowTitle -> "With focus on '" ++ windowTitle ++ "'"
    liftIO $ appendFile path ("\n* " ++ context ++ ":\n  " ++ content ++ "\n")
