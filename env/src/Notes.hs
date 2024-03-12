module Notes where

import Data.Time.Format(formatTime, defaultTimeLocale)
import Data.Time.LocalTime(getZonedTime)
import Data.Tuple (Solo(MkSolo))
import Imports
import Prompt
import Text.Regex.PCRE.Rex
import XMonad.Prompt

addNote :: FilePath -> XX ()
addNote path = do
  xpConfig <- getXpConfig
  env <- ask
  toXX $ mkXPrompt (GenericPrompt ("Add to " ++ path ++ ": ")) xpConfig (const $ return []) $ \content -> withEnv env $ do
    maybeTitle <- runQuery title
    let focusContext = case maybeTitle of
          Nothing -> ""
          Just ([rex|^(?{}.+) \s+ - \s+ (?{}\S+) \s+ - \s+ Google \s Chrome$|] -> Just (pageTitle, url)) ->
            -- TODO: escape title?
            "While browsing [" ++ pageTitle ++ "](" ++ url ++ ")"
          Just ([rex|^(?{}\S+) \s+ - \s+ Google \s Chrome$|] -> Just (MkSolo url)) ->
            "While browsing " ++ url
          Just windowTitle -> "With focus on '" ++ windowTitle ++ "'"
    timeContext <- formatTime defaultTimeLocale "[[%Y-%m-%d]] %T" <$> liftIO getZonedTime
    liftIO $ appendFile path ("\n* " ++ timeContext ++ " " ++ focusContext ++ ":\n  " ++ content ++ "\n")
