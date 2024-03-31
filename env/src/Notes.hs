module Notes where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Time.Format(formatTime, defaultTimeLocale)
import Data.Time.LocalTime(getZonedTime)
import Data.Tuple (Solo(MkSolo))
import Imports
import Misc (notify, notifyTruncated)
import Prompt
import Text.Regex.PCRE.Rex

addNote :: FilePath -> XX ()
addNote path =
  plainPrompt ("Add to " ++ path ++ ": ") $ \content -> do
    timeAndFocusContext <- getTimeAndFocusContext
    forkXio $ appendNote path $ unlines
      [ ""
      , timeAndFocusContext ++ ":"
      , "  " ++ content
      ]

addNoteWithClipboard :: FilePath -> XX ()
addNoteWithClipboard path =
  plainPrompt ("Add to " ++ path ++ " (with clipboard): ") $ \content -> do
    timeAndFocusContext <- getTimeAndFocusContext
    clipboardLines <- lines <$> getClipboard
    forkXio $ appendNote path $ unlines $
      [ ""
      , "* " ++ timeAndFocusContext ++ ":"
      ]
      ++ (if all isSpace content then ["  " ++ content] else [])
      ++ map ("  > " ++) clipboardLines

-- TODO: Ideally this would be idempotent, but it is not
addContextToClipboard :: XX ()
addContextToClipboard = do
  timeAndFocusContext <- getTimeAndFocusContext
  clipboardLines <- lines <$> getClipboard
  let content = unlines $ [timeAndFocusContext ++ ":", ""] ++ map ("> " ++) clipboardLines
  spawnAndNotifyFailWithInput "xclip" [] content
  forkXio $ notifyTruncated 300 $ "Copied: " ++ content

getTimeAndFocusContext :: XX String
getTimeAndFocusContext = do
  -- TODO: What if this was done for the title of all visible windows?
  maybeTitle <- runQuery title
  let focusContext = case maybeTitle of
        Nothing -> ""
        Just ([rex|^(?{}.+) \s+ - \s+ (?{}\S+) \s+ - \s+ Google \s Chrome$|] -> Just (pageTitle, url)) ->
          -- TODO: escape title?
          "While browsing [" ++ pageTitle ++ "](" ++ url ++ ")"
        Just ([rex|^(?{}\S+) \s+ - \s+ Google \s Chrome$|] -> Just (MkSolo url)) ->
          "While browsing " ++ url
        Just ([rex|^(?{}\S+) \s+ - \s+ Emacs$|] -> Just (MkSolo path)) ->
          "While editing file://" ++ path
        Just ([rex|^(?{}.+) \s+ - \s+ obsidian \s+ - \s+ Obsidian \s+ v[0-9\.]+|] -> Just (MkSolo name)) ->
          "With focus on [[" ++ name ++ "]]"
        Just windowTitle -> "With focus on '" ++ windowTitle ++ "'"
  timeContext <- formatTime defaultTimeLocale "[[%Y-%m-%d]] %T" <$> liftIO getZonedTime
  return $ timeContext ++ " " ++ focusContext

getClipboard :: XX String
getClipboard = do
  mx <- runXio $ timeout (100 * 1000) $ syncSpawnAndRead "xclip" ["-o"]
  case mx of
    Nothing -> do
      forkXio $ notify "xclip timed out"
      return "xclip timed out"
    Just x -> return (trim x)

appendNote :: FilePath -> String -> Xio ()
appendNote path content = do
  liftIO $ appendFile path content
  notifyTruncated 300 $ "Appended the following: " ++ content

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
