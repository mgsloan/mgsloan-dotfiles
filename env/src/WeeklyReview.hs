-- | Bring up emacs session recording a weekly review and reminding of
-- priorities.
module WeeklyReview where

import Imports
import Data.List (isSuffixOf, sort)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

weeklyReview :: Xio ()
weeklyReview = do
  homeDir <- view envHomeDir
  let weeklyDir = homeDir </> "docs/weekly"
  dateString <- liftIO getDateString
  prioritiesFile <- liftIO $ findFileWithSuffixIn "priorities.md" weeklyDir
  templateFile <- liftIO $ findFileWithSuffixIn "template.md" weeklyDir
  spawn "emacs"
    [ prioritiesFile
    , weeklyDir </> dateString <.> "md"
    , "-f", "delete-other-windows"
    , "-f", "split-window-right"
    , "-insert", templateFile
    ]

findFileWithSuffixIn :: String -> FilePath -> IO FilePath
findFileWithSuffixIn suffix weeklyDir = do
  entries <- sort <$> listDirectory weeklyDir
  let foundFileName = last $ filter ((suffix `isSuffixOf`) . takeFileName) entries
  return $ weeklyDir </> foundFileName

getDateString :: IO String
getDateString = formatTime defaultTimeLocale "%0Y-%m-%d" <$> getZonedTime

dailyReview :: Xio ()
dailyReview = do
  homeDir <- view envHomeDir
  let weeklyDir = homeDir </> "docs/weekly"
      dailyDir = homeDir </> "docs/daily"
  dateString <- liftIO getDateString
  prioritiesFile <- liftIO $ findFileWithSuffixIn "priorities.md" weeklyDir
  spawn "emacs"
    [ prioritiesFile
    , dailyDir </> dateString <.> "md"
    , "-f", "delete-other-windows"
    , "-f", "split-window-right"
    , "-f", "evil-insert"
    ]
