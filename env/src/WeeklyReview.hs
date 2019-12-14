-- | Bring up emacs session recording a weekly review and reminding of
-- priorities.
module WeeklyReview where

import Imports
import Data.List (isSuffixOf, stripPrefix, sort)
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import Safe (lastMay)
import System.IO (hPutStrLn)
import qualified Data.ByteString as BS

weeklyReview :: Xio ()
weeklyReview = do
  homeDir <- view envHomeDir
  let weeklyDir = homeDir </> "docs/weekly"
      dailyDir = homeDir </> "docs/daily"
  dateString <- liftIO currentDateString
  prioritiesFile <- liftIO $ findFileWithSuffixIn "priorities.md" weeklyDir
  templateFile <- liftIO $ findFileWithSuffixIn "template.md" weeklyDir
  lastWeekly <- liftIO $ lastMay <$> listDatedMarkdownFiles weeklyDir
  weekStart <- case lastWeekly of
    Nothing -> return $ ModifiedJulianDay 0
    Just (day, _) -> return day
  withRunInIO $ \runInIO -> do
    withSystemTempFile "catenated-dailies.md" $ \catPath catHandle -> do
      dailiesSinceLastWeekly <-
        filter ((>= weekStart) . fst) <$> listDatedMarkdownFiles dailyDir
      runInIO $ logInfo $ "dailies: " <> fromString (show dailiesSinceLastWeekly)
      forM_ dailiesSinceLastWeekly $ \(dailyDate, dailyPath) -> do
        hPutStrLn catHandle ("# " ++ formatDate dailyDate ++ "\n")
        BS.hPut catHandle =<< BS.readFile dailyPath
        hPutStrLn catHandle ""
      runInIO $ syncSpawn "emacs"
        [ prioritiesFile
        -- FIXME , catPath
        -- FIXME , snd <$> lastWeekly
        , weeklyDir </> dateString <.> "md"
        , "-f", "delete-other-windows"
        , "-f", "split-window-right"
        , "-insert", templateFile
        , "-f", "evil-next-line"
        , "-f", "evil-next-line"
        , "-f", "evil-insert"
        ]

dailyReview :: Xio ()
dailyReview = do
  homeDir <- view envHomeDir
  let weeklyDir = homeDir </> "docs/weekly"
      dailyDir = homeDir </> "docs/daily"
  dateString <- liftIO currentDateString
  prioritiesFile <- liftIO $ findFileWithSuffixIn "priorities.md" weeklyDir
  syncSpawn "emacs"
    [ prioritiesFile
    , dailyDir </> dateString <.> "md"
    , "-f", "delete-other-windows"
    , "-f", "split-window-right"
    , "-f", "evil-insert"
    ]

listDatedMarkdownFiles :: FilePath -> IO [(Day, FilePath)]
listDatedMarkdownFiles dir =
  mapMaybe (nameToDay . takeFileName) . sort <$> listDirectory dir
 where
  nameToDay name = do
    withoutExtension <- stripSuffix ".md" name
    day <- parseTimeM False defaultTimeLocale dateFormat withoutExtension
    return (day, dir </> name)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix xs = fmap reverse (stripPrefix (reverse suffix) (reverse xs))

findFileWithSuffixIn :: String -> FilePath -> IO FilePath
findFileWithSuffixIn suffix dir = do
  entries <- sort <$> listDirectory dir
  let foundFileName = last $ filter ((suffix `isSuffixOf`) . takeFileName) entries
  return $ dir </> foundFileName

currentDateString :: IO String
currentDateString = formatTime defaultTimeLocale dateFormat <$> getZonedTime

formatDate :: Day -> String
formatDate = formatTime defaultTimeLocale dateFormat

dateFormat :: String
dateFormat = "%0Y-%m-%d"
