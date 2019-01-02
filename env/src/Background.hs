-- | Selects and displays a random desktop background
module Background where

import System.Directory (doesDirectoryExist)
import System.Process (spawnProcess)
import System.Random (randomRIO)
import Path
import Path.IO (listDirRecur)

import Imports

randomBackground :: MX ()
randomBackground =
  liftIO $ setRandomBackground "/home/mgsloan/env/backgrounds"

setRandomBackground :: FilePath -> IO ()
setRandomBackground dir = void $ forkIO $ do
  exists <- doesDirectoryExist dir
  if exists
    then do
      (_, files) <- listDirRecur =<< parseAbsDir dir
      let jpgs = filter ((".jpg" ==) . fileExtension) files
      randomJpg <- randomIndex jpgs
      void $ spawnProcess "feh" ["--bg-scale", toFilePath randomJpg]
    else putStrLn $ "Not changing background, because " ++ show dir ++ " does not exist."

randomIndex :: [a] -> IO a
randomIndex xs = (xs !!) <$> randomRIO (0, length xs - 1)
