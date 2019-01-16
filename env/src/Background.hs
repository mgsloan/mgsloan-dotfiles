-- | Selects and displays a random desktop background
module Background
  ( randomBackground
  , updateBackgrounds
  ) where

import System.Directory (doesDirectoryExist)
import Path (parseAbsDir, fileExtension, toFilePath)
import Path.IO (listDirRecur)
import qualified Data.Vector as V

import Imports
import Misc

randomBackground :: Xio ()
randomBackground = do
  backgrounds <- ensureBackgrounds
  result <- liftIO $ randomIndex backgrounds
  spawn "feh" ["--bg-scale", result]

ensureBackgrounds :: Xio (V.Vector FilePath)
ensureBackgrounds = do
  backgroundsVar <- view envBackgroundsVar
  mbackgrounds <- readMVar backgroundsVar
  case mbackgrounds of
    Nothing -> updateBackgrounds
    Just backgrounds -> return backgrounds

updateBackgrounds :: Xio (V.Vector FilePath)
updateBackgrounds = do
  homeDir <- view envHomeDir
  backgroundsVar <- view envBackgroundsVar
  modifyMVar backgroundsVar $ \oldValue -> do
    let backgroundsDir = homeDir </> "env/untracked/backgrounds"
    exists <- liftIO $ doesDirectoryExist backgroundsDir
    if exists
      then do
        (_, files) <- liftIO $ listDirRecur =<< parseAbsDir backgroundsDir
        let result =
              V.fromList $
              map toFilePath $
              filter ((".jpg" ==) . fileExtension) files
        return (Just result, result)
      else do
        logError $ mconcat
          [ "Not updating backgrounds list, because "
          , fromString (show backgroundsDir)
          , " does not exist."
          ]
        return (oldValue, fromMaybe mempty oldValue)
