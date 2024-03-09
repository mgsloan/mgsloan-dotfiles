module Prompt where

import Data.List (isInfixOf)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import XMonad.Prompt
import qualified Data.Map as M
import qualified XMonad.Prompt.Shell as Shell

import Imports
import Misc

data GenericPrompt = GenericPrompt String

instance XPrompt GenericPrompt where
  showXPrompt (GenericPrompt x) = x

shellPrompt :: XX ()
shellPrompt = do
  (cmds, xpConfig) <- runXio $
    io Shell.getCommands `concurrently` getXpConfig
  let completion = Shell.getShellCompl cmds $ searchPredicate xpConfig
  env <- ask
  toXX $ mkXPrompt Shell.Shell xpConfig completion $ \input ->
    withEnv env $ spawn "sh" ["-c", input]

data ActionsPrompt = ActionsPrompt

instance XPrompt ActionsPrompt where
  showXPrompt ActionsPrompt = "M-x "

actionPrompt :: M.Map String (XX ()) -> XX ()
actionPrompt actions = do
  xpConfig <- getXpConfig
  let completion = mkComplFunFromList' xpConfig (M.keys actions)
  env <- ask
  toXX $ mkXPrompt ActionsPrompt xpConfig completion $ \input ->
    withEnv env $
      case M.lookup input actions of
        Nothing -> forkXio $ notify $ "No action matching " <> input
        Just action -> do
          logDebug $ "Running action " <> fromString input
          action
          logDebug $ "Finished running action " <> fromString input

data ColorScheme = Light | Dark

getColorScheme :: Xio ColorScheme
getColorScheme = do
  result <- timeout (100 * 1000) $ syncSpawnAndRead
    "gsettings"
    [ "get"
    , "org.gnome.desktop.interface"
    , "color-scheme"
    ]
  syncSpawn "notify-send" ["Uhh", show result]
  return $ case result of
    Just value | "prefer-light" `isInfixOf` value -> Light
    _ -> Dark

getXpConfig :: (MonadIO m, MonadReader Env m) => m XPConfig
getXpConfig = mkXpConfig <$> runXio getColorScheme

mkXpConfig :: ColorScheme -> XPConfig
mkXpConfig colorScheme = setColors $ def
  { font              = if isHiDpi
                          then "xft:Hack:pixelsize=18"
                          else "xft:Hack:pixelsize=10"
  , promptBorderWidth = 1
  , position          = Bottom
  , height            = if isHiDpi then 32 else 18
  , historySize       = 1000
  , promptKeymap      = km
  }
  where
    setColors x = case colorScheme of
      Light -> x
        { bgColor           = "white"
        , fgColor           = "black"
        , bgHLight          = "yellow"
        , fgHLight          = "black"
        , borderColor       = "orange"
        }
      Dark -> x
        { bgColor           = "black"
        , fgColor           = "white"
        , bgHLight          = "gray"
        , fgHLight          = "black"
        , borderColor       = "orange"
        }
    km =
      M.insert (controlMask, xK_Right) (moveWord Next) $
      M.insert (controlMask, xK_Left) (moveWord Prev) $
      -- C-y makes no sense for paste??
      M.delete (controlMask, xK_y) $
      M.insert (controlMask, xK_v) pasteString $
      M.insert (mod4Mask, xK_v) pasteString $
      emacsLikeXPKeymap

isHiDpi :: Bool
isHiDpi = isJust $ unsafePerformIO $ lookupEnv "HIDPI"
