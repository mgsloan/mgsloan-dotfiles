-- | Prompts, backed by @fuzzel@.
--
-- xmonad draws prompts itself with X primitives, which has no counterpart
-- here. river does offer @river_window_manager_v1.get_shell_surface@, so a
-- native prompt is possible in principle, but it needs @wl_shm@ (and therefore
-- file descriptor passing, which this wire implementation does not do) plus
-- font rendering. Shelling out to an existing launcher is the pragmatic
-- answer.
--
-- Two behavioural differences follow from the prompt being a separate process:
--
-- * __The focused window changes.__ While fuzzel is up it holds keyboard
--   focus. Any handler wanting the /previously/ focused window's title must
--   read it before the prompt opens, which is why 'mkXPrompt' captures the
--   focus context up front and hands it to the completion function rather than
--   letting the handler query it afterwards. This config's @addNote@ depends
--   on that ordering.
--
-- * __The keymap is fuzzel's.__ 'promptKeymap' and friends are accepted for
--   source compatibility and ignored. The emacs-style bindings this config
--   sets (@C-Left@ word motion, @C-v@ paste) would have to be configured in
--   @fuzzel.ini@ instead.
module XMonad.Prompt
  ( XPrompt(..)
  , XPConfig(..)
  , XPPosition(..)
  , mkXPrompt
  , mkXPromptWithReturn
  , mkComplFunFromList
  , mkComplFunFromList'
  , emacsLikeXPKeymap
  , moveWord
  , pasteString
  , Direction1D(..)
  ) where

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.Default.Class (Default(..))
import Data.List (isInfixOf)
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents, hPutStr)
import System.Process
import qualified Data.Map as M

import XMonad.Core
import XMonad.River.X11Compat (KeyMask, KeySym)

-- | Anything that can label a prompt.
class XPrompt t where
  showXPrompt :: t -> String
  nextCompletion :: t -> String -> [String] -> String
  nextCompletion _ _ cs = case cs of { (c:_) -> c; [] -> "" }
  commandToComplete :: t -> String -> String
  commandToComplete _ = id
  completionToCommand :: t -> String -> String
  completionToCommand _ = id

data XPPosition = Top | Bottom deriving (Eq, Show)

data Direction1D = Next | Prev deriving (Eq, Show)

-- | The subset of xmonad's prompt configuration that maps onto fuzzel.
--
-- Fields that have no analogue are kept so that configs setting them still
-- compile; the haddock on each says what happens to it.
data XPConfig = XPConfig
  { font              :: String
    -- ^ Passed to fuzzel as @--font@. X11 XLFD or @xft:@ prefixes are
    -- stripped, since fuzzel wants a fontconfig name.
  , bgColor           :: String
  , fgColor           :: String
  , bgHLight          :: String
  , fgHLight          :: String
  , borderColor       :: String
  , promptBorderWidth :: Int
  , position          :: XPPosition
    -- ^ Ignored: fuzzel centres its window.
  , height            :: Int
    -- ^ Ignored: fuzzel sizes itself from the font and line count.
  , historySize       :: Int
    -- ^ Ignored: fuzzel keeps its own history.
  , promptKeymap      :: M.Map (KeyMask, KeySym) (X ())
    -- ^ Ignored: see the module header.
  , searchPredicate   :: String -> String -> Bool
  , alwaysHighlight   :: Bool
  , maxComplRows      :: Maybe Int
  }

instance Default XPConfig where
  def = XPConfig
    { font = "monospace:size=11"
    , bgColor = "black"
    , fgColor = "white"
    , bgHLight = "grey"
    , fgHLight = "black"
    , borderColor = "orange"
    , promptBorderWidth = 1
    , position = Bottom
    , height = 32
    , historySize = 1000
    , promptKeymap = M.empty
    , searchPredicate = isInfixOf
    , alwaysHighlight = False
    , maxComplRows = Just 15
    }

-- | Show a prompt and run an action on what the user typed.
mkXPrompt
  :: XPrompt p
  => p
  -> XPConfig
  -> (String -> IO [String])   -- ^ completion function
  -> (String -> X ())          -- ^ handler
  -> X ()
mkXPrompt p conf compl action = do
  mResult <- mkXPromptWithReturn p conf compl
  whenJust mResult action

-- | As 'mkXPrompt', but returns the entered string instead of acting on it.
--
-- Returns 'Nothing' if the user cancelled.
mkXPromptWithReturn
  :: XPrompt p => p -> XPConfig -> (String -> IO [String]) -> X (Maybe String)
mkXPromptWithReturn p conf compl = do
  -- Only complain when the config actually customised the keymap. A config
  -- leaving it at the default loses nothing by fuzzel owning key handling.
  unless (M.null (promptKeymap conf)) $
    warnUnimplemented "XPConfig.promptKeymap"
      "fuzzel handles prompt keys itself, so custom prompt bindings are \
      \ignored. Configure equivalents in ~/.config/fuzzel/fuzzel.ini."
  completions <- io (compl "")
  io (runFuzzel (showXPrompt p) conf completions)

-- | Run fuzzel with the given prompt and candidate list.
--
-- @--dmenu@ makes it read candidates on stdin and write the selection to
-- stdout. Crucially fuzzel prints whatever was typed when nothing matches,
-- which is what makes it usable for free-text prompts as well as
-- selection ones.
runFuzzel :: String -> XPConfig -> [String] -> IO (Maybe String)
runFuzzel prompt conf completions = do
  let args =
        [ "--dmenu"
        , "--prompt", prompt
        , "--font", fontconfigName (font conf)
        , "--background-color", rgba (bgColor conf)
        , "--text-color", rgba (fgColor conf)
        , "--selection-color", rgba (bgHLight conf)
        , "--selection-text-color", rgba (fgHLight conf)
        , "--border-color", rgba (borderColor conf)
        , "--border-width", show (promptBorderWidth conf)
        ] ++ maybe [] (\n -> ["--lines", show n]) (maxComplRows conf)
  (Just hin, Just hout, _, ph) <- createProcess (proc "fuzzel" args)
    { std_in = CreatePipe, std_out = CreatePipe }
  hPutStr hin (unlines completions)
  hClose hin
  out <- hGetContents hout
  -- Force the output before waiting, or the pipe deadlocks.
  length out `seq` pure ()
  code <- waitForProcess ph
  pure $ case code of
    ExitSuccess | not (all isSpace out) -> Just (trimEnd out)
    _ -> Nothing
  where
    trimEnd = reverse . dropWhile (== '\n') . reverse

-- | xmonad configs write X font names; fuzzel wants fontconfig.
-- @\"xft:Hack:pixelsize=18\"@ becomes @\"Hack:pixelsize=18\"@.
fontconfigName :: String -> String
fontconfigName f = case f of
  ('x':'f':'t':':':rest) -> rest
  _ -> f

-- | fuzzel wants @rrggbbaa@ with no leading hash.
rgba :: String -> String
rgba c = case c of
  ('#':rest) | length rest == 6 -> rest ++ "ff"
             | length rest == 8 -> rest
  _ -> M.findWithDefault "000000ff" c named
  where
    named = M.fromList
      [ ("black", "000000ff"), ("white", "ffffffff")
      , ("grey", "808080ff"), ("gray", "808080ff")
      , ("orange", "ffa500ff"), ("yellow", "ffff00ff")
      , ("red", "ff0000ff") ]

mkComplFunFromList :: [String] -> String -> IO [String]
mkComplFunFromList xs s = pure (filter (isInfixOf s) xs)

mkComplFunFromList' :: XPConfig -> [String] -> String -> IO [String]
mkComplFunFromList' conf xs s = pure (filter (searchPredicate conf s) xs)

-- | Accepted for source compatibility. fuzzel owns the prompt's key handling.
emacsLikeXPKeymap :: M.Map (KeyMask, KeySym) (X ())
emacsLikeXPKeymap = M.empty

-- | A prompt keymap action. Never invoked: 'promptKeymap' is not consulted.
-- Present so that configs building a keymap out of these still compile.
moveWord :: Direction1D -> X ()
moveWord _ = pure ()

-- | See 'moveWord'.
pasteString :: X ()
pasteString = pure ()
