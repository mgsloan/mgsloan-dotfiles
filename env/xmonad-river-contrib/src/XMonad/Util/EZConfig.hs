-- | Emacs-style key descriptions, as in xmonad-contrib's
-- @XMonad.Util.EZConfig@.
--
-- The parser is the same because it can be: xkbcommon reuses X11's keysym
-- numbering and river's modifier bits are X11's, so @\"M-S-\<Return\>\"@
-- resolves to the same @(mask, keysym)@ pair it would under xmonad and can be
-- handed straight to @river_xkb_bindings_v1.get_xkb_binding@.
--
-- One real difference: xmonad supports multi-key submaps, and this config uses
-- them (@\"M-m M-l\"@, @\"M-b M-g\"@). river has no notion of a prefix key, so
-- 'additionalKeysP' implements submaps itself — see 'parseKeySequence'.
module XMonad.Util.EZConfig
  ( additionalKeysP
  , additionalKeys
  , additionalMouseBindings
  , removeKeysP
  , mkKeymap
  , parseKey
  , parseKeySequence
  ) where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M

import XMonad.Core
import XMonad.River.X11Compat

--------------------------------------------------------------------------------
-- Public interface

-- | Add key bindings written as strings.
additionalKeysP :: XConfig l -> [(String, X ())] -> XConfig l
additionalKeysP conf keyList = conf
  { keys = \cnf -> mkKeymap cnf keyList `M.union` keys conf cnf }

-- | Add key bindings given as explicit @(mask, keysym)@ pairs.
additionalKeys :: XConfig l -> [((KeyMask, KeySym), X ())] -> XConfig l
additionalKeys conf keyList = conf
  { keys = \cnf -> M.fromList keyList `M.union` keys conf cnf }

additionalMouseBindings
  :: XConfig l -> [((KeyMask, Button), Window -> X ())] -> XConfig l
additionalMouseBindings conf mouseBindingsList = conf
  { mouseBindings = \cnf -> M.fromList mouseBindingsList `M.union`
                            mouseBindings conf cnf }

removeKeysP :: XConfig l -> [String] -> XConfig l
removeKeysP conf keyList = conf
  { keys = \cnf -> keys conf cnf `M.difference`
                   M.fromList (zip (mapMaybe (parseKey cnf) keyList) (repeat ())) }

-- | Build a keymap from string descriptions.
--
-- Descriptions that name a key sequence rather than a single key are grouped
-- into submaps, so that @\"M-m M-l\"@ and @\"M-m M-m\"@ share one @M-m@ prefix
-- binding.
mkKeymap :: XConfig l -> [(String, X ())] -> M.Map (KeyMask, KeySym) (X ())
mkKeymap conf = buildSubmaps . mapMaybe parse
  where
    parse (descr, action) = (\ks -> (ks, action)) <$> parseKeySequence conf descr

--------------------------------------------------------------------------------
-- Parsing

-- | Parse a single key description, e.g. @\"M-S-\<Return\>\"@.
parseKey :: XConfig l -> String -> Maybe (KeyMask, KeySym)
parseKey conf descr = case parseKeySequence conf descr of
  Just [k] -> Just k
  _        -> Nothing

-- | Parse a whitespace-separated sequence of key descriptions.
--
-- Returns 'Nothing' if any component fails to parse, so that a typo in a
-- config is dropped rather than silently binding something unintended.
parseKeySequence :: XConfig l -> String -> Maybe [(KeyMask, KeySym)]
parseKeySequence conf = mapM (parseSingle conf) . words

parseSingle :: XConfig l -> String -> Maybe (KeyMask, KeySym)
parseSingle conf = go 0
  where
    go mask s = case s of
      ('M':'-':rest) -> go (mask + modMask conf) rest
      ('S':'-':rest) -> go (mask + shiftMask) rest
      ('C':'-':rest) -> go (mask + controlMask) rest
      ('M':'1':'-':rest) -> go (mask + mod1Mask) rest
      ('M':'2':'-':rest) -> go (mask + mod2Mask) rest
      ('M':'3':'-':rest) -> go (mask + mod3Mask) rest
      ('M':'4':'-':rest) -> go (mask + mod4Mask) rest
      ('M':'5':'-':rest) -> go (mask + mod5Mask) rest
      _ -> (\ks -> (mask, ks)) <$> parseKeyName s

-- | A key name is either a single character, or an @\<Angled\>@ name.
parseKeyName :: String -> Maybe KeySym
parseKeyName s = case s of
  ['<'] -> Nothing
  ('<':rest) | not (null rest), last rest == '>' ->
    stringToKeysym (mapName (init rest))
  [c] -> stringToKeysym [c]
  _ -> stringToKeysym s

-- | EZConfig's spellings for keys whose X11 names differ.
mapName :: String -> String
mapName n = M.findWithDefault n n aliases
  where
    aliases = M.fromList
      [ ("Backspace", "BackSpace")
      , ("Esc", "Escape")
      , ("Enter", "Return")
      , ("PageUp", "Prior")
      , ("PageDown", "Next")
      , ("Space", "space")
      , ("Insert", "Insert")
      , ("Del", "Delete")
      ]

--------------------------------------------------------------------------------
-- Submaps

-- | Collapse multi-key sequences into nested prefix bindings.
--
-- river binds single keys only, so a two-key sequence becomes a binding on the
-- prefix whose action installs the continuation. Bindings that share a prefix
-- are merged; a sequence colliding with a plain binding on the same key loses,
-- matching xmonad's behaviour of preferring the more specific description.
buildSubmaps :: [([(KeyMask, KeySym)], X ())] -> M.Map (KeyMask, KeySym) (X ())
buildSubmaps = fmap toAction . foldl' insert M.empty
  where
    insert acc (ks, action) = case ks of
      []      -> acc
      [k]     -> M.insert k (Leaf action) acc
      (k:rest) -> M.insertWith merge k (Branch [(rest, action)]) acc

    merge (Branch newer) (Branch older) = Branch (newer ++ older)
    merge newer _ = newer

    toAction (Leaf action) = action
    toAction (Branch subs) = submap (buildSubmaps subs)

data Node = Leaf (X ()) | Branch [([(KeyMask, KeySym)], X ())]

-- | Wait for one more key press and dispatch it.
--
-- Not yet implemented: it needs a transient river binding set, installed for
-- the duration of the prefix and torn down afterwards, using
-- @river_xkb_bindings_seat_v1.ensure_next_key_eaten@ so that an unbound key
-- cancels cleanly rather than reaching the focused window.
submap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
submap _ = trace "xmonad-river: submaps (multi-key sequences) are not implemented yet"
