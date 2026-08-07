-- | Tests for the contrib layer's pure logic.
module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Data.Map.Strict as M

import XMonad
import XMonad.Util.EZConfig

main :: IO ()
main = do
  results <- mapM run tests
  unless (and results) exitFailure
  where
    run (name, ok) = do
      putStrLn ((if ok then "PASS " else "FAIL ") ++ name)
      pure ok

-- | The parser reads only 'modMask', but the default config is used rather
-- than a stub so that the test cannot be fooled by a field it forgot to set.
-- This config uses Super as its mod key.
stub :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
stub = def { modMask = mod4Mask }

parse :: String -> Maybe (KeyMask, KeySym)
parse = parseKey stub

parseSeq :: String -> Maybe [(KeyMask, KeySym)]
parseSeq = parseKeySequence stub

tests :: [(String, Bool)]
tests =
  [ ( "plain letter", parse "a" == Just (0, 0x61) )
  , ( "mod prefix maps to the config's modMask"
    , parse "M-a" == Just (mod4Mask, 0x61) )
  , ( "shift and mod combine"
    , parse "M-S-c" == Just (mod4Mask + shiftMask, 0x63) )
  , ( "control prefix", parse "C-y" == Just (controlMask, 0x79) )
  , ( "angled key names"
    , parse "M-S-<Return>" == Just (mod4Mask + shiftMask, 0xff0d) )
  , ( "angled Space alias resolves to the lowercase X11 name"
    , parse "M-<Space>" == Just (mod4Mask, 0x20) )
  , ( "arrow keys", parse "M-<Left>" == Just (mod4Mask, 0xff51) )

    -- These are the values river's get_xkb_binding receives, so an error here
    -- is a silently wrong binding rather than a compile failure.
  , ( "XF86 media keys keep their X11 values"
    , parse "<XF86AudioRaiseVolume>" == Just (0, 0x1008ff13)
      && parse "<XF86AudioMicMute>" == Just (0, 0x1008ffb2) )

  , ( "punctuation the config binds"
    , map parse ["M-,", "M-.", "M-;", "M-=", "M--", "M-S-/"]
        == [ Just (mod4Mask, 0x2c), Just (mod4Mask, 0x2e), Just (mod4Mask, 0x3b)
           , Just (mod4Mask, 0x3d), Just (mod4Mask, 0x2d)
           , Just (mod4Mask + shiftMask, 0x2f) ] )

  , ( "a two-key sequence parses as two keys"
    , parseSeq "M-m M-l" == Just [(mod4Mask, 0x6d), (mod4Mask, 0x6c)] )
  , ( "parseKey rejects a sequence"
    , parse "M-m M-l" == Nothing )
  , ( "an unknown key name fails rather than binding something else"
    , parse "M-<NoSuchKey>" == Nothing )

    -- Every binding in the real config's keymap must parse; anything that
    -- does not is silently dropped at runtime.
  , ( "every key description used by this config parses"
    , all (\d -> parseSeq d /= Nothing) configKeyDescriptions )
  ]

-- | The key descriptions from src/xmonad.hs, verbatim.
configKeyDescriptions :: [String]
configKeyDescriptions =
  [ "M-q", "M-<Space>", "M-u", "M-i", "M-o", "M-S-u", "M-S-i", "M-S-o"
  , "M-k", "M-j", "M-S-k", "M-S-j", "M-S-c", "M-h", "M-S-h", "M-t", "M-S-t"
  , "M-,", "M-.", "M-l", "M-;", "M-p", "M-S-<Return>", "M-e", "M-s"
  , "M-m M-l", "M-m M-m", "M-<Left>", "M-<Right>", "M-<Up>", "M-<Down>"
  , "M-S-<Up>", "M-S-<Down>", "M-m M-d", "M-S-/"
  , "M-S-=", "M-S--", "M-=", "M--"
  , "M-S-f", "M-S-d", "M-f", "M-d"
  , "<XF86AudioRaiseVolume>", "<XF86AudioLowerVolume>", "<XF86AudioMute>"
  , "<XF86AudioMicMute>", "<XF86AudioPlay>"
  , "M-r", "M-S-r", "M-b M-g", "M-a", "M-S-a", "M-y", "M-x"
  ] ++
  [ m ++ "M-" ++ i | i <- map show ([1..9] ++ [0 :: Int])
                   , m <- ["", "S-", "C-"] ]
