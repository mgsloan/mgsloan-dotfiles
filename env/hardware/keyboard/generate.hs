#!/usr/bin/env stack
-- stack runghc --package shelly

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text as T
import Shelly
import Prelude hiding (FilePath)
import System.Environment (getArgs)

-- How to add this keymap to your keyboard:
--
-- 1. Enter kinesis advantage power user mode by holding down
-- `Progm+Shift+Escape`. Note that if you have remapped these keys,
-- use the remapped ones.
--
-- 2. Create a new layout: Press `Progm+F2`. Then press `m`, to
-- specify the name of the new layout. Or use a different name by
-- changing the prefix of 'layout', below.
--
-- 3. Mount your Kinesis's v-drive. Switch it to v-drive mode by
-- typing `Progm+F1`. Modify 'kinesisMount', below, to the mounted
-- path.
--
-- 4. Check that `m_qwerty.txt` exists in the `active` folder of the
-- v-drive.
--
-- 5. Run this program. Pass in "Adv2" or "Adv360" as the first arg.
--
-- 6. Press `Progm+F1` to close and apply v-drive settings.  I'd hoped
-- to be able to script this via unmounting, but doing that didn't
-- apply the settings.
--
-- 7. Type `Progm+m` to switch to this layout. Note that after
-- unplugging and replugging the keyboard, it will be on the last used
-- layout - no need to switch again.
--
-- The nice thing about not overwriting `qwerty.txt` is that I can
-- have that layout be empty, so that I can switch to it for other
-- people to try out the keyboard.

data Keyboard = Adv2 | Adv360 deriving Read

layoutFile :: Keyboard -> FilePath
layoutFile Adv2 = "m_qwerty.txt"
layoutFile Adv360 = "layout1.txt"

vDriveLayoutsPath :: Keyboard -> FilePath
vDriveLayoutsPath Adv2 = "/media/mgsloan/KINESIS KB/active"
vDriveLayoutsPath Adv360 = "/media/mgsloan/ADV360/layouts"

main :: IO ()
main = do
  [read -> kbd] <- getArgs
  shelly $ updateLayout kbd

updateLayout :: Keyboard -> Sh ()
updateLayout kbd = do
  let outputFile = layoutFile kbd
  writefile outputFile $ T.unlines $ concat
    [ case kbd of
        Adv2 -> []
        Adv360 -> ["<base>"]

    -- Tilde in top left corner, next to number keys, like on most
    -- keyboards.
    , case kbd of
        Adv2 -> ["[=]>[`]"]
        Adv360 -> ["[eql]>[grav]"]

    -- Escape key is to the left on the home row - nice for vim-ey
    -- bindings.
    , case kbd of
        Adv2 -> ["[caps]>[escape]"]
        Adv360 -> []

    -- Brackets on either sides, under the pinkies.
    , case kbd of
        Adv2 -> ["[lshift]>[obrack]", "[rshift]>[cbrack]"]
        Adv360 -> ["[lshf]>[obrk]", "[rshf]>[cbrk]"]

    -- Arrow keys are similar to the hjkl layout, but shifted one
    -- over. Not sure how I got used to that, but I did.
    , ["[up]>[left]"]
    , case kbd of
        Adv2 ->
          [ "[obrack]>[up]"
          , "[cbrack]>[right]"
          ]
        Adv360 ->
          [ "[obrk]>[up]"
          , "[cbrk]>[rght]"
          ]

    -- Bottom row of keys for left hand has finger rolls for `<-`,
    -- `->`, `<->`.  Naturally, for typing Haskell code. `=` is also
    -- on this row for niceness of typing `=>`, `<=`, `>=`, `>>=`,
    -- etc. This also works nicely because it means `+` and `-` are
    -- near-ish eachother.
    , case kbd of
        Adv2 -> ["[`]>[=]"]
        Adv360 -> ["[grav]>[eql]"]
    , case kbd of
        Adv2 -> ["[left]>[hyphen]"]
        Adv360 -> ["[left]>[hyph]"]
    , case kbd of
        Adv2 -> ["{right}>{speed9}{-rshift}{.}{+rshift}"]
        Adv360 -> []
          -- Adv 360 changed the behavior of macros vs bindings, with
          -- this I can't rebind rght to its position. Waiting to hear
          -- back from support about this..
          --
          -- [ "{rght}>{s9}{x1}{-rshf}{perd}{+rshf}"
          -- , "{caps}>{s9}{x1}{-rshf}{comm}{+rshf}"
          -- ]

    -- Since the default location for `=` got bound to backtick, need
    -- a place to put `=`. Since hyphen is available on middle finger
    -- of right hand, we can use it's position, so effectively the `=`
    -- key is still in a corner, just on the opposite side.
    , case kbd of
        Adv2 -> ["[hyphen]>[=]"]
        Adv360 -> ["[hyph]>[eql]"]

    -- Left thumb keys
    --
    -- NOTE: The usage of "calc" here gets remapped by my xkb config
    -- to be the hyper modifier key.
    --
    -- NOTE: In most applications, "menu" brings up
    -- right-click. However, in my emacs configuration, it enters into
    -- a mode for quickly moving the cursor (avy jump).
    --
    --        \----/ \----/
    --        |hype| |alt |
    --        /----\ /----\
    --
    -- \----/ \----/ \----/
    -- |bksp| |lshi| |ctrl|
    -- |    | |ft  | /----\
    -- |    | |    |
    -- |    | |    | \----/
    -- |    | |    | |menu|
    -- /----\ /----\ /----\
    --
    -- (backspace is in its default location)
    -- (left alt is in its default location)
    , case kbd of
        Adv2 ->
          [ "[lctrl]>[calc]"
          , "[delete]>[lshift]"
          , "[home]>[lctrl]"
          , "[end]>[menu]"
          ]
        Adv360 ->
          [ "[lctr]>[calc]"
          , "[del]>[lshf]"
          , "[home]>[lctr]"
          , "[end]>[kh76]"
          ]

    -- Right thumb keys
    --
    -- \----/ \----/
    -- |alt | |win |
    -- /----\ /----\
    --
    -- \----/ \----/ \----/
    -- |ctrl| |rshi| |spac|
    -- /----\ |ft  | |e   |
    --        |    | |    |
    -- \----/ |    | |    |
    -- |entr| |    | |    |
    -- /----\ /----\ /----\
    --
    , ["[rwin]>[lalt]"]
    , case kbd of
        Adv2 ->
          [ "[rctrl]>[rwin]"
          , "[pup]>[rctrl]"
          , "[enter]>[rshift]"
          -- (space is in its default location)
          , "[pdown]>[enter]"
          ]
        Adv360 ->
          [ "[rctr]>[rwin]"
          , "[pgup]>[rctr]"
          , "[ent]>[rshf]"
          -- (space is in its default location)
          , "[pgdn]>[ent]"
          ]

    , case kbd of
        Adv2 -> []
        Adv360 ->
          [ ""
          , "<keypad>"
          , ""
          , "<function1>"
          , ""
          , "<function2>"
          , ""
          , "<function3>"
          ]

    -- Actions that don't have keys, since I don't seem to use them:
    -- `[delete]`, `[pup]`, `[pdown]`.

    -- Keys not yet remapped (redundant):
    --
    -- , "[hyphen]>"
    ]
  -- Copy over old layout
  let destinationDir = vDriveLayoutsPath kbd
      destination = destinationDir </> outputFile
  rm_f destination
  cp outputFile destinationDir
  liftIO $ putStrLn $ "keymap copied to " ++ show destination
