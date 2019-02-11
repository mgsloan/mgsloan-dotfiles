#!/usr/bin/env stack
-- stack runghc --package shelly

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Shelly
import Prelude hiding (FilePath)

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
-- 5. Run this program.
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

layout :: FilePath
layout = "m_qwerty.txt"

kinesisMount :: FilePath
kinesisMount = "/media/mgsloan/KINESIS KB" :: FilePath

main :: IO ()
main = shelly $ do
  writefile layout $ T.unlines
    -- Tilde in top right corner, next to number keys, like on most
    -- keyboards.
    [ "[=]>[`]"

    -- Escape key is to the left on the home row - nice for vim-ey
    -- bindings.
    , "[caps]>[escape]"

    -- Brackets on either sides, under the pinkies.
    , "[lshift]>[obrack]", "[rshift]>[cbrack]"

    -- Arrow keys are similar to the hjkl layout, but shifted one
    -- over. Not sure how I got used to that, but I did.
    , "[up]>[left]"
    , "[obrack]>[up]"
    , "[cbrack]>[right]"

    -- Bottom row of keys for left hand has finger rolls for `<-`,
    -- `->`, `<->`.  Naturally, for typing Haskell code. `=` is also
    -- on this row for niceness of typing `=>`, `<=`, `>=`, `>>=`,
    -- etc. This also works nicely because it means `+` and `-` are
    -- near-ish eachother.
    , "[`]>[=]"
    , "[left]>[hyphen]"
    , "{right}>{speed9}{-rshift}{.}{+rshift}"

    -- Since the default location for `=` got bound to backtick, need
    -- a place to put `=`. Since hyphen is available on middle finger
    -- of right hand, we can use it's position, so effectively the `=`
    -- key is still in a corner, just on the opposite side.
    , "[hyphen]>[=]"

    -- Left thumb keys
    --
    -- Reminder: try to actually use lshift.. lol!
    --
    -- NOTE: The usage of "calc" here relies on my .Xmodmap which
    -- binds it to mod3 / Hyper_L. In most applications, "menu" brings
    -- up right-click. However, in my emacs configuration, it enters
    -- into a mode for quickly moving the cursor (ace-jump-mode).
    --
    --        \----/ \----/
    --        |ctrl| |alt |
    --        /----\ /----\
    --
    -- \----/ \----/ \----/
    -- |bksp| |lshi| |calc|
    -- |    | |ft  | /----\
    -- |    | |    |
    -- |    | |    | \----/
    -- |    | |    | |menu|
    -- /----\ /----\ /----\
    --
    -- , "[lctrl]>"
    -- , "[lalt]>"
    -- (backspace is in its default location)
    , "[delete]>[lshift]"
    , "[home]>[calc]"
    , "[end]>[menu]"

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
    , "[rwin]>[lalt]"
    , "[rctrl]>[rwin]"
    , "[pup]>[rctrl]"
    , "[enter]>[rshift]"
    -- (space is in its default location)
    , "[pdown]>[enter]"

    -- Actions that don't have keys, since I don't seem to use them:
    -- `[delete]`, `[pup]`, `[pdown]`.

    -- Keys not yet remapped (redundant):
    --
    -- , "[hyphen]>"
    ]
  -- Copy over old layout
  let destination = kinesisMount </> ("active" :: FilePath)
  rm_f (destination </> layout)
  cp layout destination
