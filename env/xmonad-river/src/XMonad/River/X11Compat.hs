-- | The handful of X11 types and constants that xmonad's API exposes, redefined
-- against river.
--
-- The reason this is possible at all, rather than being a translation layer, is
-- a pair of happy coincidences:
--
-- * __Keysyms are identical.__ xkbcommon reuses X11's keysym numbering, so
--   @xK_Return@ is 0xff0d in both worlds and every key name in the config
--   means the same thing. @river_xkb_bindings_v1.get_xkb_binding@ takes an
--   xkbcommon keysym directly.
--
-- * __Modifier masks are identical.__ @river_seat_v1.modifiers@ assigns
--   shift=1, ctrl=4, mod1=8, mod3=32, mod4=64, mod5=128, which are exactly
--   X11's @ShiftMask@ and friends. So @mod4Mask@ keeps its value and its
--   meaning.
--
-- What is genuinely different is 'Window', which is a river protocol object
-- rather than an X server resource id, and 'Rectangle', which is redefined here
-- only so that nothing has to depend on the @X11@ package.
module XMonad.River.X11Compat
  ( -- * Geometry
    Rectangle(..)
  , Position
  , Dimension
    -- * Windows
  , Window
    -- * Input
  , KeyMask
  , KeySym
  , Button
    -- * Modifier masks
  , shiftMask
  , lockMask
  , controlMask
  , mod1Mask
  , mod2Mask
  , mod3Mask
  , mod4Mask
  , mod5Mask
  , noModMask
    -- * Buttons
  , button1
  , button2
  , button3
  , button4
  , button5
    -- * Keysyms
  , stringToKeysym
  , keysymToString
  , xK_Return
  , xK_Escape
  , xK_Tab
  , xK_BackSpace
  , xK_Delete
  , xK_Left
  , xK_Right
  , xK_Up
  , xK_Down
  , xK_space
  , xK_exclam
  , xK_quotedbl
  , xK_numbersign
  , xK_dollar
  , xK_percent
  , xK_ampersand
  , xK_apostrophe
  , xK_parenleft
  , xK_parenright
  , xK_asterisk
  , xK_plus
  , xK_comma
  , xK_minus
  , xK_period
  , xK_slash
  , xK_0
  , xK_1
  , xK_2
  , xK_3
  , xK_4
  , xK_5
  , xK_6
  , xK_7
  , xK_8
  , xK_9
  , xK_colon
  , xK_semicolon
  , xK_less
  , xK_equal
  , xK_greater
  , xK_question
  , xK_at
  , xK_A
  , xK_B
  , xK_C
  , xK_D
  , xK_E
  , xK_F
  , xK_G
  , xK_H
  , xK_I
  , xK_J
  , xK_K
  , xK_L
  , xK_M
  , xK_N
  , xK_O
  , xK_P
  , xK_Q
  , xK_R
  , xK_S
  , xK_T
  , xK_U
  , xK_V
  , xK_W
  , xK_X
  , xK_Y
  , xK_Z
  , xK_bracketleft
  , xK_backslash
  , xK_bracketright
  , xK_asciicircum
  , xK_underscore
  , xK_grave
  , xK_a
  , xK_b
  , xK_c
  , xK_d
  , xK_e
  , xK_f
  , xK_g
  , xK_h
  , xK_i
  , xK_j
  , xK_k
  , xK_l
  , xK_m
  , xK_n
  , xK_o
  , xK_p
  , xK_q
  , xK_r
  , xK_s
  , xK_t
  , xK_u
  , xK_v
  , xK_w
  , xK_x
  , xK_y
  , xK_z
  , xK_braceleft
  , xK_bar
  , xK_braceright
  , xK_asciitilde
  ) where

import Data.Char (chr, ord)
import Data.Int (Int32)
import Data.Word (Word32)
import qualified Data.Map.Strict as M

import XMonad.River.Wire (ObjectId)

--------------------------------------------------------------------------------
-- Geometry

-- | X11 uses @Int16@ positions and @Word16@ dimensions; river uses @Int32@
-- throughout. The wider types are used here, which is strictly more permissive,
-- so layout code written against xmonad's 'Rectangle' still typechecks.
type Position = Int32
type Dimension = Word32

data Rectangle = Rectangle
  { rect_x      :: !Position
  , rect_y      :: !Position
  , rect_width  :: !Dimension
  , rect_height :: !Dimension
  } deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- Windows

-- | A managed window. Under X11 this is a server-side resource id; here it is
-- the @river_window_v1@ protocol object.
--
-- The distinction matters in one place: river object ids are recycled after
-- @wl_display.delete_id@, so a 'Window' must never be retained past the
-- @closed@ event. The window's @identifier@ event provides a genuinely unique
-- string for anything that needs to outlive the object.
type Window = ObjectId

--------------------------------------------------------------------------------
-- Input

type KeyMask = Word32
type KeySym = Word32
type Button = Word32

-- | These match X11's masks, and hence @river_seat_v1.modifiers@.
shiftMask, lockMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask,
  mod5Mask, noModMask :: KeyMask
shiftMask   = 1
lockMask    = 2
controlMask = 4
mod1Mask    = 8
mod2Mask    = 16
mod3Mask    = 32
mod4Mask    = 64
mod5Mask    = 128
noModMask   = 0

-- | X11 button numbers. River's pointer bindings take Linux input event codes
-- instead, so these are translated at the point of use rather than being the
-- same numbers; they are kept in X11 form because that is what the config
-- writes.
button1, button2, button3, button4, button5 :: Button
button1 = 1
button2 = 2
button3 = 3
button4 = 4
button5 = 5

--------------------------------------------------------------------------------
-- Keysyms

-- | Parse a key name in X11's spelling, as used by
-- "XMonad.Util.EZConfig" key descriptions.
--
-- Printable ASCII maps to its own code point, which is how X11 (and therefore
-- xkbcommon) numbers the Latin-1 range. Everything else comes from the table
-- below.
stringToKeysym :: String -> Maybe KeySym
stringToKeysym [c]
  | ord c >= 0x20 && ord c <= 0xff = Just (fromIntegral (ord c))
stringToKeysym name = M.lookup name namedKeysyms

-- | Inverse of 'stringToKeysym', for diagnostics.
keysymToString :: KeySym -> String
keysymToString ks = case M.lookup ks reverseKeysyms of
  Just name -> name
  Nothing
    | ks >= 0x20 && ks <= 0xff -> [chr (fromIntegral ks)]
    | otherwise -> "0x" ++ showHex ks
  where
    showHex 0 = "0"
    showHex n = go n ""
      where
        go 0 acc = acc
        go m acc = go (m `div` 16) (digit (m `mod` 16) : acc)
        digit d | d < 10 = chr (ord '0' + fromIntegral d)
                | otherwise = chr (ord 'a' + fromIntegral d - 10)

reverseKeysyms :: M.Map KeySym String
reverseKeysyms = M.fromList [ (v, k) | (k, v) <- M.toList namedKeysyms ]

-- | Named keysyms, with X11's values. Covers what this config's keymap uses:
-- editing and navigation keys, the function keys, the keypad, and the XF86
-- media keys.
namedKeysyms :: M.Map String KeySym
namedKeysyms = M.fromList $
  [ ("BackSpace", 0xff08), ("Tab", 0xff09), ("Linefeed", 0xff0a)
  , ("Clear", 0xff0b), ("Return", 0xff0d), ("Pause", 0xff13)
  , ("Scroll_Lock", 0xff14), ("Sys_Req", 0xff15), ("Escape", 0xff1b)
  , ("Delete", 0xffff), ("Home", 0xff50), ("Left", 0xff51), ("Up", 0xff52)
  , ("Right", 0xff53), ("Down", 0xff54), ("Prior", 0xff55), ("Page_Up", 0xff55)
  , ("Next", 0xff56), ("Page_Down", 0xff56), ("End", 0xff57), ("Begin", 0xff58)
  , ("Insert", 0xff63), ("Menu", 0xff67), ("Num_Lock", 0xff7f)
  , ("space", 0x0020), ("Print", 0xff61)
  , ("Caps_Lock", 0xffe5), ("Shift_L", 0xffe1), ("Shift_R", 0xffe2)
  , ("Control_L", 0xffe3), ("Control_R", 0xffe4)
  , ("Alt_L", 0xffe9), ("Alt_R", 0xffea)
  , ("Super_L", 0xffeb), ("Super_R", 0xffec)
  ] ++
  [ ("F" ++ show n, 0xffbe + fromIntegral n - 1) | n <- [1 :: Int .. 24] ] ++
  [ ("KP_" ++ show n, 0xffb0 + fromIntegral n) | n <- [0 :: Int .. 9] ] ++
  [ ("KP_Enter", 0xff8d), ("KP_Add", 0xffab), ("KP_Subtract", 0xffad)
  , ("KP_Multiply", 0xffaa), ("KP_Divide", 0xffaf), ("KP_Decimal", 0xffae)
  ] ++
  -- The XF86 media keys, spelled as the config writes them.
  [ ("XF86AudioLowerVolume", 0x1008ff11), ("XF86AudioMute", 0x1008ff12)
  , ("XF86AudioRaiseVolume", 0x1008ff13), ("XF86AudioPlay", 0x1008ff14)
  , ("XF86AudioStop", 0x1008ff15), ("XF86AudioPrev", 0x1008ff16)
  , ("XF86AudioNext", 0x1008ff17), ("XF86AudioMedia", 0x1008ff32)
  , ("XF86AudioMicMute", 0x1008ffb2)
  , ("XF86MonBrightnessUp", 0x1008ff02), ("XF86MonBrightnessDown", 0x1008ff03)
  , ("XF86Display", 0x1008ff59), ("XF86Sleep", 0x1008ff2f)
  , ("XF86PowerOff", 0x1008ff2a), ("XF86ScreenSaver", 0x1008ff2d)
  , ("XF86WLAN", 0x1008ff95), ("XF86Search", 0x1008ff1b)
  , ("XF86HomePage", 0x1008ff18), ("XF86Mail", 0x1008ff19)
  , ("XF86Calculator", 0x1008ff1d), ("XF86Explorer", 0x1008ff5d)
  , ("XF86TouchpadToggle", 0x1008ffa9)
  ]

-- | Keysyms for printable ASCII. In this range an X11 (and xkbcommon)
-- keysym is just the character's code point.
xK_space, xK_exclam, xK_quotedbl, xK_numbersign, xK_dollar, xK_percent, xK_ampersand, xK_apostrophe,
  xK_parenleft, xK_parenright, xK_asterisk, xK_plus, xK_comma, xK_minus, xK_period, xK_slash,
  xK_0, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7,
  xK_8, xK_9, xK_colon, xK_semicolon, xK_less, xK_equal, xK_greater, xK_question,
  xK_at, xK_A, xK_B, xK_C, xK_D, xK_E, xK_F, xK_G,
  xK_H, xK_I, xK_J, xK_K, xK_L, xK_M, xK_N, xK_O,
  xK_P, xK_Q, xK_R, xK_S, xK_T, xK_U, xK_V, xK_W,
  xK_X, xK_Y, xK_Z, xK_bracketleft, xK_backslash, xK_bracketright, xK_asciicircum, xK_underscore,
  xK_grave, xK_a, xK_b, xK_c, xK_d, xK_e, xK_f, xK_g,
  xK_h, xK_i, xK_j, xK_k, xK_l, xK_m, xK_n, xK_o,
  xK_p, xK_q, xK_r, xK_s, xK_t, xK_u, xK_v, xK_w,
  xK_x, xK_y, xK_z, xK_braceleft, xK_bar, xK_braceright, xK_asciitilde :: KeySym

xK_space = 0x0020
xK_exclam = 0x0021
xK_quotedbl = 0x0022
xK_numbersign = 0x0023
xK_dollar = 0x0024
xK_percent = 0x0025
xK_ampersand = 0x0026
xK_apostrophe = 0x0027
xK_parenleft = 0x0028
xK_parenright = 0x0029
xK_asterisk = 0x002a
xK_plus = 0x002b
xK_comma = 0x002c
xK_minus = 0x002d
xK_period = 0x002e
xK_slash = 0x002f
xK_0 = 0x0030
xK_1 = 0x0031
xK_2 = 0x0032
xK_3 = 0x0033
xK_4 = 0x0034
xK_5 = 0x0035
xK_6 = 0x0036
xK_7 = 0x0037
xK_8 = 0x0038
xK_9 = 0x0039
xK_colon = 0x003a
xK_semicolon = 0x003b
xK_less = 0x003c
xK_equal = 0x003d
xK_greater = 0x003e
xK_question = 0x003f
xK_at = 0x0040
xK_A = 0x0041
xK_B = 0x0042
xK_C = 0x0043
xK_D = 0x0044
xK_E = 0x0045
xK_F = 0x0046
xK_G = 0x0047
xK_H = 0x0048
xK_I = 0x0049
xK_J = 0x004a
xK_K = 0x004b
xK_L = 0x004c
xK_M = 0x004d
xK_N = 0x004e
xK_O = 0x004f
xK_P = 0x0050
xK_Q = 0x0051
xK_R = 0x0052
xK_S = 0x0053
xK_T = 0x0054
xK_U = 0x0055
xK_V = 0x0056
xK_W = 0x0057
xK_X = 0x0058
xK_Y = 0x0059
xK_Z = 0x005a
xK_bracketleft = 0x005b
xK_backslash = 0x005c
xK_bracketright = 0x005d
xK_asciicircum = 0x005e
xK_underscore = 0x005f
xK_grave = 0x0060
xK_a = 0x0061
xK_b = 0x0062
xK_c = 0x0063
xK_d = 0x0064
xK_e = 0x0065
xK_f = 0x0066
xK_g = 0x0067
xK_h = 0x0068
xK_i = 0x0069
xK_j = 0x006a
xK_k = 0x006b
xK_l = 0x006c
xK_m = 0x006d
xK_n = 0x006e
xK_o = 0x006f
xK_p = 0x0070
xK_q = 0x0071
xK_r = 0x0072
xK_s = 0x0073
xK_t = 0x0074
xK_u = 0x0075
xK_v = 0x0076
xK_w = 0x0077
xK_x = 0x0078
xK_y = 0x0079
xK_z = 0x007a
xK_braceleft = 0x007b
xK_bar = 0x007c
xK_braceright = 0x007d
xK_asciitilde = 0x007e

-- | Named keys outside the printable range.
xK_Return, xK_Escape, xK_Tab, xK_BackSpace, xK_Delete,
  xK_Left, xK_Right, xK_Up, xK_Down :: KeySym
xK_Return    = 0xff0d
xK_Escape    = 0xff1b
xK_Tab       = 0xff09
xK_BackSpace = 0xff08
xK_Delete    = 0xffff
xK_Left      = 0xff51
xK_Right     = 0xff53
xK_Up        = 0xff52
xK_Down      = 0xff54
