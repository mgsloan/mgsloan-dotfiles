module Prompt where

import qualified Data.Map as M
import XMonad
import XMonad.Prompt

xpconfig :: Bool -> XPConfig
xpconfig auto
    | auto = res { autoComplete = Just 1000 }
    | otherwise = res
  where
    res = def
        { font              = "xft:Hack:pixelsize=18"
        , bgColor           = "black"
        , fgColor           = "white"
        , bgHLight          = "gray"
        , fgHLight          = "black"
        , borderColor       = "orange"
        , promptBorderWidth = 1
        , position          = Bottom
        , height            = 32
        , historySize       = 1000
        , promptKeymap      = km
        }
    km =
      M.insert (controlMask, xK_Right) (moveWord Next) $
      M.insert (controlMask, xK_Left) (moveWord Prev) $
      emacsLikeXPKeymap
