{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The module a configuration imports.
--
-- Re-exports the same surface as xmonad's @XMonad@, so that
-- @import XMonad@ in a config means what it always meant.
module XMonad
  ( module XMonad.Core
  , module XMonad.Layout
  , module XMonad.ManageHook
  , module XMonad.Operations
  , module XMonad.River.X11Compat
  , module XMonad.StackSet
  , Default(..)
  , xmonad
  ) where

import Data.Default.Class (Default(..))
import qualified Data.Map as M

import XMonad.Core
import XMonad.Layout
import XMonad.ManageHook
import XMonad.Operations
import XMonad.River.WM (riverMain)
import XMonad.River.X11Compat
import XMonad.StackSet (RationalRect(..))

-- | Run the window manager with the given configuration.
xmonad :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
xmonad conf = riverMain conf { layoutHook = Layout (layoutHook conf) }

-- | The default configuration.
--
-- The instance is an orphan for the same reason xmonad's is: 'Default' comes
-- from data-default-class and 'XConfig' from "XMonad.Core", and the default
-- layout it names lives here in "XMonad.Layout".
--
-- Differs from xmonad's in two fields that X11 required and river does not:
-- there are no event masks to select, and the terminal default follows the
-- Wayland convention rather than @xterm@.
instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) => Default (XConfig a) where
  def = XConfig
    { borderWidth        = 1
    , workspaces         = map show [1 .. 9 :: Int]
    , layoutHook         = Tall 1 (3/100) (1/2)
                       ||| Mirror (Tall 1 (3/100) (1/2))
                       ||| Full
    , terminal           = "foot"
    , normalBorderColor  = "#dddddd"
    , focusedBorderColor = "#ff0000"
    , modMask            = mod1Mask
    , keys               = \_ -> M.empty
    , logHook            = pure ()
    , startupHook        = pure ()
    , mouseBindings      = \_ -> M.empty
    , manageHook         = idHook
    , handleEventHook    = \_ -> mempty
    , focusFollowsMouse  = True
    , clickJustFocuses   = True
    , handleExtraArgs    = \args cnf -> case args of
        [] -> pure cnf
        _  -> fail ("unrecognised arguments: " ++ unwords args)
    , extensibleConf     = M.empty
    }
