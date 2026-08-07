-- | Combinators for writing manage hooks, matching xmonad's
-- @XMonad.ManageHook@.
--
-- The queries read river's accumulated window state rather than X properties.
-- Two mappings are worth knowing:
--
-- * 'className' and 'appName' both come from @river_window_v1.app_id@. X11
--   distinguished the two halves of @WM_CLASS@; Wayland has only @app_id@, so
--   they are the same string here. Config code matching on either still works,
--   but code relying on them differing will not.
-- * 'title' is @river_window_v1.title@, which is @xdg_toplevel.set_title@ —
--   the same string X11 clients put in @_NET_WM_NAME@.
module XMonad.ManageHook
  ( -- * Combinators
    (=?)
  , (-->)
  , composeAll
  , idHook
    -- * Queries
  , title
  , appName
  , className
  , stringProperty
  , willFloat
    -- * Actions
  , doF
  , doShift
  , doFloat
  , doIgnore
  , doSink
  ) where

import Control.Monad.Reader (ask, asks)
import Data.IORef (readIORef)
import Data.Monoid (Endo(..))
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M

import XMonad.Core
import qualified XMonad.StackSet as W

-- Matches xmonad: '-->' binds loosest, and '=?' is left at the default
-- infixl 9 so that @title =? "x" --> doShift "1"@ groups as intended.
infix 0 -->

-- | Compare a query's result against a value.
(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = fmap (== x) q

-- | Run the second argument only when the first yields 'True'.
(-->) :: Monoid m => Query Bool -> Query m -> Query m
p --> f = p >>= \b -> if b then f else mempty

composeAll :: Monoid m => [Query m] -> Query m
composeAll = mconcat

idHook :: Monoid m => m
idHook = mempty

-- | Look up the current window's accumulated river state.
askWindow :: Query (Maybe RiverWindow)
askWindow = do
  w <- ask
  liftQuery $ do
    ref <- asks riverWindows
    M.lookup w <$> io (readIORef ref)

title :: Query String
title = maybe "" (maybe "" BC.unpack . rwTitle) <$> askWindow

-- | River has no separate instance name; this is @app_id@, same as
-- 'className'.
appName :: Query String
appName = className

className :: Query String
className = maybe "" (maybe "" BC.unpack . rwAppId) <$> askWindow

-- | X11 property lookup. Not implemented.
--
-- Wayland has no window properties: there is no @_NET_WM_*@, no
-- @WM_WINDOW_ROLE@, and no generic key-value store on a surface. The
-- information X11 configs read this way either has a dedicated river event
-- ('title', 'className', 'XMonad.Hooks.ManageHelpers.pid') or does not exist.
stringProperty :: String -> Query String
stringProperty prop = do
  liftQuery $ warnUnimplemented ("stringProperty " ++ show prop)
    "Wayland has no window properties, so this always returns \"\". Any \
    \manage hook rule matching on it will never fire."
  pure ""

willFloat :: Query Bool
willFloat = do
  w <- ask
  liftQuery $ withWindowSet $ \ws -> pure (M.member w (W.floating ws))

doF :: (s -> s) -> Query (Endo s)
doF = pure . Endo

doShift :: WorkspaceId -> ManageHook
doShift i = doF (W.shift i)

doFloat :: ManageHook
doFloat = ask >>= \w -> doF (W.float w (W.RationalRect 0.25 0.25 0.5 0.5))

doIgnore :: ManageHook
doIgnore = ask >>= \w -> doF (W.delete w)

doSink :: ManageHook
doSink = ask >>= \w -> doF (W.sink w)
