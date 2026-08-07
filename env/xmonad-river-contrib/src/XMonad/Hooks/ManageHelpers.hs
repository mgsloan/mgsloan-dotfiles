-- | Predicates for manage hooks.
module XMonad.Hooks.ManageHelpers
  ( isDialog
  , isNotification
  , isFullscreen
  , pid
  , transience
  , transience'
  , doCenterFloat
  , (-?>)
  , composeOne
  ) where

import Control.Monad.Reader (ask, asks)
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import Data.Monoid (Endo(..))
import System.Posix.Types (ProcessID)
import qualified Data.Map.Strict as M

import XMonad.Core
import XMonad.ManageHook (doF)
import qualified XMonad.StackSet as W

-- | Look up the current window's accumulated river state.
askRiverWindow :: Query (Maybe RiverWindow)
askRiverWindow = do
  w <- ask
  liftQuery $ do
    ref <- asks riverWindows
    M.lookup w <$> io (readIORef ref)

-- | A window with a parent is a dialog.
--
-- X11 signalled this with @_NET_WM_WINDOW_TYPE_DIALOG@. Wayland has no window
-- type atom; what it has is @xdg_toplevel.set_parent@, surfaced as
-- @river_window_v1.parent@. A parented toplevel is what a dialog /is/ under
-- xdg-shell, so this is the faithful translation rather than an approximation.
isDialog :: Query Bool
isDialog = maybe False (isJust . rwParent) <$> askRiverWindow

-- | Always 'False'.
--
-- Notifications are not toplevels under Wayland — they are layer-shell
-- surfaces, which the window manager never sees as windows. A notification
-- daemon's popups therefore need no manage hook at all.
isNotification :: Query Bool
isNotification = pure False

-- | Whether the window is fullscreen. Not implemented.
--
-- The information exists — river sends @fullscreen_requested@ and the window
-- manager answers with @inform_fullscreen@ — but it arrives as an event to
-- respond to rather than as state hanging off the window, so tracking it means
-- holding a fullscreen set in 'XConf'. Until then this reports 'False'.
isFullscreen :: Query Bool
isFullscreen = do
  liftQuery $ warnUnimplemented "isFullscreen"
    "Always reports False, so manage hook rules keyed on it will never fire. \
    \river reports fullscreen via the fullscreen_requested event, which is \
    \not yet tracked."
  pure False

-- | The process that created the window, from
-- @river_window_v1.unreliable_pid@.
--
-- The name in the protocol is a warning worth repeating: PIDs are reused, and
-- one process may own many windows. Fine for routing a window to a workspace,
-- not fine for anything security sensitive.
pid :: Query (Maybe ProcessID)
pid = fmap (fmap fromIntegral . rwPid =<<) askRiverWindow

-- | Move a transient window to the workspace holding its parent.
--
-- Implemented against @river_window_v1.parent@, which is
-- @xdg_toplevel.set_parent@ — the Wayland counterpart of the @WM_TRANSIENT_FOR@
-- hint this originally read.
transience :: ManageHook
transience = do
  mParent <- fmap rwParent <$> askRiverWindow
  case mParent of
    Just (Just parent) -> liftQuery $ withWindowSet $ \ws ->
      pure $ case W.findTag parent ws of
        Just i  -> Endo (W.shift i)
        Nothing -> mempty
    _ -> pure mempty

-- | As 'transience', and identical here: the xmonad version differs only in
-- how it composes with 'composeOne', which is unaffected by the backend.
transience' :: ManageHook
transience' = transience

doCenterFloat :: ManageHook
doCenterFloat = ask >>= \w -> doF (W.float w (W.RationalRect 0.25 0.25 0.5 0.5))

infixr 0 -?>

(-?>) :: Query Bool -> Query (Endo a) -> Query (Maybe (Endo a))
p -?> f = p >>= \b -> if b then Just <$> f else pure Nothing

composeOne :: [Query (Maybe (Endo a))] -> Query (Endo a)
composeOne = foldr step (pure mempty)
  where
    step q rest = q >>= maybe rest pure
