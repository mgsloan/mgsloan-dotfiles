-- | Cursor theme selection.
module XMonad.Util.Cursor
  ( setDefaultCursor
  , xC_left_ptr
  ) where

import Control.Monad (forM_)
import Control.Monad.Reader (asks)
import Data.IORef (readIORef)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M

import XMonad.Core
import XMonad.River.Protocol.WindowManagement (riverSeatV1SetXcursorTheme)

-- | X11 named a cursor /shape/; Wayland names a cursor /theme/. There is no
-- per-shape selection in the river protocol, so the argument is treated as a
-- theme name and applied to every seat.
setDefaultCursor :: String -> X ()
setDefaultCursor theme = do
  conn <- asks riverConn
  seats <- io . readIORef =<< asks riverSeats
  forM_ (M.keys seats) $ \seat ->
    io (riverSeatV1SetXcursorTheme conn seat (BC.pack theme) 24)

-- | The default theme name, standing in for X11's @XC_left_ptr@.
xC_left_ptr :: String
xC_left_ptr = "default"
