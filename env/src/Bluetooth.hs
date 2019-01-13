-- | Utilities for controlling bluetooth.
--
-- Note that these only work when there is a tmux pane called btctl
-- that is running bluetoothctl. The approach using tmux comes from
-- https://serverfault.com/a/547144
--
-- TODO: Make it so that this starts the pane if it isn't already
-- running.
module Bluetooth where

import qualified Data.Text as T

import Imports

connectHeadphones :: Xio ()
connectHeadphones = withHeadphonesUuid $ \uuid ->
  sendToBluetoothCtl ["connect ", uuid, "Enter"]

disconnectHeadphones :: Xio ()
disconnectHeadphones = withHeadphonesUuid $ \uuid ->
  sendToBluetoothCtl ["disconnect ", uuid, "Enter"]

sendToBluetoothCtl :: [String] -> Xio ()
sendToBluetoothCtl keypresses =
  syncSpawn "tmux" $ ["send-keys", "-t", "btctl"] ++ keypresses

withHeadphonesUuid :: (String -> Xio ()) -> Xio ()
withHeadphonesUuid f = do
  muuid <- view envHeadphonesUuid
  case muuid of
    Nothing ->
      logError "Can't connect to headphones because no headphones.uuid file exists"
    Just uuid -> f (T.unpack uuid)
