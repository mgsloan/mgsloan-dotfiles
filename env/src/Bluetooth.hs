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

bluetoothConnect :: Utf8Builder -> Lens' Env (Maybe Text) -> Xio ()
bluetoothConnect n l = withUuid n l $ \uuid ->
  sendToBluetoothCtl ["connect ", uuid, "Enter"]

bluetoothDisconnect :: Utf8Builder -> Lens' Env (Maybe Text) -> Xio ()
bluetoothDisconnect n l = withUuid n l $ \uuid ->
  sendToBluetoothCtl ["disconnect ", uuid, "Enter"]

withUuid :: Utf8Builder -> Lens' Env (Maybe Text) -> (String -> Xio ()) -> Xio ()
withUuid n l f = do
  muuid <- view l
  case muuid of
    Just uuid -> f (T.unpack uuid)
    Nothing -> logError $ mconcat
      ["Can't connect to ", n, ", as ", n, ".uuid doesn't exist"]

sendToBluetoothCtl :: [String] -> Xio ()
sendToBluetoothCtl keypresses =
  syncSpawn "tmux" $ ["send-keys", "-t", "bt"] ++ keypresses
