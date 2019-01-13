module Bluetooth where

import Control.Monad
import Data.List (partition, find, isInfixOf, stripPrefix)
import Data.Maybe
import Safe
import System.Process (readProcess)
import System.Process.Typed hiding (readProcess)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import Imports

-- | This reconnects bluetooth devices that match the specified
-- string, expected to be found infix within lines of the device
-- listing.
--
-- @sudo apt install bluez-tools@ to install necessary utilities.
reconnectBluetooth :: [String] -> IO ()
reconnectBluetooth matches = do
  deviceList <- fmap lines $ readProcess "bt-device" ["--list"] ""
  let (found, notFound)
        = partition (isJust . snd)
        $ map (\match -> (match, find (match `isInfixOf`) deviceList)) matches
  unless (null notFound) $ putStrLn $ concat
    [ "Didn't find bluetooth devices matching "
    , show (map fst notFound)
    , " in list:\n"
    , unlines deviceList
    ]
  putStrLn $ concat
    [ "Reconnecting the following bluetooth devices:\n"
    , unlines (map fst found)
    ]
  let (macParsed, macNotParsed)
        = partition (isJust . snd)
        $ map (\x -> (x, getMac x))
        $ map (fromJust . snd) found
  unless (null macNotParsed) $ putStrLn $ concat
    [ "Failed to parse MAC from some matching output of bt-device --list:\n"
    , unlines (map fst macNotParsed)
    ]
  {-
  -- This should work, but it doesn't, so instead driving bluetoothctl
  -- https://github.com/khvzak/bluez-tools/issues/24
  forM_ macParsed $ \(_, Just mac) ->
    callProcess "bt-device" ["--connect=" ++ mac]
  -}
  let connectLines = map (("connect " ++) . fromJust . snd) macParsed
      lbsInput
        = LBS.fromStrict
        $ encodeUtf8
        $ T.pack
        $ unlines
        $ connectLines ++ ["exit"]
  runProcess_ $
    setStdin (byteStringInput lbsInput) $
    proc "bluetoothctl" []

getMac :: String -> Maybe String
getMac str = lastMay (words str) >>= stripPrefix "(" >>= stripSuffix ")"

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = reverse <$> stripPrefix (reverse xs) (reverse ys)
