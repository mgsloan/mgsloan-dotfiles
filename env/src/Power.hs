module Power where

import qualified Data.Text as T
import qualified RIO.ByteString as BS

import Imports

checkAcConnected :: Xio Bool
checkAcConnected = do
  let fp = "/sys/class/power_supply/AC/online"
  eres <- tryAny $ fmap decodeUtf8Lenient $ BS.readFile fp
  case eres of
    Left e -> do
      logError $ mconcat
        [ "Expected to be able to read "
        , fromString (show fp)
        , ", but encountered exception:\n"
        , fromString (show e)
        ]
      return False
    Right (headMay . T.words -> Just status)
      | status == "0" -> return False
      | status == "1" -> return True
    Right contents -> do
      logError $ mconcat
         [ "Expected "
         , fromString (show fp)
         , " to contain just 0 or 1, but instead it contains "
         , display contents
         ]
      return False
