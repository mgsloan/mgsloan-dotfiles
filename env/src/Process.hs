module Process
  ( spawn
  , syncSpawn
  , syncSpawnAndRead
  ) where

import Control.Monad
import RIO
import RIO.Process
import qualified Data.Text as T

import Monad

spawn :: FilePath -> [String] -> MX ()
spawn cmd args =
  void $ forkMX $ syncSpawn cmd args

syncSpawn :: FilePath -> [String] -> ReaderT Env IO ()
syncSpawn cmd args =
  proc cmd args $ runProcess_ . setStdin closed

syncSpawnAndRead :: FilePath -> [String] -> ReaderT Env IO String
syncSpawnAndRead cmd args =
  proc cmd args $
    fmap lazyBytesToString . readProcessStdout_ . setStdin closed

lazyBytesToString :: LByteString -> String
lazyBytesToString = bytesToString . toStrictBytes

bytesToString :: ByteString -> String
bytesToString = T.unpack . decodeUtf8Lenient
