-- Modified from
-- https://github.com/giovanifss/xmonad-brightness/blob/ec9e4196872c6b2573399e0db980f2ecbc2be4fc/src/XMonad/Util/Brightness.hs
-- Doesn't really match how I'd write it, but it works fine

-- | Module to control the brightness of the screen in linux environments
module Brightness
    ( increase
    , decrease
    , brightest
    , set
    , change
    ) where

import XMonad
import Data.Traversable (traverse)
import System.IO (hPutStrLn, stderr)
import Data.Bitraversable (bitraverse)
import Control.Monad (join)
import Data.Bifunctor (first)
import Control.Exception (try)
import Control.Applicative (liftA2)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString as BS

minBrightness :: Int
minBrightness = 1

brightnessRate :: Int
brightnessRate = 30

maxfile, currentfile :: String
maxfile = "/sys/class/backlight/intel_backlight/max_brightness"
currentfile = "/sys/class/backlight/intel_backlight/brightness"

-- | Update brightness by 30
increase :: X ()
increase = liftIO $ change (+ brightnessRate) *> (pure ())

-- | Update brightness by -30
decrease :: X ()
decrease = liftIO $ change (subtract brightnessRate) *> (pure ())

brightest :: X ()
brightest = set 1000000000

set :: Int -> X ()
set x = liftIO $ change (const x) *> (pure ())

-- | Perform all needed IO to update screen brightness
change :: (Int -> Int) -> IO (Either () ())
change f = do
  maxBrightness <- getFromFile maxfile readInt
  current <- getFromFile currentfile readInt
  printError =<< apply (writeToFile currentfile) (liftA2 (guard f minBrightness) maxBrightness current)

apply :: (Int -> IO (Either String ())) -> Either String Int -> IO (Either String ())
apply f = fmap join . traverse f

guard :: (Int -> Int) -> Int -> Int -> Int -> Int
guard f minima maxima current
  | value > maxima = maxima
  | value < minima = minima
  | otherwise = value
  where value = f current

readInt :: BS.ByteString -> Either String Int
readInt str = case (reads (unpack str)) of
                [(n, "\n")] -> Right n
                [(n, "")]   -> Right n
                _           -> Left "Could not parse string to int"

printError :: Either String e -> IO (Either () e)
printError = bitraverse (hPutStrLn stderr) (pure . id)

getFromFile :: FilePath -> (BS.ByteString -> Either String a) -> IO (Either String a)
getFromFile filename fcast = fmap (fcast =<<) (try' $ BS.readFile filename)

writeToFile :: FilePath -> Int -> IO (Either String ())
writeToFile filename value = try' $ writeFile filename (show value)

try' :: forall a . IO a -> IO (Either String a)
try' x = fmap (first show) (try x :: IO (Either IOError a))
