{-# LANGUAGE OverloadedStrings #-}
-- | Wire codec tests.
--
-- The protocol stack cannot be exercised end to end without a running river,
-- so the encoder and decoder are pinned here against byte sequences derived by
-- hand from the wire format specification.
module Main (main) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import System.Exit (exitFailure)
import Data.Word (Word16)
import qualified Data.ByteString as BS

import XMonad.Layout
import XMonad.River.Wire
import XMonad.River.X11Compat

main :: IO ()
main = do
  results <- mapM runTest (tests ++ tallWheelTests)
  unless (and results) exitFailure

runTest :: (String, Bool) -> IO Bool
runTest (name, ok) = do
  putStrLn ((if ok then "PASS " else "FAIL ") ++ name)
  pure ok

tests :: [(String, Bool)]
tests =
  [ ( "empty message is header only"
    , enc (ObjectId 1) 0 mempty
        == BS.pack [1,0,0,0, 0,0, 8,0] )

    -- size 8+4=12 in the high 16 bits, opcode 3 in the low.
  , ( "single uint argument"
    , enc (ObjectId 2) 3 (argUInt 0xdeadbeef)
        == BS.pack [2,0,0,0, 3,0, 12,0, 0xef,0xbe,0xad,0xde] )

    -- "ab" is 3 bytes with the NUL, padded to 4.
  , ( "string is NUL terminated and padded"
    , enc (ObjectId 1) 0 (argString (Just "ab"))
        == BS.pack [1,0,0,0, 0,0, 16,0, 3,0,0,0, 0x61,0x62,0,0] )

    -- A 4 byte string needs 5 with the NUL, so pads to 8.
  , ( "string padding rounds up past the NUL"
    , BS.length (enc (ObjectId 1) 0 (argString (Just "abcd"))) == 8 + 4 + 8 )

  , ( "null string is a bare zero length"
    , enc (ObjectId 1) 0 (argString Nothing)
        == BS.pack [1,0,0,0, 0,0, 12,0, 0,0,0,0] )

  , ( "framing splits concatenated messages"
    , let a = enc (ObjectId 1) 0 (argUInt 7)
          b = enc (ObjectId 2) 1 (argString (Just "hi"))
          (msgs, rest) = splitMessages (a <> b)
      in map (\(h, _) -> (msgObject h, msgOpcode h)) msgs
           == [(ObjectId 1, 0), (ObjectId 2, 1)]
         && BS.null rest )

    -- The property the connection layer relies on: accumulating messages with
    -- <> and materialising once must produce exactly the bytes that encoding
    -- each separately and concatenating would, so that batching is invisible
    -- on the wire.
  , ( "batched messages are byte-identical to separately encoded ones"
    , let batched = runEncoded
            ( encodeMessage (ObjectId 1) 0 (argUInt 7)
           <> encodeMessage (ObjectId 2) 1 (argString (Just "hi"))
           <> encodeMessage (ObjectId 3) 2 mempty )
          separate = enc (ObjectId 1) 0 (argUInt 7)
                  <> enc (ObjectId 2) 1 (argString (Just "hi"))
                  <> enc (ObjectId 3) 2 mempty
          (msgs, rest) = splitMessages batched
      in batched == separate
         && map (\(h, _) -> (msgObject h, msgOpcode h)) msgs
              == [(ObjectId 1, 0), (ObjectId 2, 1), (ObjectId 3, 2)]
         && BS.null rest )

  , ( "an empty batch produces no bytes"
    , BS.null (runEncoded mempty) )

  , ( "framing holds back a partial message"
    , let full = enc (ObjectId 1) 0 (argUInt 7)
          (msgs, rest) = splitMessages (BS.init full)
      in null msgs && rest == BS.init full )

  , ( "roundtrip: uint, int, string"
    , let body = bodyOf (enc (ObjectId 1) 0
                   (argUInt 42 <> argInt (-7) <> argString (Just "river")))
      in decodeBody ((,,) <$> getWord32 <*> getInt <*> getString) body
           == Right (42, -7, "river") )

  , ( "roundtrip: nullable string present and absent"
    , let encStr s = bodyOf (enc (ObjectId 1) 0 (argString s))
      in decodeBody getStringMaybe (encStr (Just "x")) == Right (Just "x")
         && decodeBody getStringMaybe (encStr Nothing) == Right Nothing )

  , ( "roundtrip: fixed point survives eighths"
    , let body = bodyOf (enc (ObjectId 1) 0 (argFixed 1.125))
      in decodeBody getFixed body == Right 1.125 )

    -- Forward compatibility: a newer server may append arguments this build
    -- does not know about, and the decoder must ignore the excess.
  , ( "decoding tolerates trailing arguments"
    , let body = bodyOf (enc (ObjectId 1) 0 (argUInt 1 <> argUInt 2))
      in decodeBody getWord32 body == Right 1 )

  , ( "decoding a truncated body fails rather than reading past the end"
    , case decodeBody ((,) <$> getWord32 <*> getWord32) (BS.pack [1,0,0,0]) of
        Left _  -> True
        Right _ -> False )
  ]

-- | Encode a message to bytes. 'encodeMessage' now returns an 'Encoded' so
-- that batches share one buffer, so tests materialise explicitly.
enc :: ObjectId -> Word16 -> Encoded -> ByteString
enc oid opcode = runEncoded . encodeMessage oid opcode

bodyOf :: ByteString -> ByteString
bodyOf = BS.drop headerSize

--------------------------------------------------------------------------------
-- TallWheel

-- | The config's own layout, copied from src/TallWheel.hs unchanged apart from
-- dropping the xmonad imports. Its presence here is the point: a custom layout
-- is ordinary pure code that river's window management protocol can drive,
-- which is the thing sway cannot offer at all.
tileWheel :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tileWheel f r nmaster n
  | n <= nmaster || nmaster == 0 = splitVertically n r
  | otherwise =
      reverse (splitVertically nmaster r1) ++ splitVertically (n - nmaster) r2
  where (r1, r2) = splitHorizontallyBy f r

screen :: Rectangle
screen = Rectangle 0 0 1920 1080

tallWheelTests :: [(String, Bool)]
tallWheelTests =
  [ ( "TallWheel: a single window fills the screen"
    , tileWheel 0.5 screen 1 1 == [screen] )

  , ( "TallWheel: master takes the left fraction"
    , case tileWheel 0.5 screen 1 2 of
        [master, stack] ->
          rect_width master == 960 && rect_x master == 0
            && rect_width stack == 960 && rect_x stack == 960
        _ -> False )

    -- The whole point of TallWheel over Tall: the master side is reversed, so
    -- cycling focus walks a wheel rather than bouncing between two stacks.
  , ( "TallWheel: the master column is reversed relative to Tall"
    , let ws = tileWheel 0.5 screen 2 4
          masters = take 2 ws
      in map rect_y masters == [540, 0] )

  , ( "TallWheel: rectangles exactly tile the screen with no gaps"
    , let ws = tileWheel 0.61803 screen 1 5
          area r = fromIntegral (rect_width r) * fromIntegral (rect_height r)
      in sum (map area ws) == (1920 * 1080 :: Integer) )

  , ( "TallWheel: golden ratio split matches the config's phi"
    , case tileWheel 0.61803 screen 1 2 of
        [master, _] -> rect_width master == 1186
        _ -> False )
  ]
