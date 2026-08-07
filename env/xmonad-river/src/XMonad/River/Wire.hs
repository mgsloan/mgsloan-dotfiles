{-# LANGUAGE ScopedTypeVariables #-}

-- | The Wayland wire protocol, encoder and decoder.
--
-- This is a from-scratch implementation rather than a binding to
-- @libwayland-client@. That is viable here because the river window
-- management protocols pass no file descriptors, which is the only part of
-- the wire format needing @SCM_RIGHTS@ and hence C support. Avoiding
-- libwayland means @xmonad-river@ has no C dependencies at all, and that the
-- event dispatch loop is ordinary Haskell rather than a callback jungle
-- reached through @wl_proxy_marshal@'s variadic interface.
--
-- Serialization uses @store-core@'s 'Poke' and 'Peek'. The fit is unusually
-- good: Wayland's wire format is host-endian and 32-bit aligned, which is
-- exactly what 'pokeStorable' and 'peekStorable' do natively. @binary@ and
-- @cereal@ would both need every field routed through host-endian escape
-- hatches. Note that @store@'s derived 'Data.Store.Store' instances are *not*
-- wire compatible with Wayland — only the low level primitives from
-- "Data.Store.Core" are used here, which is also why the dependency is on
-- @store-core@ rather than the much heavier @store@.
--
-- Wire format, for reference (all values host-endian, 32-bit aligned):
--
-- > message := object_id : u32
-- >            (size : u16 << 16) | (opcode : u16)
-- >            arguments...
--
-- where @size@ counts the 8 byte header as well.
module XMonad.River.Wire
  ( -- * Object ids
    ObjectId(..)
  , nullObject
  , isNullObject
    -- * Encoded bytes
  , Encoded
  , runEncoded
  , argInt
  , argUInt
  , argFixed
  , argObject
  , argString
  , argArray
  , argNewIdAny
  , Fixed
  , toFixed
  , fromFixed
    -- * Encoding
  , encodeMessage
    -- * Decoding
  , decodeBody
  , getWord32
  , getInt
  , getFixed
  , getString
  , getStringMaybe
  , getObject
  , getArray
    -- * Message framing
  , MessageHeader(..)
  , headerSize
  , splitMessages
  ) where

import Control.Monad (when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Word (Word16, Word32, Word8)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Data.Store.Core
  ( Peek(..), PeekException, PeekResult(..), Poke(..), decodeWith
  , peekStateEndPtr, peekStorableTy, pokeStatePtr, pokeStorable
  , tooManyBytes, unsafeEncodeWith
  )

--------------------------------------------------------------------------------
-- Object ids

-- | A Wayland protocol object id. Ids below 0xff000000 are allocated by the
-- client, ids at or above it by the server.
newtype ObjectId = ObjectId { unObjectId :: Word32 }
  deriving (Eq, Ord)

instance Show ObjectId where
  show (ObjectId n) = '#' : show n

-- | The null object, used for nullable @object@ arguments.
nullObject :: ObjectId
nullObject = ObjectId 0

isNullObject :: ObjectId -> Bool
isNullObject = (== 0) . unObjectId

--------------------------------------------------------------------------------
-- Fixed point

-- | Wayland's 24.8 signed fixed point number, surfaced as a 'Double' since
-- nothing in the river protocols is sensitive to the exact representation.
type Fixed = Double

toFixed :: Fixed -> Int32
toFixed d = round (d * 256)

fromFixed :: Int32 -> Fixed
fromFixed n = fromIntegral n / 256

--------------------------------------------------------------------------------
-- Arguments

-- | Bytes destined for the wire: how many there are, and how to write them.
--
-- Size and writer are deliberately bundled into one value rather than being
-- computed by two functions over an argument ADT. Keeping a @sizeOfArg@ and a
-- @pokeArg@ in step by hand is precisely the kind of duplication that produces
-- buffer overruns when a new argument type is added and only one of the two is
-- updated. Here it is not expressible.
--
-- The same type describes an argument, an argument list, a whole message, and
-- a batch of messages, since all four are just "this many bytes, written like
-- so". That is what lets a flush encode every pending request into one buffer
-- with a single allocation and no byte written twice: see
-- 'XMonad.River.Connection.flush'.
--
-- Combine with '<>':
--
-- > encodeMessage obj 3 (argInt x <> argString (Just name) <> argObject other)
--
-- There is no 'Show' instance as a result. Logging in the style of
-- @WAYLAND_DEBUG@ belongs in the generated bindings, which know each argument
-- statically, rather than here.
data Encoded = Encoded !Int (Poke ())

instance Semigroup Encoded where
  Encoded n p <> Encoded m q = Encoded (n + m) (p >> q)
  {-# INLINE (<>) #-}

instance Monoid Encoded where
  mempty = Encoded 0 (pure ())
  {-# INLINE mempty #-}

-- | Materialise into a 'ByteString', allocating exactly once.
runEncoded :: Encoded -> ByteString
runEncoded (Encoded n p) = unsafeEncodeWith p n

-- | @int@
argInt :: Int32 -> Encoded
argInt = Encoded 4 . pokeStorable

-- | @uint@, including @enum@ and bitfield arguments.
argUInt :: Word32 -> Encoded
argUInt = Encoded 4 . pokeStorable

-- | @fixed@
argFixed :: Fixed -> Encoded
argFixed = Encoded 4 . pokeStorable . toFixed

-- | @object@, or @new_id@ with a statically known interface — the same 32-bit
-- id on the wire. Pass 'nullObject' for nullable arguments.
argObject :: ObjectId -> Encoded
argObject = Encoded 4 . pokeStorable . unObjectId

-- | @string@. Length-prefixed with a length that includes the NUL terminator,
-- then padded to a 4 byte boundary. 'Nothing', encoded as a zero length, is
-- only valid for arguments the XML marks nullable.
argString :: Maybe ByteString -> Encoded
argString Nothing = Encoded 4 (pokeStorable (0 :: Word32))
argString (Just s) = Encoded (4 + pad4 len) $ do
    pokeStorable (fromIntegral len :: Word32)
    pokeBytes s
    pokeZeros (pad4 len - len + 1)  -- NUL terminator plus padding
  where
    len = BS.length s + 1

-- | @array@
argArray :: ByteString -> Encoded
argArray bs = Encoded (4 + pad4 len) $ do
    pokeStorable (fromIntegral len :: Word32)
    pokeBytes bs
    pokeZeros (pad4 len - len)
  where
    len = BS.length bs

-- | @new_id@ with no interface named in the XML, as used by
-- @wl_registry.bind@: interface name, version, then the id.
argNewIdAny :: ByteString -> Word32 -> ObjectId -> Encoded
argNewIdAny iface version oid =
  argString (Just iface) <> argUInt version <> argObject oid

--------------------------------------------------------------------------------
-- Encoding

-- | Prefix an argument list with its message header.
--
-- Returns 'Encoded' rather than 'ByteString' so that a caller batching several
-- messages pays for one allocation covering all of them, instead of one per
-- message plus a concatenation. The size field is computed from the arguments,
-- so callers never have to think about it or about padding.
encodeMessage :: ObjectId -> Word16 -> Encoded -> Encoded
encodeMessage (ObjectId oid) opcode (Encoded argsSize pokeArgs) =
    Encoded totalSize poke
  where
    totalSize = headerSize + argsSize
    poke = do
      pokeStorable oid
      pokeStorable ((fromIntegral totalSize `shiftL` 16) .|. fromIntegral opcode :: Word32)
      pokeArgs

-- | Copy a 'ByteString' into the output buffer.
pokeBytes :: ByteString -> Poke ()
pokeBytes bs = Poke $ \ps off ->
  BSU.unsafeUseAsCStringLen bs $ \(src, len) -> do
    copyBytes (pokeStatePtr ps `plusPtr` off) (castPtr src :: Ptr Word8) len
    pure (off + len, ())

-- | Write @n@ zero bytes, for NUL terminators and padding.
pokeZeros :: Int -> Poke ()
pokeZeros n = Poke $ \ps off -> do
  fillBytes (pokeStatePtr ps `plusPtr` off :: Ptr Word8) 0 n
  pure (off + n, ())

-- | Round up to the next multiple of four.
pad4 :: Int -> Int
pad4 n = (n + 3) .&. (-4)

--------------------------------------------------------------------------------
-- Decoding

-- | Decode a message body with the 'Peek' the generated code supplies for that
-- interface and opcode.
--
-- Trailing bytes are tolerated rather than treated as an error: a server is
-- permitted to send arguments added in a protocol version newer than the one
-- this client was generated against, and ignoring them is how a client stays
-- forward compatible.
decodeBody :: Peek a -> ByteString -> Either PeekException a
decodeBody p = decodeWith (p <* skipRest)

-- | Consume whatever is left of the buffer.
skipRest :: Peek ()
skipRest = Peek $ \ps _ptr -> pure (PeekResult (peekStateEndPtr ps) ())

-- | A @uint@, and the basis of every other fixed-width getter.
getWord32 :: Peek Word32
getWord32 = peekStorableTy "Word32"

getInt :: Peek Int32
getInt = fromIntegral <$> getWord32

getFixed :: Peek Fixed
getFixed = fromFixed . fromIntegral <$> getWord32

-- | An @object@ or @new_id@ argument.
getObject :: Peek ObjectId
getObject = ObjectId <$> getWord32

-- | A non-nullable string. The NUL terminator and padding are consumed but not
-- returned.
getString :: Peek ByteString
getString = do
  ms <- getStringMaybe
  case ms of
    Just s  -> pure s
    Nothing -> Peek $ \_ _ -> tooManyBytes 1 0 "non-null string"

-- | A nullable string.
getStringMaybe :: Peek (Maybe ByteString)
getStringMaybe = do
  len <- fromIntegral <$> getWord32
  if len == (0 :: Int)
    then pure Nothing
    -- len includes the NUL terminator, which is dropped; the remainder of the
    -- 4 byte padding is skipped.
    else Just <$> peekBytesPadded (len - 1) (pad4 len - len + 1)

getArray :: Peek ByteString
getArray = do
  len <- fromIntegral <$> getWord32
  peekBytesPadded len (pad4 len - len)

-- | Read @len@ bytes, then skip @skip@ further bytes of padding.
peekBytesPadded :: Int -> Int -> Peek ByteString
peekBytesPadded len skip = Peek $ \ps ptr -> do
  let remaining = peekStateEndPtr ps `minusPtr` ptr
  when (len + skip > remaining) $ tooManyBytes (len + skip) remaining "ByteString"
  bs <- BS.packCStringLen (castPtr ptr, len)
  pure (PeekResult (ptr `plusPtr` (len + skip)) bs)

--------------------------------------------------------------------------------
-- Framing

data MessageHeader = MessageHeader
  { msgObject :: !ObjectId
  , msgOpcode :: !Word16
  , msgSize   :: !Int
    -- ^ Total size, including the 8 byte header.
  } deriving (Eq, Show)

headerSize :: Int
headerSize = 8

-- | Split a buffer into as many complete messages as it contains, returning
-- the leftover bytes. Each message is paired with its body, header stripped.
--
-- Framing is done with plain 'ByteString' slicing rather than a 'Peek', since
-- it must cope with a truncated buffer without throwing.
splitMessages :: ByteString -> ([(MessageHeader, ByteString)], ByteString)
splitMessages = go id
  where
    go acc bs
      | BS.length bs < headerSize = (acc [], bs)
      | msgSize h < headerSize    = (acc [], BS.empty)  -- corrupt stream; drop
      | BS.length bs < msgSize h  = (acc [], bs)
      | otherwise =
          let body = BS.take (msgSize h - headerSize) (BS.drop headerSize bs)
          in go (acc . ((h, body) :)) (BS.drop (msgSize h) bs)
      where h = peekHeader bs

-- | Read a header from the front of a buffer known to hold at least
-- 'headerSize' bytes. Safe to call unchecked only because 'splitMessages'
-- verifies the length first.
peekHeader :: ByteString -> MessageHeader
peekHeader bs =
    case decodeWith (headerPeek <* skipRest) bs of
      Right h -> h
      -- Unreachable: the caller has already checked the length, and the peek
      -- reads exactly 'headerSize' bytes.
      Left e  -> error ("XMonad.River.Wire.peekHeader: " ++ show e)
  where
    headerPeek = do
      oid <- getWord32
      sizeAndOpcode <- getWord32
      pure MessageHeader
        { msgObject = ObjectId oid
        , msgOpcode = fromIntegral (sizeAndOpcode .&. 0xffff)
        , msgSize   = fromIntegral (sizeAndOpcode `shiftR` 16)
        }
