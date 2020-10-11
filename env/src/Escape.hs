module Escape where

import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Prelude
import Text.ShellEscape (Bash, escape, bytes)

-- | Escapes ASCII string as a bash literal. Might work for UTF-8, not
-- certain.
stringToBashLiteral :: String -> String
stringToBashLiteral = unpack . decodeUtf8 . bytes @Bash . escape . encodeUtf8 . pack
