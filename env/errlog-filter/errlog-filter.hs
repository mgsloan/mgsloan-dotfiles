{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (catch, throwIO)
import Control.Monad (forever)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Network.HostName (getHostName)
import System.Exit (exitSuccess)
import System.IO (hSetBuffering, hSetBinaryMode, BufferMode(LineBuffering), stdout, stdin)
import System.IO.Error (isEOFError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
  hostName <- getHostName
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout LineBuffering
  forever $ do
    ln <- BS8.hGetLine stdin `catch` \e ->
      if isEOFError e
        then exitSuccess
        else throwIO e
    let errorType = getErrorType hostName ln
    case errorType of
      Unknown -> do
        BS8.hPutStr stdout "(error) "
        BS8.hPutStrLn stdout ln
      Uncertain -> do
        BS8.hPutStr stdout "(warn) "
        BS8.hPutStrLn stdout ln
      Ignore -> return ()

data ErrorType
  -- | Error is of unknown type.
  = Unknown
  -- | Error has been investigated, but unsure whether it is harmless.
  | Uncertain
  -- | Error has been investigated, and the conclusion was that it's
  -- probably harmless.
  | Ignore
  deriving Eq

getErrorType :: String -> BS.ByteString -> ErrorType
getErrorType hostName ln = fromMaybe Unknown $
  case programName of
    "kernel" -> lookup message
        -- See
        -- https://bugs.launchpad.net/ubuntu/+source/linux/+bug/1584407
        -- I did not see an explanation for this error. Some kernels
        -- apparently even match and hide this error. Linus noticed it
        -- too and the only response was that they should probably be
        -- debug logs: https://lkml.org/lkml/2016/10/5/299
      [ ("ACPI Error: [\\_SB_.PCI0.XHC_.RHUB.HS11] Namespace lookup failure, AE_NOT_FOUND (20170831/dswload-210)", Ignore)
      , ("ACPI Exception: AE_NOT_FOUND, During name lookup/catalog (20170831/psobject-252)", Ignore)
      , ("ACPI Exception: AE_NOT_FOUND, (SSDT:ProjSsdt) while loading table (20170831/tbxfload-228)", Ignore)
      , ("ACPI Error: 1 table load failures, 12 successful (20170831/tbxfload-246)", Ignore)
        -- Based on googling probably some UEFI secure boot
        -- fiddliness, uncertain if it is a real issue.
      , ("Couldn't get size: 0x800000000000000e", Uncertain)
        -- Likely due to secure boot + proprietary drivers, doesn't
        -- seem like something to be concerned about.
        -- https://unix.stackexchange.com/a/455230
      , ("PKCS#7 signature not signed with a trusted key", Ignore)
      ]
    "wpa_supplicant" -> lookup message
        -- Couldn't easily google what these mean.  wpa_supplicant
        -- seems to work otherwise.
      [ ("dbus: wpa_dbus_get_object_properties: failed to get object properties: (none) none", Uncertain)
      , ("dbus: Failed to construct signal", Uncertain)
      ]
    "chrome"
        -- After a quick search only found one thing for this:
        -- https://askubuntu.com/questions/1110133/google-chrome-reports-odd-error-when-invoked-from-terminal
      | "Not implemented reached in virtual void libgtkui::X11InputMethodContextImplGtk::SetSurroundingText(const base::string16 &, const gfx::Range &)" `BS.isSuffixOf` message -> Just Uncertain
        -- Seems like ibus is used for entering non-Latin characters,
        -- which isn't something I need much, so ignoring this one.
      | "Unable to connect to ibus: Could not connect: Connection refused" `BS.isSuffixOf` message -> Just Uncertain
    _ -> Nothing
  where
    (_, afterHostName) = breakOn (" " <> fromString hostName <> " ") ln
    (unit, message) = breakOn ": " afterHostName
    programName = BS.takeWhile (/= fromIntegral (ord '[')) unit

-- | Like breakSubstring, but doesn't include the string searched for
-- in the snd result.
breakOn :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
breakOn needle haystack =
  (before, BS.drop (BS.length needle) after)
  where
    (before, after) = BS.breakSubstring needle haystack
