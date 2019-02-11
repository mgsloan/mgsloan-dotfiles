module Gist where

import Imports

-- Clipboard gists via https://github.com/defunkt/gist
gistFromClipboard :: String -> XX ()
gistFromClipboard filename = forkXio $ do
  url <- syncSpawnAndRead "gist" ["-P", "-p", "-f", filename]
  logInfo $ "Gist url from clipboard: " <> fromString url
  syncSpawn "xdg-open" [url]
