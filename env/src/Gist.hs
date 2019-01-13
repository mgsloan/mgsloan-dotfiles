{-# LANGUAGE OverloadedStrings #-}

module Gist where

import Imports

-- Clipboard gists via https://github.com/defunkt/gist
gistFromClipboard :: String -> XX ()
gistFromClipboard filename = forkEnv $ do
  url <- syncSpawnAndRead "gist" ["-P", "-p", "-f", filename]
  logInfo $ "Gist url from clipboard: " <> fromString url
  syncSpawn "google-chrome" [url]
