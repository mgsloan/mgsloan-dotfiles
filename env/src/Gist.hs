module Gist where

import XMonad
import XMonad.Util.Run

import Constants

-- Clipboard gists via https://github.com/defunkt/gist
gistFromClipboard :: String -> X ()
gistFromClipboard filename = do
  url <- runProcessWithInput "gist" (words "-P -p -f" ++ [filename]) ""
  liftIO $ putStrLn $ "Gist url from clipboard: " ++ show url
  spawn (browser ++ " " ++ url)
