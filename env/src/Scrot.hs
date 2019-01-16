module Scrot where

import Imports

scrot :: Xio ()
scrot = do
  -- Without a sleep, scrot fails to grab keyboard
  threadDelay (200 * 1000)
  homeDir <- view envHomeDir
  syncSpawn "scrot"
    [ homeDir </> "pics/screenshots/%F_%T.png"
    , "--select"
    , "--exec", browser ++ " $f"
    ]
