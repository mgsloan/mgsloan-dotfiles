module Spotify where

import Imports

spotifyTogglePlay :: MX ()
spotifyTogglePlay = spotify "PlayPause"

spotifyNext :: MX ()
spotifyNext = spotify "Next"

spotifyPrevious :: MX ()
spotifyPrevious = spotify "Previous"

spotify :: String -> MX ()
spotify cmd =
  spawn
    "dbus-send"
    [ "--print-reply"
    , "--dest=org.mpris.MediaPlayer2.spotify"
    , "/org/mpris/MediaPlayer2"
    , "org.mpris.MediaPlayer2.Player." ++ cmd
    ]
