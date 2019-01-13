module Spotify where

import Imports

spotifyTogglePlay :: XX ()
spotifyTogglePlay = spotify "PlayPause"

spotifyNext :: XX ()
spotifyNext = spotify "Next"

spotifyPrevious :: XX ()
spotifyPrevious = spotify "Previous"

spotify :: String -> XX ()
spotify cmd =
  spawn
    "dbus-send"
    [ "--print-reply"
    , "--dest=org.mpris.MediaPlayer2.spotify"
    , "/org/mpris/MediaPlayer2"
    , "org.mpris.MediaPlayer2.Player." ++ cmd
    ]
