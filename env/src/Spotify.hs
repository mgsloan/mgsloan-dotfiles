module Spotify where

import XMonad

spotify :: String -> X ()
spotify cmd = spawn $
  "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++
  cmd

spotifyTogglePlay :: X ()
spotifyTogglePlay = spotify "PlayPause"

spotifyNext :: X ()
spotifyNext = spotify "Next"

spotifyPrevious :: X ()
spotifyPrevious = spotify "Previous"
