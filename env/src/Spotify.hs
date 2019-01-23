module Spotify where

import Imports

spotifyTogglePlay :: (MonadIO m, MonadReader Env m) => m ()
spotifyTogglePlay = spotify "PlayPause"

spotifyNext :: (MonadIO m, MonadReader Env m) => m ()
spotifyNext = spotify "Next"

spotifyPrevious :: (MonadIO m, MonadReader Env m) => m ()
spotifyPrevious = spotify "Previous"

spotifyStop :: (MonadIO m, MonadReader Env m) => m ()
spotifyStop = spotify "Stop"

spotify :: (MonadIO m, MonadReader Env m) => String -> m ()
spotify cmd =
  spawn
    "dbus-send"
    [ "--print-reply"
    , "--dest=org.mpris.MediaPlayer2.spotify"
    , "/org/mpris/MediaPlayer2"
    , "org.mpris.MediaPlayer2.Player." ++ cmd
    ]

-- NOTE: Unfortunately, dbus does not support mpris volume
-- control.. if it ever does it will work like this:
--
-- dbus-send --print-reply \
--           --dest=org.mpris.MediaPlayer2.spotify \
--           /org/mpris/MediaPlayer2 \
--           org.freedesktop.DBus.Properties.Set \
--           string:org.mpris.MediaPlayer2.Player \
--           string:Volume \
--           variant:double:1
--
-- and to get the volume (this always yields 0 currently)
--
-- dbus-send --print-reply \
--           --dest=org.mpris.MediaPlayer2.spotify \
--           /org/mpris/MediaPlayer2 \
--           org.freedesktop.DBus.Properties.Get \
--           string:org.mpris.MediaPlayer2.Player \
--           string:Volume
