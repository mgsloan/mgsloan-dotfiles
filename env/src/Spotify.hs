module Spotify where

import Control.Lens ((^?))
import Data.Aeson.Lens
import Imports
import Misc
import qualified Data.Text as  T

spotifyTogglePlay :: (MonadIO m, MonadReader Env m) => m ()
spotifyTogglePlay = do
  mtoken <- view envSpotifyToken
  case mtoken of
    Nothing -> spotifyDbus "PlayPause"
    Just token -> forkXio $ do
      responseJson <- spotifyWebGet token "player" []
      case responseJson ^? key "is_playing" . _Bool of
        Nothing -> error $ "Unexpected player response: " ++ responseJson
        Just True -> spotifyWeb token "PUT" "player/pause" []
        Just False -> spotifyWeb token "PUT" "player/play" []

spotifyNext :: (MonadIO m, MonadReader Env m) => m ()
spotifyNext = spotifyDbusOrWeb "Next" "POST" "player/next" []

spotifyPrevious :: (MonadIO m, MonadReader Env m) => m ()
spotifyPrevious = spotifyDbusOrWeb "Previous" "POST" "player/previous" []

spotifyPlay :: (MonadIO m, MonadReader Env m) => m ()
spotifyPlay = spotifyDbusOrWeb "Play" "PUT" "player/play" []

spotifyStop :: (MonadIO m, MonadReader Env m) => m ()
spotifyStop = spotifyDbusOrWeb "Stop" "PUT" "player/pause" []

spotifySetVolume :: (MonadIO m, MonadReader Env m) => Int -> m ()
spotifySetVolume vol =
  spotifyWebOnly "PUT" "player/volume" ["volume_percent==" ++ show vol]

spotifyDbusOrWeb
  :: (MonadIO m, MonadReader Env m)
  => String -> String -> String -> [String] -> m ()
spotifyDbusOrWeb dbusCmd method urlSuffix args = do
  noDbus <- view envSpotifyNoDbus
  mtoken <- view envSpotifyToken
  case (noDbus, mtoken) of
    -- TODO: consider falling back on web if dbus fails?  (to remote
    -- control phone without desktop client running)
    (False, _) ->
      spotifyDbus dbusCmd
    (True, Just token) ->
      spotifyWeb token method urlSuffix args
    (True, Nothing) ->
      forkXio $ notify $ concat
        [ "Error: SPOTIFY_NO_DBUS=true but no token in"
        , " ~/env/untracked/spotify.token"
        , " (if it exists, restarting XMonad is required)."
        ]

spotifyWebOnly
  :: (MonadIO m, MonadReader Env m)
  => String -> String -> [String] -> m ()
spotifyWebOnly method urlSuffix args = do
  mtoken <- view envSpotifyToken
  case mtoken of
    Nothing -> forkXio $
      notify "Operation requires spotify token in env/untracked/spotify.token"
    Just token ->
      spotifyWeb token method urlSuffix args

spotifyDbus :: (MonadIO m, MonadReader Env m) => String -> m ()
spotifyDbus cmd =
  spawn
    "dbus-send"
    [ "--print-reply"
    , "--dest=org.mpris.MediaPlayer2.spotify"
    , "/org/mpris/MediaPlayer2"
    , "org.mpris.MediaPlayer2.Player." ++ cmd
    ]

spotifyWeb
  :: (MonadIO m, MonadReader Env m)
  => SpotifyToken -> String -> String -> [String] -> m ()
spotifyWeb (SpotifyToken token) method urlSuffix args =
  spawn "http" $
    [ method
    , "https://api.spotify.com/v1/me/" ++ urlSuffix
    , "Authorization:Bearer " ++ T.unpack token
    ] ++ args

spotifyWebGet :: SpotifyToken -> String -> [String] -> Xio String
spotifyWebGet (SpotifyToken token) urlSuffix args =
  syncSpawnAndReadInheritStdin "http" $
    [ "GET"
    , "https://api.spotify.com/v1/me/" ++ urlSuffix
    , "Authorization:Bearer " ++ T.unpack token
    ] ++ args
