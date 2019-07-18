module Spotify where

import Control.Lens ((^?), (&))
import Data.Aeson (Value)
import Data.Aeson.Lens
import Imports
import Misc
import Network.HTTP.Simple
import qualified Data.Text as  T
import qualified Data.Text.Encoding as  T
import qualified Data.ByteString.Char8 as BS8

spotifyTogglePlay :: (MonadThrow m, MonadIO m, MonadReader Env m) => m ()
spotifyTogglePlay = do
  mtoken <- view envSpotifyToken
  case mtoken of
    Nothing -> spotifyDbus "PlayPause"
    Just token -> forkXio $ do
      responseJson <- spotifyWebGet token "player" id
      case responseJson ^? key "is_playing" . _Bool of
        Nothing -> error $ "Unexpected player response: " ++ show responseJson
        Just True -> spotifyWeb token "PUT" "player/pause" id
        Just False -> spotifyWeb token "PUT" "player/play" id

spotifyNext :: (MonadThrow m, MonadIO m, MonadReader Env m) => m ()
spotifyNext = spotifyDbusOrWeb "Next" "POST" "player/next" id

spotifyPrevious :: (MonadThrow m, MonadIO m, MonadReader Env m) => m ()
spotifyPrevious = spotifyDbusOrWeb "Previous" "POST" "player/previous" id

spotifyPlay :: (MonadThrow m, MonadIO m, MonadReader Env m) => m ()
spotifyPlay = spotifyDbusOrWeb "Play" "PUT" "player/play" id

spotifyStop :: (MonadThrow m, MonadIO m, MonadReader Env m) => m ()
spotifyStop = spotifyDbusOrWeb "Stop" "PUT" "player/pause" id

spotifySetVolume :: (MonadThrow m, MonadIO m, MonadReader Env m) => Int -> m ()
spotifySetVolume vol =
  spotifyWebOnly "PUT" "player/volume" $
    setRequestQueryString [("volume_percent", Just (BS8.pack (show vol)))]

spotifyDbusOrWeb
  :: (MonadThrow m, MonadIO m, MonadReader Env m)
  => String -> ByteString -> String -> (Request -> Request) -> m ()
spotifyDbusOrWeb dbusCmd method urlSuffix mod = do
  noDbus <- return True -- view envSpotifyNoDbus
  mtoken <- view envSpotifyToken
  case (noDbus, mtoken) of
    -- TODO: consider falling back on web if dbus fails?  (to remote
    -- control phone without desktop client running)
    (False, _) ->
      spotifyDbus dbusCmd
    (True, Just token) ->
      spotifyWeb token method urlSuffix mod
    (True, Nothing) ->
      forkXio $ notify $ concat
        [ "Error: SPOTIFY_NO_DBUS=true but no token in"
        , " ~/env/untracked/spotify.token"
        , " (if it exists, restarting XMonad is required)."
        ]

spotifyWebOnly
  :: (MonadThrow m, MonadIO m, MonadReader Env m)
  => ByteString -> String -> (Request -> Request) -> m ()
spotifyWebOnly method urlSuffix mod =
  withSpotifyToken $ \token ->
    spotifyWeb token method urlSuffix mod

withSpotifyToken
  :: (MonadIO m, MonadReader Env m)
  => (SpotifyToken -> m ()) -> m ()
withSpotifyToken f = do
  mtoken <- view envSpotifyToken
  case mtoken of
    Nothing -> forkXio $
      notify "Operation requires spotify token in env/untracked/spotify.token"
    Just token ->
      f token

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
  :: (MonadThrow m, MonadIO m, MonadReader Env m)
  => SpotifyToken -> ByteString -> String -> (Request -> Request) -> m ()
spotifyWeb (SpotifyToken token) method urlSuffix mod = do
  req0 <- parseRequestThrow $ "https://api.spotify.com/v1/me/" ++ urlSuffix
  void $ httpNoBody $ mod $ req0
    & setRequestMethod method
    & addRequestHeader "Authorization" ("Bearer " <> T.encodeUtf8 token)

spotifyWebGet :: SpotifyToken -> String -> (Request -> Request) -> Xio Value
spotifyWebGet (SpotifyToken token) urlSuffix mod = do
  req0 <- parseRequestThrow $ "https://api.spotify.com/v1/me/" ++ urlSuffix
  let req = mod $ req0
        & setRequestMethod "GET"
        & addRequestHeader "Authorization" ("Bearer " <> T.encodeUtf8 token)
  logInfo $ "Sending " <> fromString (show req)
  fmap getResponseBody $ httpJSON req
