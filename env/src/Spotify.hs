module Spotify where

import Control.Lens ((^?), (&))
import Control.Monad.Fail
import Data.Aeson (ToJSON, Value)
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens
import Data.Time
import Imports
import Misc
import Network.HTTP.Simple
import Spotify.Types
import qualified Data.Text as  T
import qualified Data.Text.Encoding as  T
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64

spotifyTogglePlay :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m) => m ()
spotifyTogglePlay = do
  mspotify <- view envSpotify
  case mspotify of
    Nothing -> spotifyDbus "PlayPause"
    Just spotify -> forkXio $ do
      isPlaying <- spotifyGetPlayerInfo spotify (^? (key "is_playing" . _Bool))
      if isPlaying then spotifyStop else spotifyPlay

spotifyNext :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m) => m ()
spotifyNext = spotifyDbusOrWeb "Next" "POST" "player/next" id

spotifyPrevious :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m) => m ()
spotifyPrevious = spotifyDbusOrWeb "Previous" "POST" "player/previous" id

spotifyPlay :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m) => m ()
spotifyPlay = spotifyDbusOrWeb "Play" "PUT" "player/play" id

spotifyStop :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m) => m ()
spotifyStop = spotifyDbusOrWeb "Stop" "PUT" "player/pause" id

spotifySetVolume
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => Int -> m ()
spotifySetVolume vol =
  spotifyWebOnly "PUT" "player/volume" $
    setRequestQueryString [("volume_percent", Just (BS8.pack (show vol)))]

spotifyAddToVolume
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => Int -> m ()
spotifyAddToVolume amount = withSpotify $ \spotify -> forkXio $ do
  vol <- spotifyGetPlayerInfo spotify
    (^? (key "device" . key "volume_percent" . _Integral))
  let vol' = vol + amount
  spotifySetVolume vol'
  let msg = "Spotify volume set to " ++ show vol'
  syncSpawn "notify-send" ["-t", "1000", "spotify-control", msg]

spotifyDebugPlayerInfo
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => m ()
spotifyDebugPlayerInfo = withSpotify $ \spotify -> forkXio $ do
  info <- spotifyGetPlayerInfo spotify Just
  logDebug $ "Player info is\n" <> displayJson info

spotifyGetPlayerInfo :: Spotify -> (Value -> Maybe a) -> Xio a
spotifyGetPlayerInfo spotify checker = do
  playerInfo <- spotifyWebGet spotify "player" id
  case checker playerInfo of
    Just x -> return x
    Nothing -> unexpectedResponse playerInfo

spotifyDbusOrWeb
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => String -> ByteString -> String -> (Request -> Request) -> m ()
spotifyDbusOrWeb dbusCmd method urlSuffix f = do
  noDbus <- view envSpotifyNoDbus
  mspotify <- view envSpotify
  case (noDbus, mspotify) of
    -- TODO: consider falling back on web if dbus fails?  (to remote
    -- control phone without desktop client running)
    (False, _) ->
      spotifyDbus dbusCmd
    (True, Just spotify) ->
      spotifyWeb spotify method urlSuffix f
    (True, Nothing) ->
      forkXio $ notify $ concat
        [ "Error: SPOTIFY_NO_DBUS=true but no token or client info in"
        , " ~/env/untracked/ (if it exists, restarting XMonad is required)."
        ]

spotifyWebOnly
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => ByteString -> String -> (Request -> Request) -> m ()
spotifyWebOnly method urlSuffix f =
  withSpotify $ \spotify ->
    spotifyWeb spotify method urlSuffix f

withSpotify
  :: (MonadIO m, MonadReader Env m)
  => (Spotify -> m ()) -> m ()
withSpotify f = do
  mspotify <- view envSpotify
  case mspotify of
    Nothing -> forkXio $
      notify "Operation requires spotify token and client info in env/untracked/"
    Just spotify ->
      f spotify

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
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => Spotify -> ByteString -> String -> (Request -> Request) -> m ()
spotifyWeb spotify method urlSuffix f = do
  SpotifyAccessToken accessToken <- spotifyAccessToken spotify
  req0 <- parseRequestThrow $ "https://api.spotify.com/v1/me/" ++ urlSuffix
  let req = f $ req0
        & setRequestMethod method
        & addRequestHeader "Authorization" ("Bearer " <> T.encodeUtf8 accessToken)
  logInfo $ "Spotify request " <> fromString urlSuffix
  void $ httpNoBody req

spotifyWebGet
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => Spotify -> String -> (Request -> Request) -> m Value
spotifyWebGet spotify urlSuffix f = do
  SpotifyAccessToken accessToken <- spotifyAccessToken spotify
  req0 <- parseRequestThrow $ "https://api.spotify.com/v1/me/" ++ urlSuffix
  let req = f $ req0
        & setRequestMethod "GET"
        & addRequestHeader "Authorization" ("Bearer " <> T.encodeUtf8 accessToken)
  logInfo $ "Spotify request " <> fromString urlSuffix
  fmap getResponseBody $ httpJSON req

spotifyAccessToken
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => Spotify -> m SpotifyAccessToken
spotifyAccessToken spotify = do
  mexistingToken <- existingAccessToken spotify
  case mexistingToken of
    Just token -> do
      logInfo "Using cached spotify access token."
      return token
    Nothing -> getNewAccessToken spotify

existingAccessToken :: MonadIO m => Spotify -> m (Maybe SpotifyAccessToken)
existingAccessToken spotify = do
  mstored <- readIORef (spotify ^. spotifyAccessTokenRef)
  case mstored of
    Nothing -> return Nothing
    Just (expiration, token) -> do
      now <- liftIO getCurrentTime
      if now > expiration
        then return Nothing
        else return (Just token)

getNewAccessToken
  :: (MonadThrow m, MonadFail m, MonadIO m, MonadReader Env m)
  => Spotify -> m SpotifyAccessToken
getNewAccessToken spotify = do
  let SpotifyClientId clientId = spotify ^. spotifyClientId
      SpotifyClientSecret clientSecret = spotify ^. spotifyClientSecret
      SpotifyRefreshToken refreshToken = spotify ^. spotifyRefreshToken
  req0 <- parseRequestThrow "https://accounts.spotify.com/api/token"
  let req = req0
        & setRequestMethod "POST"
        & addRequestHeader "Authorization" ("Basic " <> authorization)
        & setRequestBodyURLEncoded
          [ ("grant_type", "refresh_token")
          , ("refresh_token", encodeUtf8 (T.strip refreshToken))
          ]
      authorization = B64.encode $ mconcat
        [ encodeUtf8 clientId
        , ":"
        , encodeUtf8 clientSecret
        ]
  logInfo "Refreshing spotify access token."
  response <- fmap getResponseBody $ httpJSON req
  case response ^? key "access_token" . _String of
    Nothing -> unexpectedResponse response
    Just accessToken ->
      case response ^? key "expires_in" . _Integral of
        Nothing -> unexpectedResponse response
        Just expirationSeconds -> do
          now <- liftIO getCurrentTime
          let expiration =
                addUTCTime (fromIntegral (expirationSeconds - 5 :: Int)) now
              token = SpotifyAccessToken accessToken
          writeIORef (spotify ^. spotifyAccessTokenRef)
                     (Just (expiration, token))
          return token

unexpectedResponse :: (MonadIO m, MonadReader Env m) => Value -> m a
unexpectedResponse value = do
  logError $ "Unexpected spotify response:\n" <> displayJson value
  error "Unexpected spotify response."

displayJson :: ToJSON a => a -> Utf8Builder
displayJson = displayBytesUtf8 . LBS.toStrict . encodePretty
