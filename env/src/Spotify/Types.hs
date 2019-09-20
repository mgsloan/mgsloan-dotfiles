module Spotify.Types where

import Control.Lens (makeLenses)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Time
import Prelude

data Spotify = Spotify
  { _spotifyClientId :: !SpotifyClientId
  , _spotifyClientSecret :: !SpotifyClientSecret
  , _spotifyRefreshToken :: !SpotifyRefreshToken
  , _spotifyAccessTokenRef :: !(IORef (Maybe (UTCTime, SpotifyAccessToken)))
  }

newtype SpotifyClientId = SpotifyClientId Text
newtype SpotifyClientSecret = SpotifyClientSecret Text
newtype SpotifyRefreshToken = SpotifyRefreshToken Text
newtype SpotifyAccessToken = SpotifyAccessToken Text

data SpotifyTrackInfo = SpotifyTrackInfo
  { _spotifyTrackId :: !Text
  , _spotifyTrackName :: !Text
  , _spotifyTrackArtists :: ![Text]
  }

makeLenses 'Spotify
makeLenses 'SpotifyTrackInfo
