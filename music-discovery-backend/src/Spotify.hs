module Spotify where

import Control.Monad.Trans.Except (throwE)
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Char8 (pack)
import Network.Wreq
import RIO
import qualified Redis
import Servant
import System.ReadEnvVar
import Types

-- Reads the SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET from env var
spotifyClientID :: IO ByteString
spotifyClientID = readEnvDef "SPOTIFY_CLIENT_ID" "" :: IO ByteString

spotifyClientSecret :: IO ByteString
spotifyClientSecret = readEnvDef "SPOTIFY_CLIENT_SECRET" "" :: IO ByteString

spotifyAuthTokenKey :: String
spotifyAuthTokenKey = "spotifyAuthToken"

spotifyBaseUrl :: String
spotifyBaseUrl = "https://api.spotify.com/v1/"

data SpotifyRoutes
    = AuthTokenR
    | SpotifyArtistsR String
    | SpotifyArtistInfoR ArtistId
    | SpotifyArtistRelatedR ArtistId
    | SpotifyArtistTopTracksR ArtistId
    deriving (Eq, Show, Generic)

getSpotifyRoute :: SpotifyRoutes -> String
getSpotifyRoute = \case
    AuthTokenR -> "https://accounts.spotify.com/api/token"
    SpotifyArtistsR artistName -> spotifyBaseUrl <> "search?q=artist:" <> artistName <> "&type=artist&market=US"
    SpotifyArtistInfoR artistId -> spotifyBaseUrl <> "artists/" <> show artistId
    SpotifyArtistRelatedR artistId -> spotifyBaseUrl <> "artists/" <> show artistId <> "/related-artists"
    SpotifyArtistTopTracksR artistId -> spotifyBaseUrl <> "artists/" <> show artistId <> "/top-tracks"

-- Requests a new OAuth access token from Spotify which is valid for 1 hour, where the request requires the clientId + secret to be encoded to base64
getNewSpotifyAuthToken :: IO Text
getNewSpotifyAuthToken = do
    spotifyCID <- spotifyClientID
    spotifyCS <- spotifyClientSecret
    let opts =
            defaults & header "Authorization" .~ ["Basic " <> B64.encode (spotifyCID <> ":" <> spotifyCS)]
                & header "Content-Type" .~ ["application/x-www-form-urlencoded"]
    let body = ["grant_type" := ("client_credentials" :: String)]
    resp <- postWith opts (getSpotifyRoute AuthTokenR) body
    pure $ resp ^. responseBody . key "access_token" . _String

-- Checks Redis to see if there is an access token present, returning it from cache if it is, or requests a new token and stores for 1 hour
getSpotifyAuthToken :: IO ByteString
getSpotifyAuthToken = do
    authToken <- Redis.fetch spotifyAuthTokenKey
    case authToken of
        Just token -> pure token
        Nothing -> do
            newToken <- getNewSpotifyAuthToken
            let newTokenBS = encodeUtf8 newToken
            Redis.insert spotifyAuthTokenKey newTokenBS oneHour
            pure newTokenBS

-- Gets an auth token and generates headers
generateOptions :: IO Options
generateOptions = do
    currentAuthToken <- getSpotifyAuthToken
    let opts = defaults & header "Authorization" .~ ["Bearer " <> currentAuthToken]
    pure opts

-- Makes a request to get detailed info on an artist with the id artistId and caches results into Redis
getArtistInfo :: ArtistId -> Servant.Handler ArtistInfo
getArtistInfo artistId = do
    opts <- liftIO generateOptions
    resp <- liftIO $ asJSON =<< getWith opts spotifyRoute
    let maybeArtistInfo = resp ^. responseBody -- use Lens to grab the ArtistInfo from the Response
    case maybeArtistInfo of
        Just artistInfo -> liftIO (Redis.insert (show artistId) (pack . show $ artistInfo) twoWeeks) >> pure artistInfo
        Nothing -> Servant.Handler $ throwE err401
    where
      spotifyRoute = getSpotifyRoute $ SpotifyArtistInfoR artistId

-- Makes a request to get related artists to the artist with id artistId and caches results into Redis
getRelatedArtists :: ArtistId -> String -> Servant.Handler RelatedArtists
getRelatedArtists artistId redisKey = do
    opts <- liftIO generateOptions
    resp <- liftIO $ asJSON =<< getWith opts spotifyRoute
    let maybeRelatedArtists = resp ^. responseBody
    case maybeRelatedArtists of
        Just relatedArtists -> liftIO (Redis.insert redisKey (pack . show $ relatedArtists) twoWeeks) >> pure relatedArtists
        Nothing -> Servant.Handler $ throwE err401
    where
      spotifyRoute = getSpotifyRoute $ SpotifyArtistRelatedR artistId

-- Makes a request to get top tracks of an artist with id artistId and caches results into Redis
getArtistTopTracks :: ArtistId -> String -> Servant.Handler TopTracks
getArtistTopTracks artistId redisKey = do
    opts <- liftIO generateOptions
    resp <- liftIO $ asJSON =<< getWith opts spotifyRoute
    let maybeTopTracks = resp ^. responseBody
    case maybeTopTracks of
        Just topTracks -> liftIO (Redis.insert redisKey (pack . show $ topTracks) twoWeeks) >> pure topTracks
        Nothing -> Servant.Handler $ throwE err401
    where
      spotifyRoute = getSpotifyRoute $ SpotifyArtistTopTracksR artistId

-- Makes a request to get artists with similar names to artistName and caches results into Redis
getArtists :: String -> Servant.Handler Artists
getArtists artistName = do
    opts <- liftIO generateOptions
    resp <- liftIO $ asJSON =<< getWith opts spotifyRoute
    let maybeArtists = resp ^. responseBody
    case maybeArtists of
        Just artists -> liftIO (Redis.insert artistName (pack . show $ artists) twoWeeks) >> pure artists
        Nothing -> Servant.Handler $ throwE err401
    where
      spotifyRoute = getSpotifyRoute $ SpotifyArtistsR artistName
