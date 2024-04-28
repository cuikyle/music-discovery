{-# LANGUAGE TypeOperators #-}

module Server where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Data.ByteString.Char8 (unpack)
import RIO
import Servant
import Types
import Prelude
import qualified Redis
import qualified Spotify


{-
    This module defines the webserver + handlers to respond to requests when the server is running.
    When a request is received, values are captured from the URL and processed accordingly.
    i.e.  GET /music/artists/Coldplay will capture Coldplay as a String and process + return a response.

    Redis is checked to see if a previous request has been made and cached.
    If no entry exists / entry has expired, then a new request to Spotify is made and the results are cached. 
-}

-- Handles a request to get Artists with a similar name
getArtistsHandler :: String -> Servant.Handler Artists
getArtistsHandler name = do
    maybeArtists <- liftIO $ Redis.fetch name  -- check if entry exists
    case maybeArtists of
        Just artists -> return (read . unpack $ artists)    -- convert the ByteString from Redis into an Artists and return
        Nothing -> Spotify.getArtists name  -- call getArtists to make a request to Spotify + cache

-- Handles a request to get an artist's ArtistInfo
getArtistInfoHandler :: String -> Servant.Handler ArtistInfo
getArtistInfoHandler artistId = do
    maybeArtistInfo <- liftIO $ Redis.fetch artistId
    case maybeArtistInfo of
        Just artistInfo -> return (read . unpack $ artistInfo)
        Nothing -> Spotify.getArtistInfo (ArtistId artistId)

-- Handles a request to get an artist's RelatedArtists
getRelatedArtistsHandler :: String -> Servant.Handler RelatedArtists
getRelatedArtistsHandler artistId = do
    let redisKey = artistId <> "/related-artists"
    maybeRelatedArtists <- liftIO $ Redis.fetch redisKey
    case maybeRelatedArtists of
        Just relatedArtists -> return (read . unpack $ relatedArtists)
        Nothing -> Spotify.getRelatedArtists (ArtistId artistId) redisKey

-- Handles a request to get an artist's TopTracks
getArtistTopTracksHandler :: String -> Servant.Handler TopTracks
getArtistTopTracksHandler artistId = do
    let redisKey = artistId <> "/top-tracks"
    maybeTopTracks <- liftIO $ Redis.fetch redisKey
    case maybeTopTracks of
        Just topTracks -> return (read . unpack $ topTracks)
        Nothing -> Spotify.getArtistTopTracks (ArtistId artistId) redisKey


-- MusicDiscoveryAPI defines the routes
type MusicDiscoveryAPI =
             "music" :> "artists" :> Capture "name" String :> Get '[JSON] Artists
        :<|> "music" :> "artists" :> "related" :> Capture "artistId" String :> Get '[JSON] RelatedArtists
        :<|> "music" :> "artists" :> "info" :> Capture "artistId" String :> Get '[JSON] ArtistInfo
        :<|> "music" :> "artists" :> "toptracks" :> Capture "artistId" String :> Get '[JSON] TopTracks

musicDiscoveryServer :: Server MusicDiscoveryAPI
musicDiscoveryServer = getArtistsHandler :<|> getRelatedArtistsHandler :<|> getArtistInfoHandler :<|> getArtistTopTracksHandler

musicDiscoveryProxy :: Servant.Proxy MusicDiscoveryAPI
musicDiscoveryProxy = Servant.Proxy

musicDiscoveryApplication :: Application
musicDiscoveryApplication = simpleCors $ serve musicDiscoveryProxy musicDiscoveryServer

runMain :: IO ()
runMain = do
    print ("Running on port 8000!" :: String)
    run 8000 musicDiscoveryApplication
