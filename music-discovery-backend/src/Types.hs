module Types where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.Char
import RIO

{-
  Custom Aeson JSON fieldLabelModifier used for serializing/deserializing objects into/from JSON.
  The main purpose here to bypass namespace pollution (i.e. several datatypes may have the fields "name" or "id")
  i.e. for JSON -> Image, it gets serialized into ImageHeight, etc. but when converting back to JSON, it will be Height
-}
jsonOptions :: String -> JSON.Options
jsonOptions prefix =
    let prefixLength = length prefix
        lowercaseFirstCharacter (c : rest) = toLower c : rest
        lowercaseFirstCharacter [] = []
     in JSON.defaultOptions{JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter}

newtype ArtistId = ArtistId
    { artistIdId :: String
    }
    deriving (Eq, Generic, Read)

instance ToJSON ArtistId where
    toJSON = JSON.genericToJSON $ jsonOptions "ArtistId"

instance FromJSON ArtistId where
    parseJSON = JSON.genericParseJSON $ jsonOptions "ArtistId"

instance Show ArtistId where
    show :: ArtistId -> String
    show = artistIdId

newtype Artists = Artists
    { artistsArtists :: Items
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON Artists where
    toJSON = JSON.genericToJSON $ jsonOptions "Artists"

instance FromJSON Artists where
    parseJSON = JSON.genericParseJSON $ jsonOptions "Artists"

newtype Items = Items
    { itemsItems :: [Artist]
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON Items where
    toJSON = JSON.genericToJSON $ jsonOptions "Items"

instance FromJSON Items where
    parseJSON = JSON.genericParseJSON $ jsonOptions "Items"

data Artist = Artist
    { artistId :: Text
    , artistImages :: [Image]
    , artistName :: Text
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON Artist where
    toJSON = JSON.genericToJSON $ jsonOptions "Artist"

instance FromJSON Artist where
    parseJSON = JSON.genericParseJSON $ jsonOptions "Artist"

data Image = Image
    { imageHeight :: Int
    , imageWidth :: Int
    , imageUrl :: Text
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON Image where
    toJSON = JSON.genericToJSON $ jsonOptions "Image"

instance FromJSON Image where
    parseJSON = JSON.genericParseJSON $ jsonOptions "Image"

data ArtistInfo = ArtistInfo
    { artistInfoGenres :: [String]
    , artistInfoPopularity :: Int
    , artistInfoFollowers :: Followers
    , artistInfoImages :: [Image]
    , artistInfoName :: String
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON ArtistInfo where
    toJSON = JSON.genericToJSON $ jsonOptions "ArtistInfo"

instance FromJSON ArtistInfo where
    parseJSON = JSON.genericParseJSON $ jsonOptions "ArtistInfo"

newtype Followers = Followers
    { followersTotal :: Int
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON Followers where
    toJSON = JSON.genericToJSON $ jsonOptions "Followers"

instance FromJSON Followers where
    parseJSON = JSON.genericParseJSON $ jsonOptions "Followers"

newtype RelatedArtists = RelatedArtists
    { relatedArtistsArtists :: [Artist]
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON RelatedArtists where
    toJSON = JSON.genericToJSON $ jsonOptions "RelatedArtists"

instance FromJSON RelatedArtists where
    parseJSON = JSON.genericParseJSON $ jsonOptions "RelatedArtists"

data Album = Album
    { albumImages :: [Image]
    , albumName :: String
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON Album where
    toJSON = JSON.genericToJSON $ jsonOptions "Album"

instance FromJSON Album where
    parseJSON = JSON.genericParseJSON $ jsonOptions "Album"

data Track = Track
    { trackHref :: String
    , trackName :: String
    , trackAlbum :: Album
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON Track where
    toJSON = JSON.genericToJSON $ jsonOptions "Track"

instance FromJSON Track where
    parseJSON = JSON.genericParseJSON $ jsonOptions "Track"

newtype TopTracks = TopTracks
    { topTracksTracks :: [Track]
    }
    deriving (Eq, Show, Generic, Read)

instance ToJSON TopTracks where
    toJSON = JSON.genericToJSON $ jsonOptions "TopTracks"

instance FromJSON TopTracks where
    parseJSON = JSON.genericParseJSON $ jsonOptions "TopTracks"

oneHour :: Integer
oneHour = 60 * 60

oneDay :: Integer
oneDay = oneHour * 24

twoWeeks :: Integer
twoWeeks = oneDay * 7 * 2
