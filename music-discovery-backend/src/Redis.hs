{-# LANGUAGE TypeOperators #-}

module Redis where

import Data.ByteString.Char8 (pack)
import Data.Time.Clock
import Database.Redis
import RIO
import System.ReadEnvVar

-- reads Redis host from REDIS_HOSTNAME env var
redisHost :: IO String
redisHost = readEnvDef "REDIS_HOSTNAME" "localhost"

redisConnectInfo :: IO ConnectInfo
redisConnectInfo = do
    host <- redisHost
    return
        ConnInfo
            { connectHost = host
            , connectPort = PortNumber 6379
            , connectAuth = Nothing
            , connectDatabase = 0
            , connectMaxConnections = 50
            , connectMaxIdleTime = 60 :: NominalDiffTime
            , connectTimeout = Nothing
            , connectTLSParams = Nothing
            }

runCommand :: ConnectInfo -> Redis a -> IO a
runCommand redisInfo action = do
    connection <- connect redisInfo
    runRedis connection action

-- inserts key,val into Redis with a ttl
insert :: String -> ByteString -> Integer -> IO ()
insert keyR valR ttlR = do
    redisConInfo <- redisConnectInfo
    runCommand redisConInfo $ void $ setex (pack . show $ keyR) ttlR valR

-- fetches KVP from Redis if it exists
fetch :: String -> IO (Maybe ByteString)
fetch keyR = do
    redisConInfo <- redisConnectInfo
    runCommand redisConInfo $ do
        redisResult <- Database.Redis.get (pack . show $ keyR)
        case redisResult of
            Right (Just result) -> return $ Just result
            _ -> return Nothing
