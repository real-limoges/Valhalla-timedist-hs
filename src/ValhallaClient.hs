{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ValhallaClient 
    ( fetchAndCalculateAverage
    , CalcResult
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, defaultOptions, (.:), Value(..), Object)
import Network.Wreq as Wreq (postWith, asJSON, defaults, header, responseBody, Response)
import Control.Lens ((^.), (.~), (&))
import Data.ByteString.Char8 as BS (pack)
import Data.Maybe (mapMaybe)
import Control.Exception (try, SomeException)

-- This is a universal datatype
data Location = Location { lon :: Double, lat :: Double } deriving (Show, Generic, ToJSON)

-- These are for interfacinng with Valhalla
data MatrixRequest = MatrixRequest
    { sources :: [Location]
    , targets :: [Location]
    , costing :: String
    , _id     :: String 
    } deriving (Show, Generic, ToJSON)

data TargetResult = TargetResult { time :: Double, distance :: Double } deriving (Show, Generic)
instance FromJSON TargetResult where
    parseJSON (Object v) = TargetResult <$> v .: "time" <*> v .: "distance"
    parseJSON _ = fail "Expected an object for Target Result"

data MatrixResponse = MatrixResponse
    { sources_to_targets :: [[Maybe TargetResult]]
    } deriving (Show, Generic, FromJSON)


-- This is the public result
data CalcResult = CalcResult 
    { routesFound :: Int
    , routesNotFound :: Int
    , averageMinutes :: Double
    } deriving (Show, Generic)
instance ToJSON CalcResult where
    toJSON = genericToJSON defaultOptions


-- This is our function we're calling
-- The return is an Either - Left is the String error, right is the CalcResult
-- This will accept parameters in the future
fetchAndCalculateAverage :: IO (Either String CalcResult)
fetchAndCalculateAverage = do
    let url = "http://localhost:8002/matrix"
    let originPoint = [ Location { lon = -122.294, lat = 37.884 } ]
    let destinationPoints = [ Location { lon = -122.294 + (fromIntegral i * (-0.00001)), lat = (fromIntegral i * (0.0001)) } | i <- [1..5000] ]

    let payload = MatrixRequest { 
          sources = originPoint
        , targets = destinationPoints
        , costing = "auto"
        , _id = "bulk-query-5000"
        }
    
    let opts = defaults & Wreq.header "Content-Type" .~ [BS.pack "application/json"]

    eResp <- try (Wreq.asJSON =<< Wreq.postWith opts url (toJSON payload)) :: IO (Either SomeException (Wreq.Response MatrixResponse))

    case eResp of
        Left ex -> return $ Left (show ex)
        Right r -> do
            let respBody = r ^. Wreq.responseBody
                allResults = concat $ sources_to_targets respBody
                validDurations = mapMaybe (fmap time) allResults

            if null validDurations
                then return $ Left "No valid routes found."
                else do
                    let totalDuration = sum validDurations
                        avgSeconds = totalDuration / fromIntegral (length validDurations)
                        avgMinutes = avgSeconds / 60
                        routesOK = length validDurations
                        routesFail = length destinationPoints - routesOK

                    return $ Right $ CalcResult
                        { routesFound = routesOK
                        , routesNotFound = routesFail
                        , averageMinutes = avgMinutes
                        }


