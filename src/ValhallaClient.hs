{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ValhallaClient
    ( fetchAndCalculateAverage
    ) where

import           Control.Exception     (SomeException, try)
import           Control.Lens          ((&), (.~), (^.))
import           Data.Aeson            (FromJSON (..), Object, ToJSON (..),
                                        Value (..), defaultOptions,
                                        genericParseJSON, genericToJSON, (.:))
import           Data.ByteString.Char8 as BS (pack)
import           Data.Maybe            (mapMaybe)
import           GHC.Generics          (Generic)
import           Network.Wreq          as Wreq (Response, asJSON, defaults,
                                                header, postWith, responseBody)
import           Types


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


