module ValhallaClient (
    fetchAndCalculateAverage,
) where

import Control.Exception (try)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Network.HTTP.Simple (
    JSONException,
    Response,
    getResponseBody,
    httpJSON,
    parseRequest_,
    setRequestBodyJSON,
 )
import Types

fetchAndCalculateAverage :: IO (Either String CalcResult)
fetchAndCalculateAverage = do
    let url = "POST http://localhost:8002/matrix"

    let originPoint = [Location{lon = -122.294, lat = 37.884}]
    let destinationPoints =
            [ Location
                { lon = -122.294 + (fromIntegral i * (-0.00001))
                , lat = fromIntegral i * 0.0001
                }
            | i <- [1 :: Int .. 5000]
            ]

    let payload =
            MatrixRequest
                { sources = originPoint
                , targets = destinationPoints
                , costing = "auto"
                , _id = "bulk-query-5000"
                }

    let request =
            parseRequest_ url
                & setRequestBodyJSON payload

    eResp <- try (httpJSON request) :: IO (Either JSONException (Response MatrixResponse))

    case eResp of
        Left ex -> return $ Left (show ex)
        Right r -> do
            let respBody = getResponseBody r

            let allResults = concat (sources_to_targets respBody)
            let validTargets = catMaybes allResults
            let validDurations = map time validTargets

            if null validDurations
                then return $ Left "No valid routes found."
                else do
                    let totalDuration = sum validDurations
                        avgSeconds = totalDuration / fromIntegral (length validDurations)

                        finalAvgMinutes = avgSeconds / 60
                        routesOK = length validDurations
                        routesFail = length destinationPoints - routesOK

                    return $
                        Right $
                            CalcResult
                                { routesFound = routesOK
                                , routesNotFound = routesFail
                                , averageMinutes = finalAvgMinutes
                                }
