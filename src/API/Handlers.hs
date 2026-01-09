module API.Handlers (
    AppM (..),
    healthHandler,
    fetchAndCalculateAverageHandler,
) where

import Control.Exception (try)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Network.HTTP.Simple (
    JSONException,
    Response,
    getResponseBody,
    httpJSON,
    parseRequest_,
    setRequestBodyJSON,
 )
import Servant
import Types

newtype AppM a = AppM {unAppM :: Handler a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadError ServerError
        )

healthHandler :: AppM String
healthHandler = return "Hello from a stateless AppM!"

-- Change signature: Return CalcResult directly, not Either
fetchAndCalculateAverageHandler :: Double -> Double -> Int -> AppM CalcResult
fetchAndCalculateAverageHandler sub_lon sub_lat count = do
    let valhallaURL = "POST http://localhost:8002/matrix"
    let originLoc = Location{lon = sub_lon, lat = sub_lat}

    let destinationPoints =
            [ Location
                { lon = -122.294 + (fromIntegral i * (-0.00001))
                , lat = fromIntegral i * 0.0001
                }
            | i <- [1 :: Int .. count]
            ]

    let payload =
            MatrixRequest
                { sources = [originLoc]
                , targets = destinationPoints
                , costing = "auto"
                , _id = "bulk-query"
                }

    let request =
            parseRequest_ valhallaURL
                & setRequestBodyJSON payload

    eResp <- liftIO (try (httpJSON request) :: IO (Either JSONException (Response MatrixResponse)))

    case eResp of
        -- Failure 1: HTTP/JSON connection error
        Left ex ->
            throwError $ err500{errBody = TL.encodeUtf8 (TL.pack $ show ex)}
        Right r -> do
            let respBody = getResponseBody r
            let allResults = concat (sources_to_targets respBody)
            let validTargets = catMaybes allResults
            let validDurations = map time validTargets

            if null validDurations
                then
                    -- Failure 2: Valid HTTP, but no routes found
                    throwError $ err404{errBody = "No valid routes found."}
                else do
                    let totalDuration = sum validDurations
                        avgSeconds = totalDuration / fromIntegral (length validDurations)
                        finalAvgMinutes = avgSeconds / 60

                        routesOK = length validDurations
                        routesFail = length destinationPoints - routesOK

                    -- Success: Return the value directly
                    return $
                        CalcResult
                            { routesFound = routesOK
                            , routesNotFound = routesFail
                            , averageMinutes = finalAvgMinutes
                            }
