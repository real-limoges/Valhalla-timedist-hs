module API.Handlers (
    healthHandler,
    timeDistanceHandler,
    maxPoints,
) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 qualified as BL
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Simple (
    Response,
    getResponseBody,
    httpJSON,
    parseRequest_,
    setRequestBodyJSON,
    setRequestResponseTimeout,
 )
import Servant (Handler, ServerError (..), err400, err500, throwError)
import Types

maxPoints :: Int
maxPoints = 10000

-- 30 second timeout for Valhalla requests
requestTimeoutSeconds :: Int
requestTimeoutSeconds = 30

healthHandler :: Handler String
healthHandler = pure "OK"

timeDistanceHandler :: String -> TimeDistanceRequest -> Handler [CostingResult]
timeDistanceHandler valhallaBaseUrl req = do
    let numPoints = length (points req)
        costingModels = costing req

    when (null costingModels) $
        throwError err400{errBody = "At least one costing model is required"}

    when (numPoints > maxPoints) $
        throwError err400{errBody = BL.pack $ "Too many points: " ++ show numPoints ++ " exceeds limit of " ++ show maxPoints}

    forM costingModels $ \costingModel -> do
        let valhallaURL = "POST " ++ valhallaBaseUrl ++ "/matrix"
            payload = MatrixRequest
                { sources = [subject req]
                , targets = points req
                , matrixCosting = costingToString costingModel
                , _id = "time-distance-query"
                }
            timeout = responseTimeoutMicro (requestTimeoutSeconds * 1000000)
            request = setRequestResponseTimeout timeout
                    $ setRequestBodyJSON payload
                    $ parseRequest_ valhallaURL

        eResp <- liftIO (try (httpJSON request) :: IO (Either SomeException (Response MatrixResponse)))

        case eResp of
            Left ex ->
                throwError err500{errBody = BL.pack (show ex)}
            Right r ->
                pure CostingResult
                    { costingModel = costingModel
                    , results = concat $ sources_to_targets $ getResponseBody r
                    }
