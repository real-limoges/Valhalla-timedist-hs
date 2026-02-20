module Valhalla.Client (
    checkStatus,
    queryMatrix,
) where

import Control.Exception (SomeException, try)
import Network.HTTP.Client (Request)
import Network.HTTP.Simple (
    Response,
    getResponseBody,
    httpJSON,
    parseRequest,
    setRequestBodyJSON,
    setRequestManager,
 )
import Data.Text qualified as T
import Types.Config (AppConfig (..))
import Types.Domain (CostingModel, Location, ValhallaUrl (..), costingToText)
import Types.Valhalla (MatrixRequest (..), MatrixResponse (..), TargetResult)

-- | Ping the Valhalla /status endpoint. Returns Left on any error.
checkStatus :: AppConfig -> IO (Either SomeException ())
checkStatus config = do
    let url = T.unpack $ unValhallaUrl (valhallaUrl config) <> "/status"
    result <- try (parseRequest url >>= httpJSON) :: IO (Either SomeException (Response ()))
    pure (fmap (const ()) result)

-- | Query the Valhalla matrix API for times and distances from one source to
-- many targets using a single costing model.
queryMatrix
    :: AppConfig
    -> Location
    -- ^ Source location
    -> [Location]
    -- ^ Target locations
    -> CostingModel
    -> IO (Either SomeException [Maybe TargetResult])
queryMatrix config src tgts cm = do
    let url = T.unpack $ "POST " <> unValhallaUrl (valhallaUrl config) <> "/sources_to_targets"
    eBaseRequest <- try (parseRequest url) :: IO (Either SomeException Request)
    case eBaseRequest of
        Left ex -> pure (Left ex)
        Right baseRequest -> do
            let payload =
                    MatrixRequest
                        { sources = [src]
                        , targets = tgts
                        , matrixCosting = costingToText cm
                        , requestId = "time-distance-query"
                        }
                request =
                    setRequestManager (httpManager config)
                        $ setRequestBodyJSON payload baseRequest
            eResp <- try (httpJSON request) :: IO (Either SomeException (Response MatrixResponse))
            pure $ case eResp of
                Left ex -> Left ex
                Right r -> Right (concat $ sources_to_targets $ getResponseBody r)
