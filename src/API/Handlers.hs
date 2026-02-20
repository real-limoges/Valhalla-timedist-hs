module API.Handlers (
    healthHandler,
    timeDistanceHandler,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, toJSON, (.=))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Logging (logError)
import Network.HTTP.Types (hContentType)
import Servant (Handler, ServerError (..), err400, err500, throwError)
import Types
import Valhalla.Client (checkStatus, queryMatrix)

-- | Build a ServerError with a JSON body {"error": <msg>} and correct Content-Type.
jsonError :: ServerError -> Text -> ServerError
jsonError base msg =
    base
        { errBody = encode (object ["error" .= msg])
        , errHeaders = [(hContentType, "application/json")]
        }

healthHandler :: AppConfig -> Handler Text
healthHandler config = do
    eResult <- liftIO (checkStatus config)
    case eResult of
        Right _ -> pure "OK"
        Left _ -> pure "OK (Valhalla unreachable)"

timeDistanceHandler :: AppConfig -> TimeDistanceRequest -> Handler [CostingResult]
timeDistanceHandler config req = do
    let numPoints = length (points req)
        costingModels = NE.toList (costing req)

    when (numPoints == 0) $
        throwError (jsonError err400 "At least one point is required")

    when (numPoints > maxPoints config) $
        throwError $
            jsonError err400 $
                "Too many points: "
                    <> T.pack (show numPoints)
                    <> " exceeds limit of "
                    <> T.pack (show (maxPoints config))

    eitherResults <- liftIO $ mapConcurrently (queryMatrix config (subject req) (points req)) costingModels

    sequence
        [ case (cm, er) of
            (_, Right targets) ->
                pure CostingResult{costingModel = cm, results = targets}
            (cm', Left ex) -> do
                liftIO $
                    logError
                        "Valhalla request failed"
                        [ ("costing", toJSON (T.pack (show cm')))
                        , ("error", toJSON (T.pack (show ex)))
                        ]
                throwError $
                    jsonError err500 $
                        "Routing engine error for costing model: " <> T.pack (show cm')
        | (cm, er) <- zip costingModels eitherResults
        ]
