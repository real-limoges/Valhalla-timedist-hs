module API.Api (
    ValhallaAPI,
    apiProxy,
    app,
    appWithConfig,
    serverWithConfig,
    defaultValhallaUrl,
) where

import API.Handlers (healthHandler, timeDistanceHandler)
import Servant
import Types (CostingResult, TimeDistanceRequest)

type ValhallaAPI =
    HealthEndpoint
        :<|> TimeDistanceEndpoint

type HealthEndpoint =
    "health"
        :> Get '[JSON] String

type TimeDistanceEndpoint =
    "time_distance"
        :> ReqBody '[JSON] TimeDistanceRequest
        :> Post '[JSON] [CostingResult]

apiProxy :: Proxy ValhallaAPI
apiProxy = Proxy

defaultValhallaUrl :: String
defaultValhallaUrl = "http://localhost:8002"

app :: Application
app = appWithConfig defaultValhallaUrl

appWithConfig :: String -> Application
appWithConfig valhallaUrl = serve apiProxy (serverWithConfig valhallaUrl)

serverWithConfig :: String -> Server ValhallaAPI
serverWithConfig valhallaUrl =
    healthHandler
        :<|> timeDistanceHandler valhallaUrl
