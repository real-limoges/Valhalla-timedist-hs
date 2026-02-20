module API.Api (
    ValhallaAPI,
    apiProxy,
    appWithConfig,
    serverWithConfig,
) where

import API.Handlers (healthHandler, timeDistanceHandler)
import Data.Text (Text)
import Servant
import Types (AppConfig, CostingResult, TimeDistanceRequest)

type ValhallaAPI = "v1" :> V1API

type V1API =
    HealthEndpoint
        :<|> TimeDistanceEndpoint

type HealthEndpoint =
    "health"
        :> Get '[JSON] Text

type TimeDistanceEndpoint =
    "time_distance"
        :> ReqBody '[JSON] TimeDistanceRequest
        :> Post '[JSON] [CostingResult]

apiProxy :: Proxy ValhallaAPI
apiProxy = Proxy

appWithConfig :: AppConfig -> Application
appWithConfig config = serve apiProxy (serverWithConfig config)

serverWithConfig :: AppConfig -> Server ValhallaAPI
serverWithConfig config =
    healthHandler config
        :<|> timeDistanceHandler config
