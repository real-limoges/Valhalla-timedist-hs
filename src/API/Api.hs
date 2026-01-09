module API.Api (
    ValhallaAPI,
    apiProxy,
    app,
) where

import API.Handlers (AppM (..), fetchAndCalculateAverageHandler, healthHandler)
import Servant
import Types (CalcResult (..))

app :: Application
app = serve apiProxy $ hoistServer apiProxy nt server

server :: ServerT ValhallaAPI AppM
server =
    healthHandler
        :<|> fetchAndCalculateAverageHandler

type HealthEndpoint =
    "health"
        :> Get '[JSON] String

type FetchAndCalculateAverageEndpoint =
    "fetch"
        :> QueryParam' '[Required, Strict] "sub_lon" Double
        :> QueryParam' '[Required, Strict] "sub_lat" Double
        :> QueryParam' '[Required, Strict] "num" Int
        :> Post '[JSON] CalcResult

type ValhallaAPI =
    HealthEndpoint
        :<|> FetchAndCalculateAverageEndpoint

nt :: AppM a -> Handler a
nt = unAppM

apiProxy :: Proxy ValhallaAPI
apiProxy = Proxy
