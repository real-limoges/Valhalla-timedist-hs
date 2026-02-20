module Server (runServer) where

import API.Api (appWithConfig)
import Data.Aeson (toJSON)
import Data.Text qualified as T
import Logging (jsonRequestLogger, logInfo, logWarn)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setGracefulShutdownTimeout, setPort)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Types (AppConfig (..), ValhallaUrl (..))
import Valhalla.Client (checkStatus)

defaultValhallaUrl :: ValhallaUrl
defaultValhallaUrl = ValhallaUrl "http://localhost:8002"

defaultMaxPoints :: Int
defaultMaxPoints = 10000

runServer :: IO ()
runServer = do
    vUrl <- maybe defaultValhallaUrl (ValhallaUrl . T.pack) <$> lookupEnv "VALHALLA_URL"
    portEnv <- lookupEnv "PORT"
    port <- case portEnv of
        Nothing -> pure 9000
        Just s -> case readMaybe s of
            Just p -> pure p
            Nothing -> do
                hPutStrLn stderr $ "Warning: invalid PORT value '" ++ s ++ "', using default 9000"
                pure 9000

    maxPts <- maybe defaultMaxPoints id . (>>= readMaybe) <$> lookupEnv "MAX_POINTS"

    manager <- newManager tlsManagerSettings

    let config = AppConfig
            { valhallaUrl = vUrl
            , httpManager = manager
            , maxPoints = maxPts
            }
        settings = setPort port
                 $ setGracefulShutdownTimeout (Just 30)
                 $ defaultSettings

    -- Validate Valhalla URL at startup
    eStatus <- checkStatus config
    case eStatus of
        Left ex ->
            logWarn
                "Valhalla unreachable at startup"
                [ ("url", toJSON (unValhallaUrl vUrl))
                , ("error", toJSON (T.pack (show ex)))
                ]
        Right _ ->
            logInfo "Valhalla reachable" [("url", toJSON (unValhallaUrl vUrl))]

    logInfo
        "Server starting"
        [ ("port", toJSON port)
        , ("valhalla_url", toJSON (unValhallaUrl vUrl))
        , ("max_points", toJSON maxPts)
        ]

    runSettings settings (jsonRequestLogger $ appWithConfig config)
