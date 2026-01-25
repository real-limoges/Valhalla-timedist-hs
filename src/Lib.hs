module Lib (runServer) where

import API.Api (appWithConfig, defaultValhallaUrl)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

runServer :: IO ()
runServer = do
    valhallaUrl <- fromMaybe defaultValhallaUrl <$> lookupEnv "VALHALLA_URL"
    port <- fromMaybe 9000 . (>>= readMaybe) <$> lookupEnv "PORT"
    putStrLn $ "Starting server on http://localhost:" ++ show port ++ " (Valhalla at " ++ valhallaUrl ++ ")"
    run port (appWithConfig valhallaUrl)
