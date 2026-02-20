module MockValhalla (
    withMockValhalla,
    MockResponse (..),
    successResponse,
    emptyResponse,
    partialResponse,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Data.Aeson (encode)
import Network.HTTP.Types (Status (..), status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (Port, defaultSettings, openFreePort, runSettingsSocket, setBeforeMainLoop)
import Network.Socket (close)
import Types (MatrixResponse (..), Meters (..), Seconds (..), TargetResult (..))

data MockResponse
    = SuccessResponse MatrixResponse
    | ErrorResponse Int String

successResponse :: Int -> MatrixResponse
successResponse numTargets =
    MatrixResponse
        { sources_to_targets =
            [ [ Just
                    TargetResult
                        { time = Seconds 300.0
                        , distance = Meters 5000.0
                        }
              | _ <- [1 .. numTargets]
              ]
            ]
        }

emptyResponse :: MatrixResponse
emptyResponse = MatrixResponse{sources_to_targets = [[Nothing]]}

partialResponse :: Int -> Int -> MatrixResponse
partialResponse found notFound =
    MatrixResponse
        { sources_to_targets =
            [ replicate found (Just TargetResult{time = Seconds 300.0, distance = Meters 5000.0})
                ++ replicate notFound Nothing
            ]
        }

withMockValhalla :: MockResponse -> (Port -> IO a) -> IO a
withMockValhalla mockResp action = do
    (port, socket) <- openFreePort
    readyVar <- newEmptyMVar :: IO (MVar ())
    let settings = setBeforeMainLoop (putMVar readyVar ()) defaultSettings
        mockApp = mockValhallaApp mockResp
    bracket
        (forkIO $ runSettingsSocket settings socket mockApp)
        (\tid -> killThread tid >> close socket)
        (\_ -> takeMVar readyVar >> action port)

mockValhallaApp :: MockResponse -> Application
mockValhallaApp mockResp _req respond = do
    case mockResp of
        SuccessResponse matrixResp ->
            respond $
                responseLBS
                    status200
                    [("Content-Type", "application/json")]
                    (encode matrixResp)
        ErrorResponse code msg ->
            respond $
                responseLBS
                    (Status code "")
                    [("Content-Type", "text/plain")]
                    (encode msg)
