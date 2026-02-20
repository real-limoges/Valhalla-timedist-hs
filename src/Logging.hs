module Logging (
    logInfo,
    logWarn,
    logError,
    jsonRequestLogger,
) where

import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.Aeson.Key (fromText)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware, rawPathInfo, requestMethod, responseStatus)
import System.IO (Handle, hFlush, stderr, stdout)

logInfo :: Text -> [(Text, Value)] -> IO ()
logInfo = logLine stdout "info"

logWarn :: Text -> [(Text, Value)] -> IO ()
logWarn = logLine stdout "warn"

logError :: Text -> [(Text, Value)] -> IO ()
logError = logLine stderr "error"

logLine :: Handle -> Text -> Text -> [(Text, Value)] -> IO ()
logLine h level msg fields = do
    t <- getCurrentTime
    BL.hPutStrLn h $
        encode $
            object $
                [ "level" .= level
                , "timestamp" .= show t
                , "message" .= msg
                ]
                    ++ map (\(k, v) -> fromText k .= v) fields
    hFlush h

-- | WAI middleware that logs each request as a JSON line to stdout.
jsonRequestLogger :: Middleware
jsonRequestLogger app req respond = do
    start <- getCurrentTime
    app req $ \response -> do
        end <- getCurrentTime
        let ms = realToFrac (diffUTCTime end start) * 1000 :: Double
            st = responseStatus response
        logLine
            stdout
            "info"
            "request"
            [ ("method", toJSON (decodeUtf8 (requestMethod req)))
            , ("path", toJSON (decodeUtf8 (rawPathInfo req)))
            , ("status", toJSON (statusCode st))
            , ("duration_ms", toJSON ms)
            ]
        respond response
