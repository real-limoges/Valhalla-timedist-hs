module API.HandlersSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai (Application, Request (..))
import Network.Wai.Test (SRequest (..), SResponse (..), defaultRequest, runSession, setPath, srequest)
import Test.Hspec

import API.Api (appWithConfig)
import MockValhalla (MockResponse (..), partialResponse, successResponse, withMockValhalla)
import Types

-- Helper to build an AppConfig for testing
mkConfig :: String -> IO AppConfig
mkConfig url = do
    mgr <- newManager defaultManagerSettings
    pure AppConfig
        { valhallaUrl = ValhallaUrl (T.pack url)
        , httpManager = mgr
        , maxPoints = 10000
        }

mkApp :: AppConfig -> Application
mkApp = appWithConfig

-- ByteString.Lazy doesn't export isInfixOf, so use String conversion
isInfixOfLazy :: String -> BL8.ByteString -> Bool
isInfixOfLazy needle haystack = needle `isInfixOf` BL8.unpack haystack

-- Helper to build a POST /time_distance request
mkTimeDistReq :: TimeDistanceRequest -> SRequest
mkTimeDistReq reqBody =
    let req = setPath
                defaultRequest
                    { requestMethod = methodPost
                    , requestHeaders = [(hContentType, "application/json")]
                    }
                "/v1/time_distance"
    in SRequest req (encode reqBody)

spec :: Spec
spec = do
    describe "GET /v1/health" $ do
        it "returns OK" $ do
            config <- mkConfig ""
            let app = mkApp config
                req = setPath defaultRequest "/v1/health"
            resp <- runSession (srequest $ SRequest req "") app
            simpleStatus resp `shouldBe` toEnum 200

    describe "POST /v1/time_distance" $ do
        it "returns time/distance results for given points" $ do
            withMockValhalla (SuccessResponse $ successResponse 2) $ \port -> do
                config <- mkConfig ("http://localhost:" ++ show port)
                let app = mkApp config
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location (Longitude (-122.4)) (Latitude 37.8)
                            , points = [ Location (Longitude (-122.5)) (Latitude 37.9)
                                       , Location (Longitude (-122.6)) (Latitude 38.0)
                                       ]
                            , costing = Auto :| []
                            }
                resp <- runSession (srequest $ mkTimeDistReq reqBody) app
                simpleStatus resp `shouldBe` toEnum 200
                case eitherDecode (simpleBody resp) :: Either String [CostingResult] of
                    Right [cr] -> do
                        costingModel cr `shouldBe` Auto
                        length (results cr) `shouldBe` 2
                    Right other -> expectationFailure $ "Expected 1 costing result, got " ++ show (length other)
                    Left err -> expectationFailure err

        it "handles Valhalla errors gracefully" $ do
            withMockValhalla (ErrorResponse 503 "Service unavailable") $ \port -> do
                config <- mkConfig ("http://localhost:" ++ show port)
                let app = mkApp config
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location (Longitude 0) (Latitude 0)
                            , points = [Location (Longitude 1) (Latitude 1)]
                            , costing = Auto :| []
                            }
                resp <- runSession (srequest $ mkTimeDistReq reqBody) app
                simpleStatus resp `shouldBe` toEnum 500
                isInfixOfLazy "Routing engine error" (simpleBody resp) `shouldBe` True

        it "returns correct result structure with partial results" $ do
            withMockValhalla (SuccessResponse $ partialResponse 1 1) $ \port -> do
                config <- mkConfig ("http://localhost:" ++ show port)
                let app = mkApp config
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location (Longitude (-122.4)) (Latitude 37.8)
                            , points = [ Location (Longitude (-122.5)) (Latitude 37.9)
                                       , Location (Longitude (-122.6)) (Latitude 38.0)
                                       ]
                            , costing = Auto :| []
                            }
                resp <- runSession (srequest $ mkTimeDistReq reqBody) app
                simpleStatus resp `shouldBe` toEnum 200
                case eitherDecode (simpleBody resp) :: Either String [CostingResult] of
                    Right [cr] -> do
                        let res = results cr
                        length res `shouldBe` 2
                        case res of
                            [r1, r2] -> do
                                r1 `shouldSatisfy` (/= Nothing)
                                r2 `shouldBe` Nothing
                            _ -> expectationFailure "Expected 2 results"
                    Right _ -> expectationFailure "Expected 1 costing result"
                    Left err -> expectationFailure err

        it "returns 400 when too many points requested" $ do
            config <- mkConfig ""
            let app = mkApp config
                tooManyPoints = replicate (maxPoints config + 1) (Location (Longitude 0) (Latitude 0))
                reqBody =
                    TimeDistanceRequest
                        { subject = Location (Longitude 0) (Latitude 0)
                        , points = tooManyPoints
                        , costing = Auto :| []
                        }
            resp <- runSession (srequest $ mkTimeDistReq reqBody) app
            simpleStatus resp `shouldBe` toEnum 400
            isInfixOfLazy "Too many points" (simpleBody resp) `shouldBe` True

        it "returns 400 when no costing models provided" $ do
            -- NonEmpty prevents constructing an empty costing list at compile time,
            -- so we send raw JSON with an empty array to test Servant's parsing
            config <- mkConfig ""
            let app = mkApp config
                rawJson = "{\"subject\":{\"lon\":0,\"lat\":0},\"points\":[{\"lon\":1,\"lat\":1}],\"costing\":[]}"
                req =
                    setPath
                        defaultRequest
                            { requestMethod = methodPost
                            , requestHeaders = [(hContentType, "application/json")]
                            }
                        "/v1/time_distance"
                sreq = SRequest req rawJson
            resp <- runSession (srequest sreq) app
            simpleStatus resp `shouldBe` toEnum 400

        it "succeeds with exactly maxPoints points" $ do
            let limit = 3
            withMockValhalla (SuccessResponse $ successResponse limit) $ \port -> do
                config <- mkConfig ("http://localhost:" ++ show port)
                let app = mkApp config{maxPoints = limit}
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location (Longitude 0) (Latitude 0)
                            , points = replicate limit (Location (Longitude 1) (Latitude 1))
                            , costing = Auto :| []
                            }
                resp <- runSession (srequest $ mkTimeDistReq reqBody) app
                simpleStatus resp `shouldBe` toEnum 200

        it "returns 400 for malformed JSON body" $ do
            config <- mkConfig ""
            let app = mkApp config
                req =
                    setPath
                        defaultRequest
                            { requestMethod = methodPost
                            , requestHeaders = [(hContentType, "application/json")]
                            }
                        "/v1/time_distance"
                sreq = SRequest req "this is not json {{{"
            resp <- runSession (srequest sreq) app
            simpleStatus resp `shouldBe` toEnum 400

        it "returns multiple result sets for multiple costing models" $ do
            withMockValhalla (SuccessResponse $ successResponse 2) $ \port -> do
                config <- mkConfig ("http://localhost:" ++ show port)
                let app = mkApp config
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location (Longitude (-122.4)) (Latitude 37.8)
                            , points = [ Location (Longitude (-122.5)) (Latitude 37.9)
                                       , Location (Longitude (-122.6)) (Latitude 38.0)
                                       ]
                            , costing = Auto :| [Bicycle]
                            }
                resp <- runSession (srequest $ mkTimeDistReq reqBody) app
                simpleStatus resp `shouldBe` toEnum 200
                case eitherDecode (simpleBody resp) :: Either String [CostingResult] of
                    Right crs -> do
                        length crs `shouldBe` 2
                        case crs of
                            [cr1, cr2] -> do
                                costingModel cr1 `shouldBe` Auto
                                costingModel cr2 `shouldBe` Bicycle
                            _ -> expectationFailure "Expected 2 costing results"
                    Left err -> expectationFailure err
