module API.HandlersSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai (Application, Request (..))
import Network.Wai.Test (SRequest (..), SResponse (..), defaultRequest, runSession, setPath, srequest)
import Test.Hspec
import Test.Hspec.Wai

import API.Api (appWithConfig)
import API.Handlers (maxPoints)
import MockValhalla (MockResponse (..), partialResponse, successResponse, withMockValhalla)
import Types (CostingModel (..), CostingResult (..), Location (..), TimeDistanceRequest (..))

mkApp :: String -> Application
mkApp valhallaUrl = appWithConfig valhallaUrl

spec :: Spec
spec = do
    describe "GET /health" $ do
        with (pure $ mkApp "") $ do
            it "returns OK" $ do
                get "/health" `shouldRespondWith` "\"OK\"" {matchStatus = 200}

    describe "POST /time_distance" $ do
        it "returns time/distance results for given points" $ do
            withMockValhalla (SuccessResponse $ successResponse 2) $ \port -> do
                let valhallaUrl = "http://localhost:" ++ show port
                    app = mkApp valhallaUrl
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location (-122.4) 37.8
                            , points = [Location (-122.5) 37.9, Location (-122.6) 38.0]
                            , costing = [Auto]
                            }
                    req =
                        setPath
                            defaultRequest
                                { requestMethod = methodPost
                                , requestHeaders = [(hContentType, "application/json")]
                                }
                            "/time_distance"
                    sreq = SRequest req (encode reqBody)
                resp <- runSession (srequest sreq) app
                simpleStatus resp `shouldBe` toEnum 200
                case eitherDecode (simpleBody resp) :: Either String [CostingResult] of
                    Right [cr] -> do
                        costingModel cr `shouldBe` Auto
                        length (results cr) `shouldBe` 2
                    Right other -> expectationFailure $ "Expected 1 costing result, got " ++ show (length other)
                    Left err -> expectationFailure err

        it "handles Valhalla errors gracefully" $ do
            withMockValhalla (ErrorResponse 503 "Service unavailable") $ \port -> do
                let valhallaUrl = "http://localhost:" ++ show port
                    app = mkApp valhallaUrl
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location 0 0
                            , points = [Location 1 1]
                            , costing = [Auto]
                            }
                    req =
                        setPath
                            defaultRequest
                                { requestMethod = methodPost
                                , requestHeaders = [(hContentType, "application/json")]
                                }
                            "/time_distance"
                    sreq = SRequest req (encode reqBody)
                resp <- runSession (srequest sreq) app
                simpleStatus resp `shouldBe` toEnum 500

        it "returns correct result structure with partial results" $ do
            withMockValhalla (SuccessResponse $ partialResponse 1 1) $ \port -> do
                let valhallaUrl = "http://localhost:" ++ show port
                    app = mkApp valhallaUrl
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location (-122.4) 37.8
                            , points = [Location (-122.5) 37.9, Location (-122.6) 38.0]
                            , costing = [Auto]
                            }
                    req =
                        setPath
                            defaultRequest
                                { requestMethod = methodPost
                                , requestHeaders = [(hContentType, "application/json")]
                                }
                            "/time_distance"
                    sreq = SRequest req (encode reqBody)
                resp <- runSession (srequest sreq) app
                simpleStatus resp `shouldBe` toEnum 200
                case eitherDecode (simpleBody resp) :: Either String [CostingResult] of
                    Right [cr] -> do
                        let res = results cr
                        length res `shouldBe` 2
                        head res `shouldSatisfy` (/= Nothing)
                        res !! 1 `shouldBe` Nothing
                    Right _ -> expectationFailure "Expected 1 costing result"
                    Left err -> expectationFailure err

        it "returns 400 when too many points requested" $ do
            let app = mkApp ""
                tooManyPoints = replicate (maxPoints + 1) (Location 0 0)
                reqBody =
                    TimeDistanceRequest
                        { subject = Location 0 0
                        , points = tooManyPoints
                        , costing = [Auto]
                        }
                req =
                    setPath
                        defaultRequest
                            { requestMethod = methodPost
                            , requestHeaders = [(hContentType, "application/json")]
                            }
                        "/time_distance"
                sreq = SRequest req (encode reqBody)
            resp <- runSession (srequest sreq) app
            simpleStatus resp `shouldBe` toEnum 400

        it "returns 400 when no costing models provided" $ do
            let app = mkApp ""
                reqBody =
                    TimeDistanceRequest
                        { subject = Location 0 0
                        , points = [Location 1 1]
                        , costing = []
                        }
                req =
                    setPath
                        defaultRequest
                            { requestMethod = methodPost
                            , requestHeaders = [(hContentType, "application/json")]
                            }
                        "/time_distance"
                sreq = SRequest req (encode reqBody)
            resp <- runSession (srequest sreq) app
            simpleStatus resp `shouldBe` toEnum 400

        it "returns multiple result sets for multiple costing models" $ do
            withMockValhalla (SuccessResponse $ successResponse 2) $ \port -> do
                let valhallaUrl = "http://localhost:" ++ show port
                    app = mkApp valhallaUrl
                    reqBody =
                        TimeDistanceRequest
                            { subject = Location (-122.4) 37.8
                            , points = [Location (-122.5) 37.9, Location (-122.6) 38.0]
                            , costing = [Auto, Bicycle]
                            }
                    req =
                        setPath
                            defaultRequest
                                { requestMethod = methodPost
                                , requestHeaders = [(hContentType, "application/json")]
                                }
                            "/time_distance"
                    sreq = SRequest req (encode reqBody)
                resp <- runSession (srequest sreq) app
                simpleStatus resp `shouldBe` toEnum 200
                case eitherDecode (simpleBody resp) :: Either String [CostingResult] of
                    Right crs -> do
                        length crs `shouldBe` 2
                        costingModel (head crs) `shouldBe` Auto
                        costingModel (crs !! 1) `shouldBe` Bicycle
                    Left err -> expectationFailure err
