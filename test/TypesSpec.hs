module TypesSpec (spec) where

import Data.Aeson (decode, eitherDecode, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson (Value (..))
import Test.Hspec
import Types

spec :: Spec
spec = do
    describe "Location" $ do
        it "round-trips through JSON" $ do
            let loc = Location{lon = Longitude (-122.4), lat = Latitude 37.8}
            decode (encode loc) `shouldBe` Just loc

        it "rejects out-of-range latitude" $ do
            let json = "{\"lon\": 0.0, \"lat\": 91.0}"
            (decode json :: Maybe Location) `shouldBe` Nothing

        it "rejects out-of-range longitude" $ do
            let json = "{\"lon\": 181.0, \"lat\": 0.0}"
            (decode json :: Maybe Location) `shouldBe` Nothing

    describe "TargetResult" $ do
        it "round-trips through JSON" $ do
            let result = TargetResult{time = Seconds 300.5, distance = Meters 1000.0}
            decode (encode result) `shouldBe` Just result

    describe "MatrixResponse" $ do
        it "parses Valhalla-style response" $ do
            let json = "{\"sources_to_targets\":[[{\"time\":300.5,\"distance\":1000.0}]]}"
            case eitherDecode json of
                Right (MatrixResponse [row]) -> do
                    length row `shouldBe` 1
                    case row of
                        [r] -> r `shouldBe` Just TargetResult{time = Seconds 300.5, distance = Meters 1000.0}
                        _ -> expectationFailure "Expected 1 element in row"
                Right _ -> expectationFailure "Expected 1 row"
                Left err -> expectationFailure err

        it "handles null entries in matrix" $ do
            let json = "{\"sources_to_targets\":[[null,{\"time\":100,\"distance\":500}]]}"
            case eitherDecode json of
                Right (MatrixResponse [row]) -> do
                    length row `shouldBe` 2
                    case row of
                        [r1, r2] -> do
                            r1 `shouldBe` Nothing
                            r2 `shouldBe` Just TargetResult{time = Seconds 100, distance = Meters 500}
                        _ -> expectationFailure "Expected 2 elements in row"
                Right _ -> expectationFailure "Expected 1 row"
                Left err -> expectationFailure err

        it "handles all null entries" $ do
            let json = "{\"sources_to_targets\":[[null,null,null]]}"
            case eitherDecode json of
                Right (MatrixResponse [row]) -> do
                    length row `shouldBe` 3
                    all (== Nothing) row `shouldBe` True
                Right _ -> expectationFailure "Expected 1 row"
                Left err -> expectationFailure err

    describe "MatrixRequest" $ do
        it "serializes field names correctly for Valhalla" $ do
            let mr = MatrixRequest
                    { sources = [Location (Longitude 1.0) (Latitude 2.0)]
                    , targets = [Location (Longitude 3.0) (Latitude 4.0)]
                    , matrixCosting = "auto"
                    , requestId = "test-id"
                    }
                json = encode mr
            case decode json of
                Just (Object o) -> do
                    KM.member "costing" o `shouldBe` True
                    KM.member "id" o `shouldBe` True
                    KM.member "matrixCosting" o `shouldBe` False
                    KM.member "requestId" o `shouldBe` False
                _ -> expectationFailure "Expected JSON object"

    describe "CostingModel" $ do
        it "round-trips all models through JSON" $ do
            mapM_ (\cm -> decode (encode cm) `shouldBe` Just cm)
                [Auto, Bicycle, Pedestrian, Truck]

        it "rejects unknown costing model" $ do
            (decode "\"skateboard\"" :: Maybe CostingModel) `shouldBe` Nothing
