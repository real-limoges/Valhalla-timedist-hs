module TypesSpec (spec) where

import Data.Aeson (decode, eitherDecode, encode)
import Test.Hspec
import Types

spec :: Spec
spec = do
    describe "Location" $ do
        it "round-trips through JSON" $ do
            let loc = Location{lon = -122.4, lat = 37.8}
            decode (encode loc) `shouldBe` Just loc

    describe "TargetResult" $ do
        it "round-trips through JSON" $ do
            let result = TargetResult{time = 300.5, distance = 1000.0}
            decode (encode result) `shouldBe` Just result

    describe "MatrixResponse" $ do
        it "parses Valhalla-style response" $ do
            let json = "{\"sources_to_targets\":[[{\"time\":300.5,\"distance\":1000.0}]]}"
            case eitherDecode json of
                Right (MatrixResponse sts) -> do
                    length sts `shouldBe` 1
                    length (head sts) `shouldBe` 1
                    head (head sts) `shouldBe` Just TargetResult{time = 300.5, distance = 1000.0}
                Left err -> expectationFailure err

        it "handles null entries in matrix" $ do
            let json = "{\"sources_to_targets\":[[null,{\"time\":100,\"distance\":500}]]}"
            case eitherDecode json of
                Right (MatrixResponse sts) -> do
                    length (head sts) `shouldBe` 2
                    head (head sts) `shouldBe` Nothing
                    (head sts !! 1) `shouldBe` Just TargetResult{time = 100, distance = 500}
                Left err -> expectationFailure err

        it "handles all null entries" $ do
            let json = "{\"sources_to_targets\":[[null,null,null]]}"
            case eitherDecode json of
                Right (MatrixResponse sts) -> do
                    length (head sts) `shouldBe` 3
                    all (== Nothing) (head sts) `shouldBe` True
                Left err -> expectationFailure err
