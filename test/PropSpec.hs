{-# OPTIONS_GHC -fno-warn-orphans #-}

module PropSpec (spec) where

import Data.Aeson (decode, encode, object, (.=))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Types

-- ---------------------------------------------------------------------------
-- Arbitrary instances
-- ---------------------------------------------------------------------------

instance Arbitrary Longitude where
    arbitrary = Longitude <$> choose (-180, 180)

instance Arbitrary Latitude where
    arbitrary = Latitude <$> choose (-90, 90)

instance Arbitrary Location where
    arbitrary = Location <$> arbitrary <*> arbitrary

instance Arbitrary Seconds where
    arbitrary = Seconds . abs <$> arbitrary

instance Arbitrary Meters where
    arbitrary = Meters . abs <$> arbitrary

instance Arbitrary CostingModel where
    arbitrary = elements [Auto, Bicycle, Pedestrian, Truck]

instance Arbitrary TargetResult where
    arbitrary = TargetResult <$> arbitrary <*> arbitrary

instance Arbitrary CostingResult where
    arbitrary = CostingResult <$> arbitrary <*> listOf arbitrary

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "Location" $ do
        prop "valid coordinates round-trip through JSON" $ \loc ->
            decode (encode (loc :: Location)) === Just loc

        prop "rejects latitude above 90" $ \(Positive (d :: Double)) ->
            let v = object ["lon" .= (0 :: Double), "lat" .= (90 + d)]
             in (decode (encode v) :: Maybe Location) === Nothing

        prop "rejects latitude below -90" $ \(Positive (d :: Double)) ->
            let v = object ["lon" .= (0 :: Double), "lat" .= ((-90) - d)]
             in (decode (encode v) :: Maybe Location) === Nothing

        prop "rejects longitude above 180" $ \(Positive (d :: Double)) ->
            let v = object ["lat" .= (0 :: Double), "lon" .= (180 + d)]
             in (decode (encode v) :: Maybe Location) === Nothing

        prop "rejects longitude below -180" $ \(Positive (d :: Double)) ->
            let v = object ["lat" .= (0 :: Double), "lon" .= ((-180) - d)]
             in (decode (encode v) :: Maybe Location) === Nothing

    describe "CostingModel" $ do
        prop "round-trips through JSON" $ \cm ->
            decode (encode (cm :: CostingModel)) === Just cm

    describe "TargetResult" $ do
        prop "round-trips through JSON" $ \tr ->
            decode (encode (tr :: TargetResult)) === Just tr

    describe "CostingResult" $ do
        prop "round-trips through JSON" $ \cr ->
            decode (encode (cr :: CostingResult)) === Just cr
