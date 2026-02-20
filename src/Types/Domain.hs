module Types.Domain (
    Longitude (..),
    Latitude (..),
    Location (..),
    Seconds (..),
    Meters (..),
    ValhallaUrl (..),
    CostingModel (..),
    costingToText,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Longitude in degrees, must be in [-180, 180]
newtype Longitude = Longitude {unLongitude :: Double}
    deriving stock (Show, Eq)
    deriving newtype (FromJSON, ToJSON)

-- | Latitude in degrees, must be in [-90, 90]
newtype Latitude = Latitude {unLatitude :: Double}
    deriving stock (Show, Eq)
    deriving newtype (FromJSON, ToJSON)

-- | Geographic coordinate
data Location = Location
    { lon :: Longitude
    , lat :: Latitude
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON Location where
    parseJSON = withObject "Location" $ \v -> do
        lo <- v .: "lon"
        la <- v .: "lat"
        let Longitude loVal = lo
            Latitude laVal = la
        if laVal < -90 || laVal > 90
            then fail $ "lat must be in [-90, 90], got: " ++ show laVal
            else
                if loVal < -180 || loVal > 180
                    then fail $ "lon must be in [-180, 180], got: " ++ show loVal
                    else pure (Location lo la)

instance ToJSON Location where
    toJSON loc = object ["lon" .= lon loc, "lat" .= lat loc]

-- | Time duration in seconds
newtype Seconds = Seconds {unSeconds :: Double}
    deriving stock (Show, Eq)
    deriving newtype (FromJSON, ToJSON)

-- | Distance in meters
newtype Meters = Meters {unMeters :: Double}
    deriving stock (Show, Eq)
    deriving newtype (FromJSON, ToJSON)

-- | Base URL for the Valhalla routing engine
newtype ValhallaUrl = ValhallaUrl {unValhallaUrl :: Text}
    deriving stock (Show, Eq)

-- | Supported Valhalla costing models
data CostingModel = Auto | Bicycle | Pedestrian | Truck
    deriving stock (Show, Eq, Generic)

instance FromJSON CostingModel where
    parseJSON = withText "CostingModel" $ \case
        "auto" -> pure Auto
        "bicycle" -> pure Bicycle
        "pedestrian" -> pure Pedestrian
        "truck" -> pure Truck
        other -> fail $ "Unknown costing model: " ++ T.unpack other

instance ToJSON CostingModel where
    toJSON = toJSON . costingToText

costingToText :: CostingModel -> Text
costingToText = \case
    Auto -> "auto"
    Bicycle -> "bicycle"
    Pedestrian -> "pedestrian"
    Truck -> "truck"
