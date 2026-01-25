module Types where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import GHC.Generics (Generic)

data Location = Location
    { lon :: Double
    , lat :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- These are for interfacing with Valhalla
data MatrixRequest = MatrixRequest
    { sources :: [Location]
    , targets :: [Location]
    , matrixCosting :: String
    , _id :: String
    }
    deriving stock (Show, Generic)

instance ToJSON MatrixRequest where
    toJSON mr = object
        [ "sources" .= sources mr
        , "targets" .= targets mr
        , "costing" .= matrixCosting mr
        , "id" .= _id mr
        ]

data TargetResult = TargetResult
    { time :: Double
    , distance :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype MatrixResponse = MatrixResponse
    { sources_to_targets :: [[Maybe TargetResult]]
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Supported Valhalla costing models
data CostingModel = Auto | Bicycle | Pedestrian | Truck
    deriving stock (Show, Eq, Generic)

instance FromJSON CostingModel where
    parseJSON = withText "CostingModel" $ \case
        "auto" -> pure Auto
        "bicycle" -> pure Bicycle
        "pedestrian" -> pure Pedestrian
        "truck" -> pure Truck
        other -> fail $ "Unknown costing model: " ++ show other

instance ToJSON CostingModel where
    toJSON = toJSON . costingToString

costingToString :: CostingModel -> String
costingToString = \case
    Auto -> "auto"
    Bicycle -> "bicycle"
    Pedestrian -> "pedestrian"
    Truck -> "truck"

-- Request for time/distance calculation
data TimeDistanceRequest = TimeDistanceRequest
    { subject :: Location
    , points :: [Location]
    , costing :: [CostingModel]  -- at least one required
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Response for time/distance calculation (one per costing model)
data CostingResult = CostingResult
    { costingModel :: CostingModel
    , results :: [Maybe TargetResult]
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON CostingResult where
    toJSON cr = object
        [ "costing" .= costingModel cr
        , "results" .= results cr
        ]

instance FromJSON CostingResult where
    parseJSON = withObject "CostingResult" $ \v ->
        CostingResult <$> v .: "costing" <*> v .: "results"
