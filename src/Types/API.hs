module Types.API (
    TimeDistanceRequest (..),
    CostingResult (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Types.Domain (CostingModel, Location)
import Types.Valhalla (TargetResult)

-- | Request for time/distance calculation
data TimeDistanceRequest = TimeDistanceRequest
    { subject :: Location
    , points :: [Location]
    , costing :: NonEmpty CostingModel
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Response for time/distance calculation (one per costing model)
data CostingResult = CostingResult
    { costingModel :: CostingModel
    , results :: [Maybe TargetResult]
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON CostingResult where
    toJSON cr =
        object
            [ "costing" .= costingModel cr
            , "results" .= results cr
            ]

instance FromJSON CostingResult where
    parseJSON = withObject "CostingResult" $ \v ->
        CostingResult <$> v .: "costing" <*> v .: "results"
