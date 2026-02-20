module Types.Valhalla (
    TargetResult (..),
    MatrixResponse (..),
    MatrixRequest (..),
) where

import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Domain (Location, Meters, Seconds)

-- | Result for a single target point from Valhalla
data TargetResult = TargetResult
    { time :: Seconds
    , distance :: Meters
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Valhalla matrix API response
newtype MatrixResponse = MatrixResponse
    { sources_to_targets :: [[Maybe TargetResult]]
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Request payload for the Valhalla matrix API
data MatrixRequest = MatrixRequest
    { sources :: [Location]
    , targets :: [Location]
    , matrixCosting :: Text
    , requestId :: Text
    }
    deriving stock (Show, Generic)

instance ToJSON MatrixRequest where
    toJSON mr =
        object
            [ "sources" .= sources mr
            , "targets" .= targets mr
            , "costing" .= matrixCosting mr
            , "id" .= requestId mr
            ]
