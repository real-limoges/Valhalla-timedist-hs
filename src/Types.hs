module Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Location = Location
    { lon :: Double
    , lat :: Double
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

-- These are for interfacing with Valhalla
data MatrixRequest = MatrixRequest
    { sources :: [Location]
    , targets :: [Location]
    , costing :: String
    , _id :: String
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

data TargetResult = TargetResult
    { time :: Double
    , distance :: Double
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

newtype MatrixResponse = MatrixResponse
    { sources_to_targets :: [[Maybe TargetResult]]
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

-- This is the public result
data CalcResult = CalcResult
    { routesFound :: Int
    , routesNotFound :: Int
    , averageMinutes :: Double
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)
