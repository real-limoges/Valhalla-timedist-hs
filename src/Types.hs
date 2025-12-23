{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson            (FromJSON (..), Object, ToJSON (..),
                                        Value (..), defaultOptions,
                                        genericParseJSON, genericToJSON, (.:))
import           GHC.Generics          (Generic)



-- This is a universal datatype
data Location = Location { lon :: Double, lat :: Double } deriving (Show, Generic, ToJSON)

-- These are for interfacing with Valhalla
data MatrixRequest = MatrixRequest
    { sources :: [Location]
    , targets :: [Location]
    , costing :: String
    , _id     :: String
    } deriving (Show, Generic, ToJSON)

data TargetResult = TargetResult { time :: Double, distance :: Double } deriving (Show, Generic)
instance FromJSON TargetResult where
    parseJSON (Object v) = TargetResult <$> v .: "time" <*> v .: "distance"
    parseJSON _          = fail "Expected an object for Target Result"

newtype MatrixResponse = MatrixResponse
    { sources_to_targets :: [[Maybe TargetResult]]
    } deriving (Show, Generic, FromJSON)


-- This is the public result
data CalcResult = CalcResult
    { routesFound    :: Int
    , routesNotFound :: Int
    , averageMinutes :: Double
    } deriving (Show, Generic)
instance ToJSON CalcResult where
    toJSON = genericToJSON defaultOptions