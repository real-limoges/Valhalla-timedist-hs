module Types.Config (
    AppConfig (..),
) where

import Network.HTTP.Client (Manager)
import Types.Domain (ValhallaUrl)

-- | Application configuration
data AppConfig = AppConfig
    { valhallaUrl :: ValhallaUrl
    , httpManager :: Manager
    , maxPoints :: Int
    }
