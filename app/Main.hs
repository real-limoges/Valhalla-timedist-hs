{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty (scotty, get, json)
import Control.Monad.IO.Class (liftIO)
import ValhallaClient (fetchAndCalculateAverage)

main :: IO ()
main = do
    putStrLn "Starting Microservice on http://localhost:3000"

    scotty 3000 $ do
        get "/calculate_average" $ do
            liftIO $ putStrLn "Received request... calling Valhalla"
            eResult <- liftIO fetchAndCalculateAverage

            case eResult of
                Left err  -> json ("Error: " ++ err::String)
                Right res -> json res