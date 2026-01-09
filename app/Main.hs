module Main (main) where

import API.Api (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn "Starting Microservice on http://localhost:3000"
    run 9000 app