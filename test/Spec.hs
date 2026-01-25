module Main (main) where

import Test.Hspec

import qualified API.HandlersSpec
import qualified TypesSpec

main :: IO ()
main = hspec $ do
    describe "Types" TypesSpec.spec
    describe "API.Handlers" API.HandlersSpec.spec
