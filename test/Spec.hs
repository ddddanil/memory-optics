module Main (main) where

import           System.IO           (IO)
import           Test.Tasty

import qualified Spec.Monad.Storable

tests :: TestTree
tests = testGroup "Specs"
  [ Spec.Monad.Storable.tests
  ]

main :: IO ()
main = defaultMain tests
