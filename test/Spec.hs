module Main (main) where

import           System.IO           (IO)
import           Test.Tasty

import qualified Spec.Abi.Native

tests :: TestTree
tests = testGroup "Specs"
  [ Spec.Abi.Native.tests
  ]

main :: IO ()
main = defaultMain tests
