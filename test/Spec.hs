module Main (main) where

import           System.IO           (IO)
import           Test.Tasty

import qualified Spec.Abi.Native
import qualified Spec.Abi.C

tests :: TestTree
tests = testGroup "Specs"
  [ Spec.Abi.Native.tests
  , Spec.Abi.C.tests
  ]

main :: IO ()
main = defaultMain tests
