module Spec.Abi.Native where

import           Control.Lens                 ((&))
import           Control.Lens.Monadic         (getM, putM)
import           Control.Monad                (return)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Morph          (hoist, lift)
import           Control.Monad.Trans.Resource (allocate, runResourceT)
import           Data.Function                (($), (.))
import           Data.Memory.Abi              (deref)
import           Data.Memory.Abi.Native       (Native)
import           Data.Proxy                   (Proxy (Proxy))
import           Foreign.Marshal.Alloc        (free, malloc)
import           GHC.Word                     (Word8)

import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range
import           Test.Tasty.Hedgehog

n :: Proxy Native
n = Proxy

prop_PutGet_onWord8 :: Property
prop_PutGet_onWord8 = property . hoist runResourceT $ do
  val <- forAll $ Gen.word8 (Range.linearBounded)
  (_, p) <- lift $ allocate (malloc @Word8) free
  p' <- liftIO $ p & (deref n) `putM` val
  p === p'
  r <- liftIO $ p & getM (deref n)
  r === val
  return ()

tests = fromGroup $$(discover)
