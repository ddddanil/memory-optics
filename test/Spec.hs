module Main (main) where

import           Control.Lens         ((&))
import           Control.Lens.Monadic (MonadicLens', getM, putM)
import           Control.Monad        (Monad (return, (>>)), void)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Resource (runResourceT, allocate)
import           Data.Function        (($), (.))
import           Data.Memory.Effect          (deref')
import           Foreign.Marshal.Alloc(malloc, free)
import           Foreign.Ptr          (Ptr)
import           GHC.Word             (Word16, Word32, Word64, Word8)
import           System.IO            (IO, putStrLn)

import           Hedgehog
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

readLens :: MonadicLens' IO (Ptr Word8) Word8
readLens = deref'

putWord :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
putWord = putM readLens

something :: Ptr Word8 -> IO ()
something p = putWord 5 p >> return ()

prop_PutGet_onWord8 :: Property
prop_PutGet_onWord8 = property . hoist runResourceT $ do
  val <- forAll $ Gen.word8 (Range.linearBounded)
  (_, p) <- allocate (malloc @Word8) free
  p' <- (p & (deref' `putM` val))
  p === p'
  r <- p & getM deref'
  r === val
  return ()

main :: IO ()
main = void $ checkParallel $$(discover)
