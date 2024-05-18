module Main (main) where

import           Control.Lens         ((&))
import           Control.Lens.Monadic (MonadicLens', getM, putM)
import           Control.Monad        (Monad (return, (>>)))
import           Data.Memory          (MemoryMonad, NativeType, read')
import           Foreign.Ptr          (Ptr)
import           GHC.Word             (Word16, Word32, Word64, Word8)
import           System.IO            (IO, putStrLn)

readLens :: MonadicLens' IO (Ptr Word8) Word8
readLens = read'

putWord :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
putWord = putM readLens

something :: Ptr Word8 -> IO ()
something p = putWord 5 p >> return ()

testPutGet_onWord8 :: Ptr Word8 -> IO ()
testPutGet_onWord8 p = do
  p' <- p & read' `putM`  5
  r <- p & getM read'
  return ()

main :: IO ()
main = putStrLn "Test suite not yet implemented"
