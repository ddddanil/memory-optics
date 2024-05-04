module Data.Memory
( Pointer(Offset)
, Memory(ReadMem, WriteMem), readMem, writeMem
, runMemoryToIO
)
where

import           Data.Kind        (Type)
import qualified Foreign.Ptr      as GHC
import           Foreign.Storable (Storable (peek, poke))
import           Polysemy         (Embed, InterpreterFor, Member, interpret,
                                   makeSem)
import           Polysemy.Embed   (embed)

class Storable a => Pointer p a where
  data Offset p b :: Type

instance Storable a => Pointer GHC.Ptr a where
  data Offset GHC.Ptr b = GHCPtrOffset Int

data Memory p m a where
  ReadMem :: Pointer p n => p n -> Memory p m n
  WriteMem :: Pointer p n => n -> p n -> Memory p m ()

makeSem ''Memory

runMemoryToIO :: Member (Embed IO) r => InterpreterFor (Memory GHC.Ptr) r
runMemoryToIO = interpret \case
   ReadMem p -> do { embed $ peek p }
   WriteMem d a -> do { embed $ poke a d }

