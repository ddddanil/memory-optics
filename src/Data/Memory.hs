module Data.Memory
( Pointer(Offset, addOffset)
, offset
, Memory(ReadMem, WriteMem), readMem, writeMem
, runMemoryToIO
)
where

import           Control.Lens.Getter (Getter, to)
import           Data.Kind           (Type)
import qualified Foreign.Ptr         as GHC
import           Foreign.Storable    (Storable (peek, poke))
import           Polysemy            (Embed, InterpreterFor, Member, interpret,
                                      makeSem)
import           Polysemy.Embed      (embed)

class Storable a => Pointer p a where
  data Offset p b :: Type
  -- | Unsafe. Get a pointer to
  addOffset :: Storable b => p a -> Offset p b -> p b

instance Storable a => Pointer GHC.Ptr a where
  data Offset GHC.Ptr b = GHCPtrOffset Int
  addOffset p (GHCPtrOffset o) = p `GHC.plusPtr` o

offset :: (Pointer p a, Storable b) => Offset p b -> Getter (p a) (p b)
offset o = let
  getter p = p `addOffset` o
  in to getter

data Memory p m a where
  ReadMem :: Pointer p n => p n -> Memory p m n
  WriteMem :: Pointer p n => n -> p n -> Memory p m ()

makeSem ''Memory

runMemoryToIO :: Member (Embed IO) r => InterpreterFor (Memory GHC.Ptr) r
runMemoryToIO = interpret \case
   ReadMem p -> do { embed $ peek p }
   WriteMem d a -> do { embed $ poke a d }

