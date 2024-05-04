{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Memory
( Pointer(Offset)
, offset
, Memory(ReadMem, WriteMem), readMem, writeMem
, read
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

class Pointer p where
  data Offset p b :: Type
  -- | Unsafe. Get a pointer to
  addOffset :: (Storable b) => p a -> Offset p b -> p b
  -- | Unsafe
  unsafeCastPointer :: Storable b => p a -> p b

instance Pointer GHC.Ptr where
  data Offset GHC.Ptr b = GHCPtrOffset Int
  addOffset p (GHCPtrOffset o) = p `GHC.plusPtr` o
  unsafeCastPointer = GHC.castPtr

offset :: (Pointer p, Storable b) => Offset p b -> Getter (p a) (p b)
offset o = let
  getter p = p `addOffset` o
  in to getter

data Memory p m a where
  ReadMem :: (Storable n, Pointer p) => p n -> Memory p m n
  WriteMem :: (Storable n, Pointer p) => n -> p n -> Memory p m ()

makeSem ''Memory

read :: forall r p a b. (Storable a, Storable b, Pointer p, Member (Memory p) r) => MonadicOptic (Sem r) (p a) (p b) a b
read = let
  read_ :: p a -> Sem r (a, p b)
  read_ ptr = do
    val <- readMem ptr
    return (val, unsafeCastPointer ptr)
  write_ :: p b -> b -> Sem r (p b)
  write_ ptr d = writeMem d ptr >> return ptr
  in monadicOptic @(Sem r) (ExistentialMonadicOptic read_ write_)

runMemoryToIO :: Member (Embed IO) r => InterpreterFor (Memory GHC.Ptr) r
runMemoryToIO = interpret \case
   ReadMem p -> do { embed $ peek p }
   WriteMem d a -> do { embed $ poke a d }

