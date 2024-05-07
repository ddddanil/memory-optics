module Data.Memory
  ( Pointer(Offset, compareOffset)
  , offset
  , NativeType
  , readNative
  , Memory(ReadMem, WriteMem)
  , readMem, writeMem
  , read
  , runMemoryAsNative
  , runMemoryMonadIO
  )
where

import           Control.Lens.Getter  (Getter, to)
import           Control.Lens.Monadic (ExistentialMonadicOptic (ExistentialMonadicOptic),
                                       MonadicOptic, monadicOptic)
import           Control.Monad        (Monad (return, (>>)))
import           Data.Function        (($))
import           Data.Kind            (Type)
import           Data.Ord             (Ordering, compare)
import           Foreign              (Int)
import qualified Foreign.Ptr          as GHC
import           Foreign.Storable     (Storable (peek, poke))
import           GHC.IO               (IO)
import           Polysemy             (Embed, InterpreterFor, Member, Sem,
                                       embed, interpret, makeSem)
import           Polysemy.Embed       (runEmbedded)

class Pointer p where
  data Offset p a b :: Type
  -- | Unsafe
  addOffset :: p a -> Offset p a b -> p b
  -- | Unsafe
  unsafeCastPointer :: p a -> p b
  compareOffset :: Offset p a b -> Offset p a c -> Ordering

type OffsetOptic p a b = Getter (p a) (p b)

offset :: (Pointer p) => Offset p a b -> OffsetOptic p a b
offset o = to $ \p -> p `addOffset` o

class (Pointer p) => NativeType p a where
  data MemoryMonad p a :: Type
  readMemM :: p a -> MemoryMonad p a
  writeMemM :: p a -> a -> MemoryMonad p a

readNative :: forall p a b. (NativeType p a, NativeType p b, Monad (MemoryMonad p)) => MonadicOptic (MemoryMonad p) (p a) (p b) a b
readNative = let
  read_ :: p a -> (MemoryMonad p) (a, p b)
  read_ ptr = do
    val <- readMemM ptr
    return (val, unsafeCastPointer ptr)
  write_ :: p b -> b -> (MemoryMonad p) (p b)
  write_ ptr d = writeMemM ptr d >> return ptr
  in monadicOptic (ExistentialMonadicOptic read_ write_)

data Memory p m a where
  ReadMem :: (NativeType p a) => p a -> Memory p m a
  WriteMem :: (NativeType p a) => p a -> a -> Memory p m a

makeSem ''Memory

read :: forall r p a b. (NativeType p a, NativeType p b, Member (Memory p) r) => MonadicOptic (Sem r) (p a) (p b) a b
read = let
  read_ :: p a -> Sem r (a, p b)
  read_ ptr = do
    val <- readMem ptr
    return (val, unsafeCastPointer ptr)
  write_ :: p b -> b -> Sem r (p b)
  write_ ptr d = writeMem ptr d >> return ptr
  in monadicOptic (ExistentialMonadicOptic @(Sem r) read_ write_)

runMemoryAsNative :: (Member (Embed (MemoryMonad p)) r) => InterpreterFor (Memory p) r
runMemoryAsNative = interpret $ \case
  ReadMem ptr -> embed $ readMemM ptr
  WriteMem ptr d -> embed $ writeMemM ptr d

instance Pointer GHC.Ptr where
  data Offset GHC.Ptr a b = GHCPtrOffset Int
  addOffset p (GHCPtrOffset o) = p `GHC.plusPtr` o
  unsafeCastPointer = GHC.castPtr
  compareOffset (GHCPtrOffset a) (GHCPtrOffset b) = a `compare` b

instance Storable a => NativeType GHC.Ptr a where
  data MemoryMonad GHC.Ptr a = MemoryMonadIO (IO a)
  readMemM ptr = MemoryMonadIO $ peek ptr
  writeMemM ptr d = MemoryMonadIO $ poke ptr d >> return d

runMemoryMonadIO :: Member (Embed IO) r => InterpreterFor (Embed (MemoryMonad GHC.Ptr)) r
runMemoryMonadIO = runEmbedded $ \(MemoryMonadIO m) -> m
