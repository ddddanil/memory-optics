{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Memory
( Pointer(Offset)
, offset
, NativeType(readMem, writeMem)
, read
)
where

import           Control.Lens.Getter  (Getter, to)
import           Control.Lens.Monadic (ExistentialMonadicOptic (ExistentialMonadicOptic),
                                       MonadicOptic, monadicOptic)
import           Control.Monad        (Monad (return, (>>)))
import           Data.Kind            (Type)
import           Foreign              (Int)
import qualified Foreign.Ptr          as GHC
import           Foreign.Storable     (Storable (peek, poke))
import           GHC.IO               (IO)

class Pointer p where
  data Offset p b :: Type
  -- | Unsafe. Get a pointer to
  addOffset :: p a -> Offset p b -> p b
  -- | Unsafe
  unsafeCastPointer :: p a -> p b

offset :: (Pointer p) => Offset p b -> Getter (p a) (p b)
offset o = let
  getter p = p `addOffset` o
  in to getter

class NativeType p m a where
  readMem :: p a -> m a
  writeMem :: p a -> a -> m ()

read :: forall m p a b. (Monad m, Pointer p, NativeType p m a, NativeType p m b) => MonadicOptic m (p a) (p b) a b
read = let
  read_ :: p a -> m (a, p b)
  read_ ptr = do
    val <- readMem ptr
    return (val, unsafeCastPointer ptr)
  write_ :: p b -> b -> m (p b)
  write_ ptr d = writeMem ptr d >> return ptr
  in monadicOptic @m (ExistentialMonadicOptic read_ write_)

instance Pointer GHC.Ptr where
  data Offset GHC.Ptr b = GHCPtrOffset Int
  addOffset p (GHCPtrOffset o) = p `GHC.plusPtr` o
  unsafeCastPointer = GHC.castPtr

instance Storable a => NativeType GHC.Ptr IO a where
  readMem = peek
  writeMem = poke

