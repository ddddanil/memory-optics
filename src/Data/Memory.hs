module Data.Memory
  ( Pointer(compareOffset, addOffset, offsetSelf, unsafeCastPointer, unsafeCastOffset)
  , Offset(..)
  , offset
  , NativeType(MemoryMonad, readMemM, writeMemM, sizeOf, alignOf, strideOf)
  , OffsetGetter, AbiLens
  , derefOffset, derefOffset', deref, deref'
  )
where

import           Control.Lens.Getter  (Getter, to)
import           Control.Lens.Monadic (ExistentialMonadicLens (ExistentialMonadicLens),
                                       MonadicLens, MonadicLens', monadicLens)
import           Control.Monad        (Monad (return, (>>)))
import           Data.Function        (($))
import           Data.Kind            (Type)
import           Data.Ord             (Ordering, compare)
import           Data.Proxy           (Proxy)
import           Foreign              (Int, Storable)
import qualified Foreign.Ptr          as GHC
import           Foreign.Storable     (Storable (peek, poke))
import           GHC.IO               (IO)

class Pointer p where
  data Offset p a b
  offsetSelf :: Offset p a a
  addOffset :: p a -> Offset p a b -> p b
  -- mulOffset :: (Num n) => Proxy @(p a) -> n -> Offset p a b -> Offset p a b
  compareOffset ::  Offset p a b -> Offset p a c -> Ordering
  -- | Unsafe
  unsafeCastPointer :: p a -> p b
  -- | Unsafe
  unsafeCastOffset ::  Offset p s a -> Offset p t b

type OffsetGetter p a b = Getter (p a) (p b)

offset :: (Pointer p) => Offset p a b -> OffsetGetter p a b
offset o = to $ \p -> p `addOffset` o

class (Pointer p) => NativeType p a where
  type MemoryMonad p :: Type -> Type
  readMemM :: p a -> MemoryMonad p a
  writeMemM :: p a -> a -> MemoryMonad p ()

  sizeOf :: Proxy (p a) -> Offset p a a
  alignOf :: Proxy (p a) -> Offset p a a
  strideOf :: Proxy (p a) -> Offset p a a

derefOffset :: forall p s t a b. (NativeType p a, NativeType p b, Monad (MemoryMonad p)) => Offset p s a -> MonadicLens (MemoryMonad p) (p s) (p t) a b
derefOffset o = let
  read_ :: p s -> (MemoryMonad p) (a, (p t, Offset p t b))
  read_ ptr = do
    val <- readMemM (ptr `addOffset` o)
    return (val, (unsafeCastPointer ptr, unsafeCastOffset o))
  write_ :: (p t, Offset p t b) -> b -> (MemoryMonad p) (p t)
  write_ (ptr, off) d = writeMemM (ptr `addOffset` off) d >> return ptr
  in monadicLens (ExistentialMonadicLens read_ write_)

derefOffset' :: forall p s a. (NativeType p a, Monad (MemoryMonad p)) => Offset p s a -> MonadicLens' (MemoryMonad p) (p s) a
derefOffset' = derefOffset

deref :: forall p a b. (NativeType p a, NativeType p b, Monad (MemoryMonad p)) => MonadicLens (MemoryMonad p) (p a) (p b) a b
deref = derefOffset offsetSelf

deref' :: forall p a. (NativeType p a, Monad (MemoryMonad p)) => MonadicLens' (MemoryMonad p) (p a) a
deref' = derefOffset' offsetSelf

type AbiLens p a = MonadicLens (MemoryMonad p) (p a) (p a) a a

instance Pointer GHC.Ptr where
  data Offset GHC.Ptr a b = GHCPtrOffset Int
  offsetSelf = GHCPtrOffset 0
  addOffset p (GHCPtrOffset o) = p `GHC.plusPtr` o
  unsafeCastOffset (GHCPtrOffset o) = GHCPtrOffset o
  unsafeCastPointer = GHC.castPtr
  compareOffset (GHCPtrOffset a) (GHCPtrOffset b) = a `compare` b

instance Storable a => NativeType GHC.Ptr a where
  type MemoryMonad GHC.Ptr = IO
  readMemM = peek
  writeMemM = poke
  -- strideIndex i = GHCPtrOffset $ i * sizeOf (undefined @a)
