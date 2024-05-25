module Data.Memory
  ( Pointer
    ( offsetSelf
    , unsafeOffsetFromBytes
    , compareOffset
    , addOffset
    , composeOffsets
    , unsafeCastPointer
    , unsafeCastOffset
    )
  , Offset(..)
  , offset
  , unsafeCastPtr
  , NativeType(MemoryMonad, readMemM, writeMemM)
  , OffsetGetter
  , derefOffset, derefOffset', deref, deref'
  )
where

import           Control.Lens.Getter  (Getter, to)
import           Control.Lens.Iso     (Iso, iso)
import           Control.Lens.Monadic (ExistentialMonadicLens (ExistentialMonadicLens),
                                       MonadicLens, MonadicLens', monadicLens)
import           Control.Monad        (Monad (return, (>>)))
import           Data.Function        (($), (.))
import           Data.Kind            (Type)
import           Data.Ord             (Ordering, compare)
import           Foreign              (Int, Storable)
import qualified Foreign.Ptr          as GHC
import qualified Foreign.Storable     (Storable (peek, poke))
import           GHC.IO               (IO)
import           GHC.Num              ((+))
import           GHC.Real             (Integral, fromIntegral)

class Pointer p where
  data Offset p a b
  offsetSelf :: Offset p a a
  unsafeOffsetFromBytes :: forall a b c. (Integral a) => a -> Offset p b c
  addOffset :: p a -> Offset p a b -> p b
  composeOffsets :: Offset p a b -> Offset p b c -> Offset p a c
  -- mulOffset :: (Num n) => Proxy @(p a) -> n -> Offset p a b -> Offset p a b
  compareOffset ::  Offset p a b -> Offset p a c -> Ordering
  -- | Unsafe
  unsafeCastPointer :: p a -> p b
  -- | Unsafe
  unsafeCastOffset ::  Offset p s a -> Offset p t b

type OffsetGetter p a b = Getter (p a) (p b)

offset :: (Pointer p) => Offset p a b -> OffsetGetter p a b
offset o = to $ \p -> p `addOffset` o

unsafeCastPtr :: (Pointer p) => Iso (p a) (p a) (p b) (p b)
unsafeCastPtr = iso unsafeCastPointer unsafeCastPointer

class (Pointer p) => NativeType p a where
  type MemoryMonad p :: Type -> Type
  readMemM :: p a -> MemoryMonad p a
  writeMemM :: p a -> a -> MemoryMonad p ()

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

instance Pointer GHC.Ptr where
  data Offset GHC.Ptr a b = GHCPtrOffset Int
  offsetSelf = GHCPtrOffset 0
  unsafeOffsetFromBytes = GHCPtrOffset . fromIntegral
  p `addOffset ` (GHCPtrOffset o) = p `GHC.plusPtr` o
  (GHCPtrOffset a) `composeOffsets` (GHCPtrOffset b) = GHCPtrOffset $ a + b
  (GHCPtrOffset a) `compareOffset` (GHCPtrOffset b) = a `compare` b
  unsafeCastOffset (GHCPtrOffset o) = GHCPtrOffset o
  unsafeCastPointer = GHC.castPtr

instance Storable a => NativeType GHC.Ptr a where
  type MemoryMonad GHC.Ptr = IO
  readMemM = Foreign.Storable.peek
  writeMemM = Foreign.Storable.poke
