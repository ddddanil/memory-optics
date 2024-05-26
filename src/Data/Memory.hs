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
  , MemoryMonad
  , offset
  , unsafeCastPtr
  , OffsetGetter
  )
where

import           Control.Lens.Getter (Getter, to)
import           Control.Lens.Iso    (Iso, iso)
import           Data.Function       (($), (.))
import           Data.Kind           (Type)
import           Data.Ord            (Ordering, compare)
import           Foreign             (Int)
import qualified Foreign.Ptr         as GHC
import           GHC.IO              (IO)
import           GHC.Num             ((+))
import           GHC.Real            (Integral, fromIntegral)

class Pointer p where
  data Offset p a b
  type MemoryMonad p :: Type -> Type
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

instance Pointer GHC.Ptr where
  data Offset GHC.Ptr a b = GHCPtrOffset Int
  type MemoryMonad GHC.Ptr = IO
  offsetSelf = GHCPtrOffset 0
  unsafeOffsetFromBytes = GHCPtrOffset . fromIntegral
  p `addOffset ` (GHCPtrOffset o) = p `GHC.plusPtr` o
  (GHCPtrOffset a) `composeOffsets` (GHCPtrOffset b) = GHCPtrOffset $ a + b
  (GHCPtrOffset a) `compareOffset` (GHCPtrOffset b) = a `compare` b
  unsafeCastOffset (GHCPtrOffset o) = GHCPtrOffset o
  unsafeCastPointer = GHC.castPtr

