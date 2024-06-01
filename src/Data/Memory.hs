{-# LANGUAGE FieldSelectors       #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , Offset
  , MemoryMonad
  , offset'
  , offset
  , unsafeCastPtr
  , OffsetFor(OffsetFor, getOffsetFor)
  , OffsetB
  , OffsetGetter
  )
where

import           Control.Lens.Getter   (Getter, to)
import           Control.Lens.Iso      (Iso, iso)
import           Control.Lens.Lens     (Lens', lens)
import           Data.Eq               (Eq)
import           Data.Function         (const, ($))
import           Data.Functor.Identity (Identity)
import           Data.Kind             (Type)
import           Data.Ord              (Ordering, compare)
import           Data.Proxy            (Proxy)
import           Foreign               (Int)
import qualified Foreign.Ptr           as GHC
import           GHC.IO                (IO)
import           GHC.Num               ((+))
import           GHC.Real              (Integral, fromIntegral)
import           GHC.Show              (Show)

class Pointer p where
  type Offset p a b
  type MemoryMonad p :: Type -> Type
  offsetSelf :: Proxy (p a) -> Offset p a a
  unsafeOffsetFromBytes :: forall a b c. (Integral a) => Proxy (p b, p c) -> a -> Offset p b c
  addOffset :: p a -> Offset p a b -> p b
  composeOffsets :: Proxy (p a, p b, p c) -> Offset p a b -> Offset p b c -> Offset p a c
  -- mulOffset :: (Num n) => Proxy @(p a) -> n -> Offset p a b -> Offset p a b
  compareOffset :: Proxy (p a, p b, p c) -> Offset p a b -> Offset p a c -> Ordering
  -- | Unsafe
  unsafeCastPointer :: p a -> p b
  -- | Unsafe
  unsafeCastOffset :: Proxy (p s, p a, p t, p b) -> Offset p s a -> Offset p t b

type OffsetGetter p a b = Getter (p a) (p b)
type OffsetLens p a b = Lens' (p a) (p b)

offset' :: (Pointer p) => Offset p a b -> OffsetGetter p a b
offset' o = to $ \p -> p `addOffset` o

offset :: (Pointer p) => Offset p a b -> OffsetLens p a b
offset o = let
  to_ = (`addOffset` o)
  from_ = const
  in lens to_ from_

unsafeCastPtr :: (Pointer p) => Iso (p a) (p a) (p b) (p b)
unsafeCastPtr = iso unsafeCastPointer unsafeCastPointer

newtype OffsetFor p a b
  = OffsetFor
  { getOffsetFor :: Offset p a b
  }

deriving instance Show (Offset p a b) => Show (OffsetFor p a b)
deriving instance Eq   (Offset p a b) => Eq   (OffsetFor p a b)

type OffsetB p b = b (OffsetFor p (b Identity))

instance Pointer GHC.Ptr where
  type Offset GHC.Ptr a b = Int
  type MemoryMonad GHC.Ptr = IO
  offsetSelf _ = 0
  unsafeOffsetFromBytes _ = fromIntegral
  p `addOffset ` o = p `GHC.plusPtr` o
  composeOffsets _ a b = a + b
  compareOffset _ a b = a `compare` b
  unsafeCastOffset _ o = o
  unsafeCastPointer = GHC.castPtr

