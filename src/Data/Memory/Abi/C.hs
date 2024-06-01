{-# LANGUAGE UndecidableInstances #-}

module Data.Memory.Abi.C
  ( CAbi
  , greedyStructLayout
  , cSized
  )
where

import           Barbies                   (ApplicativeB,
                                            Container (Container, getContainer))
import           Control.Lens              (_1, _2, (%~), (^.))
import           Control.Monad             (Monad)
import           Data.Function             (($), (&), (.))
import           Data.Functor.Barbie       (ConstraintsB, TraversableB, bmapC)
import           Data.Functor.Barbie.Utils (bmapAccumL, bvoid)
import           Data.Functor.Const        (Const (Const))
import           Data.Functor.Identity     (Identity)
import           Data.Memory               (Offset, OffsetB,
                                            OffsetFor (OffsetFor),
                                            Pointer (MemoryMonad, unsafeOffsetFromBytes))
import           Data.Memory.Abi           (AbiLens, AllSizedB,
                                            SizeOf (SizeOf, alignOf, sizeOf),
                                            SizeOfAbi,
                                            Sized (AlignOf', SizeOf', abi, sized),
                                            SizedB, bbuildAbi, minimalStride)
import           Data.Memory.Abi.Native    (Native)
import           Data.Ord                  (Ord (max))
import           Data.Proxy                (Proxy (Proxy))
import           Data.Type.Equality        (type (~))
import           Data.Void                 (Void)
import           Data.Word                 (Word64)
import           GHC.Num                   (Num ((+), (-)))
import           GHC.Real                  (Integral, fromIntegral)

data CAbi

class CSized p a where
  csized :: Proxy (p a) -> SizeOfAbi CAbi
  cabi :: Proxy (p a) -> AbiLens p a

  default csized
    :: forall b
    . ( Pointer p
      , TraversableB b
      , ApplicativeB b
      , ConstraintsB b
      , AllSizedB p CAbi b
      , a ~ (b Identity)
      )
    => Proxy (p a)
    -> SizeOfAbi CAbi
  csized _ = greedyStructLayout @b (Proxy @p) ^. _1
  default cabi
    :: forall b
    . ( Pointer p
      , TraversableB b
      , ApplicativeB b
      , ConstraintsB b
      , AllSizedB p CAbi b
      , Monad (MemoryMonad p)
      , a ~ (b Identity)
      )
    => Proxy (p a)
    -> AbiLens p a
  cabi _ = let
    off = greedyStructLayout @b (Proxy @p) ^. _2
    in bbuildAbi (Proxy @CAbi) off

instance (Pointer p, CSized p a) => Sized p CAbi a where
   type SizeOf' CAbi = Word64
   type AlignOf' CAbi = Word64
   sized :: Proxy (CAbi, p a) -> SizeOfAbi CAbi
   sized _ = csized (Proxy @(p a))
   abi :: Proxy CAbi -> AbiLens p a
   abi _ = cabi (Proxy @(p a))

instance (Sized p Native a) => CSized p a where
  csized :: Proxy (p a) -> SizeOfAbi CAbi
  csized _ = let
    SizeOf{sizeOf, alignOf} = sized (Proxy @(Native, p a))
    in SizeOf
       { sizeOf = fromIntegral sizeOf
       , alignOf = fromIntegral alignOf
       }
  cabi :: Proxy (p a) -> AbiLens p a
  cabi _ = abi (Proxy @Native)

combineLayouts
  :: forall p s a b c
  . (Pointer p, Integral s, Ord a, s ~ a)
  => Proxy (p b, p c)
  -> SizeOf s a
  -> SizeOf s a
  -> (SizeOf s a, Offset p b c)
combineLayouts
  p
  SizeOf
  { sizeOf = accSize
  , alignOf = accAlign
  }
  SizeOf
  { sizeOf = newSize
  , alignOf = newAlign
  }
  = let
  alignOf = max accAlign newAlign
  padding = minimalStride SizeOf{sizeOf=accSize, alignOf=newAlign} - accSize
  -- NOTE: fix padding
  sizeOf = accSize + padding + newSize
  offset = unsafeOffsetFromBytes p (accSize + padding)
  in (SizeOf{sizeOf, alignOf}, offset)

emptyLayout :: (Num s, Num a) => SizeOf s a
emptyLayout = SizeOf
  { sizeOf = 0
  , alignOf = 0
  }

greedyStructLayout
  :: forall b p
  . ( TraversableB b
    , ConstraintsB b
    , ApplicativeB b
    , AllSizedB p CAbi b
    , Pointer p
    )
  => Proxy p
  -> (SizeOfAbi CAbi, OffsetB p b)
greedyStructLayout p
  = let
  s = getContainer . cSized $ p
  combine :: forall a. SizeOfAbi CAbi -> Const (SizeOfAbi CAbi) a -> (SizeOfAbi CAbi, OffsetFor p (b Identity) a)
  combine a (Const c) = combineLayouts (Proxy @(p (b Identity), p a)) a c & _2 %~ OffsetFor
  in bmapAccumL combine emptyLayout s

cSized
 :: forall b p
 . (ConstraintsB b, ApplicativeB b, AllSizedB p CAbi b)
 => Proxy p
 -> SizedB CAbi b
cSized _ = let
  ss :: forall a. (Sized p CAbi a) => Const Void a -> Const (SizeOfAbi CAbi) a
  ss _ = Const $ sized (Proxy @(CAbi, p a))
  in Container . bmapC @(Sized p CAbi) ss $ bvoid
