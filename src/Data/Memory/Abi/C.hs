module Data.Memory.Abi.C
  ( CAbi
  , greedyStructLayout
  , cSized
  )
where

import           Barbies                   (Container (Container, getContainer))
import           Barbies.Constraints       (Dict, requiringDict)
import           Data.Function             (($), (.))
import           Data.Functor              (Functor (fmap))
import           Data.Functor.Barbie       (ConstraintsB (baddDicts),
                                            FunctorB (bmap), TraversableB)
import           Data.Functor.Barbie.Utils (bmapAccumL)
import           Data.Functor.Const        (Const (Const))
import           Data.Functor.Product      (Product (Pair))
import           Data.Memory               (Offset,
                                            Pointer (unsafeOffsetFromBytes))
import           Data.Memory.Abi           (AllSizedB, OffsetB,
                                            SizeOf (SizeOf, alignOf, sizeOf),
                                            SizeOfAbi,
                                            Sized (AlignOf', SizeOf', readM, sized, writeM),
                                            SizedB, minimalStride)
import           Data.Memory.Abi.Native    (Native)
import           Data.Ord                  (Ord (max))
import           Data.Proxy                (Proxy (Proxy))
import           Data.Type.Equality        (type (~))
import           Data.Void                 (Void)
import           Data.Word                 (Word64)
import           GHC.Num                   (Num ((+), (-)))
import           GHC.Real                  (Integral, fromIntegral)

data CAbi

instance (Pointer p) => Sized p CAbi a where
  type SizeOf' CAbi = Word64
  type AlignOf' CAbi = Word64

class CSized a where

combineLayouts
  :: forall p s a b c
  . (Pointer p, Integral s, Ord a, s ~ a)
  => SizeOf s a
  -> SizeOf s a
  -> (SizeOf s a, Offset p b c)
combineLayouts
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
  offset = unsafeOffsetFromBytes (accSize + padding)
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
    , AllSizedB p CAbi b
    , Pointer p
    , Integral (SizeOf' CAbi)
    , Ord (AlignOf' CAbi)
    , Num (AlignOf' CAbi)
    )
  => b (Const Void)
  -> (SizeOfAbi CAbi, OffsetB p b)
greedyStructLayout b
  = let
  s = getContainer . cSized (Proxy @p) $ b
  in bmapAccumL (\a (Const c) -> combineLayouts a c) emptyLayout s

cSized
 :: forall b p
 . (ConstraintsB b, AllSizedB p CAbi b)
 => Proxy p
 -> b (Const Void)
 -> SizedB CAbi b
cSized _ = let
  ss :: forall a. Dict (Sized p CAbi) a -> SizeOfAbi CAbi
  ss = requiringDict (sized (Proxy @(CAbi, p a)))
  in Container . bmap (fmap Const ss) . bmap (\(Pair d _) -> d) . baddDicts
