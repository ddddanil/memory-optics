{-# LANGUAGE UndecidableInstances #-}
module Data.Memory.Abi.C
  ( CAbi
  , greedyStructLayout
  , greedyStructLayoutB
  , cSized
  )
where
import           Barbies                (Container (Container, getContainer))
import           Barbies.Constraints    (Dict, requiringDict)
import           Data.Function          (($), (.))
import           Data.Functor           (Functor (fmap))
import           Data.Functor.Barbie    (ConstraintsB (baddDicts),
                                         FunctorB (bmap),
                                         TraversableB (btraverse))
import           Data.Functor.Const     (Const (Const))
import           Data.Functor.Product   (Product (Pair))
import           Data.Functor.Utils     (StateL (StateL), runStateL)
import           Data.Memory            (Offset,
                                         Pointer (unsafeOffsetFromBytes))
import           Data.Memory.Abi        (AllSizedB, OffsetB,
                                         SizeOf (SizeOf, alignOf, sizeOf),
                                         SizeOfAbi,
                                         Sized (AlignOf', SizeOf', sized),
                                         SizedB)
import           Data.Memory.Abi.Native (Native)
import           Data.Ord               (Ord (max))
import           Data.Proxy             (Proxy (Proxy))
import           Data.Traversable       (Traversable, mapAccumL)
import           Data.Void              (Void)
import           Data.Word              (Word64)
import           GHC.Num                (Num ((+)))
import           GHC.Real               (Integral, fromIntegral)

data CAbi

instance
  ( Pointer p
  , Sized p Native a
  , Integral (SizeOf' p Native)
  , Integral (AlignOf' p Native)
  )
  => Sized p CAbi a
  where
  type SizeOf' p CAbi = Word64
  type AlignOf' p CAbi = Word64
  sized :: Proxy (CAbi, p a) -> SizeOfAbi p CAbi
  sized _ = let
    SizeOf
      { sizeOf
      , alignOf
      } = sized (Proxy @(Native, p a))
    in SizeOf{ sizeOf = fromIntegral sizeOf, alignOf = fromIntegral alignOf }

combineLayouts
  :: forall p s a b c
  . (Pointer p, Integral s, Ord a)
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
  -- NOTE: fix padding
  sizeOf = accSize + newSize
  offset = unsafeOffsetFromBytes accSize
  in (SizeOf{sizeOf, alignOf}, offset)

emptyLayout :: (Num s, Num a) => SizeOf s a
emptyLayout = SizeOf
  { sizeOf = 0
  , alignOf = 0
  }

greedyStructLayout
  :: forall t p s a
  . (Traversable t, Pointer p, Integral s, Num a, Ord a)
  => t (SizeOf s a)
  -> (SizeOf s a, t (Offset p () ()))
greedyStructLayout
  = mapAccumL combineLayouts emptyLayout

mapAccumLB
  :: forall b s f g
  . (TraversableB b)
  => (forall a. s -> f a -> (s, g a))
  -> s
  -> b f
  -> (s, b g)
mapAccumLB f' s'
  = let
  f :: forall a. f a -> (StateL s) (g a)
  f t = StateL (`f'` t)
  in fmap (`runStateL` s') ((btraverse @_ @_ @(StateL s)) f)

greedyStructLayoutB
  :: forall b p
  . ( TraversableB b
    , ConstraintsB b
    , AllSizedB p CAbi b
    , Pointer p
    )
  => b (Const Void)
  -> (SizeOfAbi p CAbi, OffsetB p b)
greedyStructLayoutB b
  = let
  s = getContainer . cSized (Proxy @p) $ b
  in mapAccumLB (\a (Const c) -> combineLayouts a c) emptyLayout s

cSized
 :: forall b p
 . (ConstraintsB b, AllSizedB p CAbi b)
 => Proxy p
 -> b (Const Void)
 -> SizedB p CAbi b
cSized _ = let
  ss :: forall a. Dict (Sized p CAbi) a -> SizeOfAbi p CAbi
  ss = requiringDict (sized (Proxy @(CAbi, p a)))
  in Container . bmap (fmap Const ss) . bmap (\(Pair d _) -> d) . baddDicts
