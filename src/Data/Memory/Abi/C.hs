module Data.Memory.Abi.C
  ( CAbi
  , greedyStructLayout
  )
where
import           Control.Lens.Monadic (ExistentialMonadicLens (ExistentialMonadicLens),
                                       monadicLens)
import           Control.Monad        (Monad (return))
import           Data.Function        (($), (.))
import           Data.Memory          (NativeType (MemoryMonad), Offset,
                                       Pointer (compareOffset, composeOffsets, offsetSelf, unsafeCastOffset, unsafeOffsetFromBytes),
                                       deref', unsafeCastPtr)
import           Data.Memory.Abi      (Abi, AbiLens, Native,
                                       SizeOf (SizeOf, alignOf, sizeOf),
                                       Sized (sized))
import           Data.Ord             (Ord (max))
import           Data.Proxy           (Proxy (Proxy))
import           Data.Traversable     (Traversable, mapAccumL)
import           GHC.Err              (error)
import           GHC.Generics         (Generic (Rep), K1, U1 (U1), V1, (:*:))
import           GHC.Generics.Lens    (_K1)
import           GHC.Num              (Num ((+)))
import           GHC.Real             (Integral, fromIntegral)

data CAbi

combineLayouts
  :: forall p s a
  . (Pointer p, Integral s, Ord a)
  => SizeOf s a
  -> SizeOf s a
  -> (SizeOf s a, Offset p () ())
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

