module Data.Memory.Abi
  ( AbiLens
  , Abi()
  , SizeOf(SizeOf, sizeOf, alignOf)
  , SizeOfAbi
  , minimalStride
  , Sized(SizeOf', AlignOf', sized)
  , OffsetB
  , AllSizedB
  , SizedB
  )
where

import           Barbies               (Container)
import           Control.Lens.Monadic  (MonadicLens)
import           Data.Functor.Barbie   (ConstraintsB (AllB))
import           Data.Functor.Identity (Identity)
import           Data.Memory           (NativeType (MemoryMonad), Offset,
                                        Pointer)
import           Data.Proxy            (Proxy)
import           Data.Type.Equality    (type (~))
import           GHC.Num               (Num ((+), (-)))
import           GHC.Real              (Integral (mod))

type AbiLens p a = MonadicLens (MemoryMonad p) (p a) (p a) a a

class Abi p abi where

data SizeOf s a = SizeOf
  { sizeOf  :: !s
  , alignOf :: !a
  }

class (Pointer p) => Sized p abi a where
  type SizeOf' p abi
  type AlignOf' p abi
  sized :: Proxy (abi, p a) -> SizeOf (SizeOf' p abi) (AlignOf' p abi)

type SizeOfAbi p abi = SizeOf (SizeOf' p abi) (AlignOf' p abi)

minimalStride :: (Integral s, s ~ a) => SizeOf s a -> s
minimalStride SizeOf{sizeOf, alignOf} =
  case sizeOf `mod` alignOf of
    0 -> sizeOf
    x -> sizeOf - x + alignOf

type OffsetB p b = b (Offset p (b Identity))

type AllSizedB p abi b = AllB (Sized p abi) b
type SizedB p abi b = Container b (SizeOfAbi p abi)

