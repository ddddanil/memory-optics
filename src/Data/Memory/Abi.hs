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
  , Native
  )
where

import           Barbies               (Container)
import           Control.Lens.Monadic  (MonadicLens)
import           Data.Function         (($), (.))
import           Data.Functor.Barbie   (ConstraintsB (AllB))
import           Data.Functor.Identity (Identity)
import           Data.Int              (Int)
import           Data.Memory           (NativeType (MemoryMonad), Offset,
                                        Pointer)
import           Data.Proxy            (Proxy (Proxy), asProxyTypeOf)
import           Data.Type.Equality    (type (~))
import qualified Foreign.Ptr           as GHC
import qualified Foreign.Storable      (Storable (alignment, sizeOf))
import           GHC.Err               (undefined)
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

data Native

instance (Foreign.Storable.Storable a) => Sized GHC.Ptr Native a where
  type SizeOf' GHC.Ptr Native = Int
  type AlignOf' GHC.Ptr Native = Int
  sized :: Proxy (Native, GHC.Ptr a) -> SizeOf Int Int
  sized _ = let
    p = undefined `asProxyTypeOf` Proxy @a
    sizeOf = Foreign.Storable.sizeOf $ p
    alignOf = Foreign.Storable.alignment $ p
    in SizeOf{sizeOf, alignOf}
