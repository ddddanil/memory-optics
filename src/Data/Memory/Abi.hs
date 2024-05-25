module Data.Memory.Abi
  ( AbiLens
  , Abi()
  , SizeOf(SizeOf, sizeOf, alignOf)
  , minimalStride
  , Sized(sized)
  , Native
  )
where

import           Control.Lens.Monadic (MonadicLens)
import           Data.Function        (($), (.))
import           Data.Memory          (NativeType (MemoryMonad), Pointer)
import           Data.Proxy           (Proxy (Proxy), asProxyTypeOf)
import           Data.Type.Equality   (type (~))
import           Data.Word            (Word64)
import qualified Foreign.Ptr          as GHC
import qualified Foreign.Storable     (Storable (alignment, sizeOf))
import           GHC.Err              (undefined)
import           GHC.Num              (Num ((+), (-)))
import           GHC.Real             (Integral (mod), fromIntegral)

type AbiLens p a = MonadicLens (MemoryMonad p) (p a) (p a) a a

class Abi p abi where

data SizeOf s a = SizeOf
  { sizeOf  :: !s
  , alignOf :: !a
  }

class (Pointer p) => Sized p abi a where
  sized :: Proxy (abi, p a) -> SizeOf Word64 Word64

minimalStride :: (Integral s, s ~ a) => SizeOf s a -> s
minimalStride SizeOf{sizeOf, alignOf} =
  case sizeOf `mod` alignOf of
    0 -> sizeOf
    x -> sizeOf - x + alignOf

data Native

instance (Foreign.Storable.Storable a) => Sized GHC.Ptr Native a where
  sized :: Proxy (Native, GHC.Ptr a) -> SizeOf Word64 Word64
  sized _ = let
    p = undefined `asProxyTypeOf` Proxy @a
    sizeOf = fromIntegral . Foreign.Storable.sizeOf $ p
    alignOf = fromIntegral . Foreign.Storable.alignment $ p
    in SizeOf{sizeOf, alignOf}
