module Data.Memory.Abi
  ( AbiLens, Abi(abi)
  , Sized(sizeOf, alignOf, strideOf)
  , Native
  )
where

import           Control.Lens.Monadic (MonadicLens)
import           Data.Function        (($), (.))
import           Data.Memory          (NativeType (MemoryMonad),
                                       Offset (GHCPtrOffset), Pointer)
import           Data.Proxy           (Proxy (Proxy), asProxyTypeOf)
import qualified Foreign.Ptr          as GHC
import qualified Foreign.Storable     (Storable (alignment, sizeOf))
import           GHC.Err              (undefined)

type AbiLens p a = MonadicLens (MemoryMonad p) (p a) (p a) a a

class Abi p a where
  abi :: AbiLens p a

class (Pointer p) => Sized p abi a where
  sizeOf :: Proxy abi -> Offset p a a
  alignOf :: Proxy abi -> Offset p a a
  strideOf :: Proxy abi -> Offset p a a

  -- NOTE: fix this
  default strideOf :: Proxy abi -> Offset p a a
  strideOf = sizeOf

data Native

instance (Foreign.Storable.Storable a) => Sized GHC.Ptr Native a where
  sizeOf _ = GHCPtrOffset . Foreign.Storable.sizeOf $ undefined `asProxyTypeOf` Proxy @a
  alignOf _ = GHCPtrOffset . Foreign.Storable.alignment $ undefined `asProxyTypeOf` Proxy @a
