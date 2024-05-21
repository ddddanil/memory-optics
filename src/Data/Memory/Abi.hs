module Data.Memory.Abi
  ( AbiLens
  , Sized(sizeOf, alignOf, strideOf)
  , HasDefaultAbi(defaultAbi)
  )
where

import           Control.Lens.Monadic (MonadicLens)
import           Data.Data            (Proxy (Proxy))
import           Data.Function        (($), (.))
import           Data.Memory          (NativeType (MemoryMonad),
                                       Offset (GHCPtrOffset), Pointer)
import           Data.Proxy           (asProxyTypeOf)
import qualified Foreign.Ptr          as GHC
import qualified Foreign.Storable     (Storable (alignment, sizeOf))
import           GHC.Err              (undefined)

class (Pointer p) => Sized p a where
  sizeOf :: Offset p a a
  alignOf :: Offset p a a
  strideOf :: Offset p a a

  -- NOTE: fix this
  default strideOf :: Offset p a a
  strideOf = sizeOf

type AbiLens p a = MonadicLens (MemoryMonad p) (p a) (p a) a a

class HasDefaultAbi p a where
  defaultAbi :: AbiLens p a

instance Foreign.Storable.Storable a => Sized GHC.Ptr a where
  sizeOf = GHCPtrOffset . Foreign.Storable.sizeOf $ undefined `asProxyTypeOf` Proxy @a
  alignOf = GHCPtrOffset . Foreign.Storable.alignment $ undefined `asProxyTypeOf` Proxy @a
