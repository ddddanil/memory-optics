module Data.Memory.Abi.Native
  ( Native )
where

import           Data.Int         (Int)
import           Data.Memory.Abi  (SizeOf (SizeOf, alignOf, sizeOf),
                                   Sized (AlignOf', SizeOf', readM, sized, writeM))
import           Data.Proxy       (Proxy (Proxy), asProxyTypeOf)
import           Foreign.Ptr      (Ptr)
import qualified Foreign.Storable (Storable (alignment, peek, poke, sizeOf))
import           GHC.Err          (undefined)

data Native

instance (Foreign.Storable.Storable a) => Sized Ptr Native a where
  type SizeOf' Ptr Native = Int
  type AlignOf' Ptr Native = Int
  sized :: Proxy (Native, Ptr a) -> SizeOf Int Int
  sized _ = let
    p = undefined `asProxyTypeOf` Proxy @a
    sizeOf = Foreign.Storable.sizeOf p
    alignOf = Foreign.Storable.alignment p
    in SizeOf{sizeOf, alignOf}

  readM _ = Foreign.Storable.peek
  writeM _ = Foreign.Storable.poke
