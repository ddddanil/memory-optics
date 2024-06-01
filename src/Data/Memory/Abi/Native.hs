module Data.Memory.Abi.Native
  ( Native )
where

import           Control.Lens.Monadic (ExistentialMonadicLens (ExistentialMonadicLens),
                                       monadicLens)
import           Control.Monad        (Monad (return, (>>)))
import           Data.Int             (Int)
import           Data.Memory.Abi      (AbiLens,
                                       SizeOf (SizeOf, alignOf, sizeOf),
                                       Sized (AlignOf', SizeOf', abi, sized))
import           Data.Proxy           (Proxy (Proxy), asProxyTypeOf)
import           Foreign.Ptr          (Ptr)
import qualified Foreign.Storable     (Storable (alignment, peek, poke, sizeOf))
import           GHC.Err              (undefined)
import           System.IO            (IO)

data Native

instance (Foreign.Storable.Storable a) => Sized Ptr Native a where
  type SizeOf' Native = Int
  type AlignOf' Native = Int
  sized :: Proxy (Native, Ptr a) -> SizeOf Int Int
  sized _ = let
    p = undefined `asProxyTypeOf` Proxy @a
    sizeOf = Foreign.Storable.sizeOf p
    alignOf = Foreign.Storable.alignment p
    in SizeOf{sizeOf, alignOf}

  abi :: Proxy Native -> AbiLens Ptr a
  abi _ = let
    read_ :: Ptr a -> IO (a, Ptr a)
    read_ ptr = do
      val <- Foreign.Storable.peek ptr
      return (val, ptr)
    write_ :: Ptr a -> a -> IO (Ptr a)
    write_ ptr d = Foreign.Storable.poke ptr d >> return ptr
    in monadicLens (ExistentialMonadicLens read_ write_)
  -- readM _ = Foreign.Storable.peek
  -- writeM _ = Foreign.Storable.poke
