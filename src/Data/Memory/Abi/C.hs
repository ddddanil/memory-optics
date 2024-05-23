{-# LANGUAGE UndecidableInstances #-}
module Data.Memory.Abi.C
  ( CAbi
  , HasCAbi(cAbi)
  )
where
import           Control.Lens.Monadic (ExistentialMonadicLens (ExistentialMonadicLens),
                                       monadicLens)
import           Control.Monad        (Monad (return))
import           Data.Function        (($), (.))
import           Data.Memory          (NativeType (MemoryMonad), Offset,
                                       Pointer (compareOffset, composeOffsets, offsetSelf, unsafeCastOffset),
                                       deref', unsafeCastPtr)
import           Data.Memory.Abi      (Abi (abi), AbiLens, Native,
                                       Sized (alignOf, sizeOf))
import           Data.Ord             (Ord (max), Ordering (EQ, GT, LT))
import           Data.Proxy           (Proxy (Proxy))
import           GHC.Err              (error)
import           GHC.Generics         (Generic (Rep), K1, U1 (U1), V1, (:*:))
import           GHC.Generics.Lens    (_K1)

data CAbi

-- instance Abi p CAbi where
--   abi = cAbi

-- | * Sizing

class CSized p a where
  cSizeOf :: Offset p a a
  cAlignOf :: Offset p a a

  default cSizeOf :: (Pointer p, CSized' p (Rep a)) => Offset p a a
  cSizeOf = unsafeCastOffset (cSizeOf' @p @(Rep a))
  default cAlignOf :: (Pointer p, CSized' p (Rep a)) => Offset p a a
  cAlignOf = unsafeCastOffset (cAlignOf' @p @(Rep a))

instance (Sized p Native a) => CSized p a where
  cSizeOf = sizeOf (Proxy @Native)
  cAlignOf = alignOf (Proxy @Native)

instance (Pointer p, CSized p a) => Sized p CAbi a where
  sizeOf _ = cSizeOf
  alignOf _ = cAlignOf

class CSized' p f where
  cSizeOf' :: Offset p (f x) (f x)
  cAlignOf' :: Offset p (f x) (f x)

instance (Pointer p) => CSized' p V1 where
  cSizeOf' = offsetSelf
  cAlignOf' = offsetSelf

instance (Pointer p) => CSized' p U1 where
  cSizeOf' = offsetSelf
  cAlignOf' = offsetSelf

instance (Pointer p, CSized p c) => CSized' p (K1 i c) where
  cSizeOf' = unsafeCastOffset (cSizeOf @p @c)
  cAlignOf' = unsafeCastOffset (cAlignOf @p @c)

instance (Pointer p, CSized' p f, CSized' p g) => CSized' p (f :*: g) where
  cSizeOf' = let
    sizeF :: Offset p ((f :*: g) x) (g x)
    sizeF = unsafeCastOffset $ cSizeOf' @p @f
    sizeG :: Offset p (g x) ((f :*: g) x)
    sizeG = unsafeCastOffset $ cSizeOf' @p @f
    in sizeF `composeOffsets` sizeG
  cAlignOf' = let
    alignF :: Offset p ((f :*: g) x) ((f :*: g) x)
    alignF = unsafeCastOffset $ cAlignOf' @p @f
    alignG :: Offset p ((f :*: g) x) ((f :*: g) x)
    alignG = unsafeCastOffset $ cAlignOf' @p @g
    in case alignF `compareOffset` alignG of
      EQ -> alignF
      LT -> alignG
      GT -> alignF

-- | * Abi

class (Pointer p, Monad (MemoryMonad p)) => HasCAbi p a where
  cAbi :: AbiLens p a

class (Pointer p, Monad (MemoryMonad p)) => HasCAbi' p f where
  cAbi' :: AbiLens p (f x)

instance (Pointer p, Monad (MemoryMonad p)) => HasCAbi' p V1 where
  cAbi' :: AbiLens p (V1 x)
  cAbi' = let
    read_ :: p (V1 x) -> (MemoryMonad p) (V1 x, p (V1 x))
    read_ ptr = return (error "try to get uninhabited type", ptr)
    write_ :: p (V1 x) -> (V1 x) -> (MemoryMonad p) (p (V1 x))
    write_ ptr _ = return ptr
    in monadicLens (ExistentialMonadicLens read_ write_)

instance (Pointer p, Monad (MemoryMonad p)) => HasCAbi' p U1 where
  cAbi' :: AbiLens p (U1 x)
  cAbi' = let
    read_ :: p (U1 x) -> (MemoryMonad p) (U1 x, p (U1 x))
    read_ ptr = return (U1, ptr)
    write_ :: p (U1 x) -> (U1 x) -> (MemoryMonad p) (p (U1 x))
    write_ ptr _ = return ptr
    in monadicLens (ExistentialMonadicLens read_ write_)

-- instance (Pointer p, Monad (MemoryMonad p), CAbi p c) => CAbi' p (K1 i c) where
--   cAbi' :: AbiLens p ((K1 i c) x)
--   cAbi' = unsafeCastPtr . _K1 . deref'

-- instance (Pointer p, Monad (MemoryMonad p), CAbi' p f, CAbi' p g) => CAbi' p (f :*: g) where
--   cAbi' :: AbiLens p ((f :*: g) x)
--   cAbi' = let
--     read_ :: p ((f :*: g) x) -> (MemoryMonad p) ((f :*: g) x, p ((f :*: g) x))
--     read_ ptr = do
--       return (U1, ptr)
--     write_ :: p ((f :*: g) x) -> ((f :*: g) x) -> (MemoryMonad p) (p ((f :*: g) x))
--     write_ ptr _ = return ptr
--     in monadicLens (ExistentialMonadicLens read_ write_)
