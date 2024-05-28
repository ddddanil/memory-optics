module Data.Memory.Abi
  ( AbiLens
  , abi
  , Abi()
  , SizeOf(SizeOf, sizeOf, alignOf)
  , SizeOfAbi
  , minimalStride
  , Sized(SizeOf', AlignOf', sized, readM, writeM)
  , derefOffset, derefOffset', deref, deref'
  , OffsetB
  , AllSizedB
  , SizedB
  )
where

import           Barbies               (ApplicativeB, Container)
import           Control.Lens.Monadic  (ExistentialMonadicLens (ExistentialMonadicLens),
                                        MLensFor (MLensFor), MonadicLens,
                                        MonadicLens', getM, hoistM, monadicLens,
                                        putM)
import           Control.Monad         (Monad (return, (>>), (>>=)), fmap)
import           Data.Eq               (Eq)
import           Data.Function         (($), (.))
import           Data.Functor.Barbie   (ConstraintsB (AllB), FunctorB (bmap),
                                        TraversableB, bmapC, bsequence,
                                        btraverse_, bzip)
import           Data.Functor.Compose  (Compose (Compose))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Functor.Product  (Product (Pair))
import           Data.Memory           (MemoryMonad, Offset,
                                        Pointer (addOffset, offsetSelf, unsafeCastOffset, unsafeCastPointer))
import           Data.Proxy            (Proxy)
import           Data.Type.Equality    (type (~))
import           GHC.Generics          (Generic)
import           GHC.Num               (Num ((+), (-)))
import           GHC.Real              (Integral (mod))
import           GHC.Show              (Show)

type AbiLens p a = MonadicLens (MemoryMonad p) (p a) (p a) a a

class Abi p abi where

data SizeOf s a = SizeOf
  { sizeOf  :: !s
  , alignOf :: !a
  }
  deriving (Generic)

deriving instance (Show s, Show a) => Show (SizeOf s a)
deriving instance (Eq s, Eq a) => Eq (SizeOf s a)

class (Pointer p) => Sized p abi a where
  type SizeOf' abi
  type AlignOf' abi
  sized :: Proxy (abi, p a) -> SizeOf (SizeOf' abi) (AlignOf' abi)
  readM :: Proxy abi -> p a -> MemoryMonad p a
  writeM :: Proxy abi -> p a -> a -> MemoryMonad p ()

type SizeOfAbi abi = SizeOf (SizeOf' abi) (AlignOf' abi)

derefOffset :: forall p abi s t a b. (Sized p abi a, Sized p abi b, Monad (MemoryMonad p)) => Offset p s a -> Proxy abi -> MonadicLens (MemoryMonad p) (p s) (p t) a b
derefOffset o p = let
  read_ :: p s -> (MemoryMonad p) (a, (p t, Offset p t b))
  read_ ptr = do
    val <- readM p (ptr `addOffset` o)
    return (val, (unsafeCastPointer ptr, unsafeCastOffset o))
  write_ :: (p t, Offset p t b) -> b -> (MemoryMonad p) (p t)
  write_ (ptr, off) d = writeM p (ptr `addOffset` off) d >> return ptr
  in monadicLens (ExistentialMonadicLens read_ write_)

derefOffset' :: forall p abi s a. (Sized p abi a, Monad (MemoryMonad p)) => Offset p s a -> Proxy abi -> MonadicLens' (MemoryMonad p) (p s) a
derefOffset' = derefOffset

deref :: forall p abi a b. (Sized p abi a, Sized p abi b, Monad (MemoryMonad p)) => Proxy abi -> MonadicLens (MemoryMonad p) (p a) (p b) a b
deref = derefOffset offsetSelf

deref' :: forall p abi a. (Sized p abi a, Monad (MemoryMonad p)) => Proxy abi -> MonadicLens' (MemoryMonad p) (p a) a
deref' = derefOffset' offsetSelf

minimalStride :: (Integral s, s ~ a) => SizeOf s a -> s
minimalStride SizeOf{sizeOf, alignOf} =
  case sizeOf `mod` alignOf of
    0 -> sizeOf
    x -> sizeOf - x + alignOf

type OffsetB p b = b (Offset p (b Identity))

type AllSizedB p abi b = AllB (Sized p abi) b
type SizedB abi b = Container b (SizeOfAbi abi)

readBM
  :: forall p abi b
  . (TraversableB b, ConstraintsB b, AllSizedB p abi b, Monad (MemoryMonad p))
  => Proxy abi
  -> OffsetB p b
  -> p (b Identity)
  -> (MemoryMonad p) (b Identity, p (b Identity))
readBM p off ptr = let
  offsetToLens :: forall a. (Sized p abi a) => Offset p (b Identity) a -> MLensFor (MemoryMonad p) (p (b Identity)) a
  offsetToLens o = MLensFor $ derefOffset' o p
  composeM :: MemoryMonad p a -> Compose (MemoryMonad p) Identity a
  composeM = Compose . fmap Identity
  readOffset :: MLensFor (MemoryMonad p) (p (b Identity)) a -> Compose (MemoryMonad p) Identity a
  readOffset (MLensFor l) = getM (hoistM composeM l) ptr
  in do
    b <- bsequence . bmap readOffset . bmapC @(Sized p abi) offsetToLens $ off
    return (b, ptr)

writeBM
  :: forall p abi b
  . (TraversableB b, ApplicativeB b, ConstraintsB b, AllSizedB p abi b, Monad (MemoryMonad p))
  => Proxy abi
  -> OffsetB p b
  -> p (b Identity)
  -> b Identity
  -> (MemoryMonad p) (p (b Identity))
writeBM p off ptr d = let
  offsetToLens :: forall a. (Sized p abi a) => Offset p (b Identity) a -> MLensFor (MemoryMonad p) (p (b Identity)) a
  offsetToLens o = MLensFor $ derefOffset' o p
  writeOffset :: (Identity `Product` MLensFor (MemoryMonad p) (p (b Identity))) a -> MemoryMonad p ()
  writeOffset (Pair (Identity d') (MLensFor l) ) = putM l d' ptr >> return ()
  in do
    btraverse_ writeOffset . bzip d . bmapC @(Sized p abi) offsetToLens $ off
    return ptr

abi
  :: forall p abi b
  . (TraversableB b, ApplicativeB b, ConstraintsB b, AllSizedB p abi b, Monad (MemoryMonad p))
  => Proxy abi
  -> OffsetB p b
  -> AbiLens p (b Identity)
abi p off = let
  read_ :: p (b Identity) -> (MemoryMonad p) (b Identity, p (b Identity))
  read_ = readBM p off
  write_ :: p (b Identity) -> (b Identity) -> (MemoryMonad p) (p (b Identity))
  write_ = writeBM p off
  in monadicLens (ExistentialMonadicLens read_ write_)

instance (Monad m) => Monad (Compose m Identity) where
  (>>=) a f = Compose . fmap Identity $ do
    let Compose a' = a
    Identity a'' <- a'
    let Compose b' = f a''
    Identity b'' <- b'
    return b''
