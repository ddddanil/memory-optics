module Data.Memory.Abi
  ( AbiLens
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

import           Barbies               (Container)
import           Barbies.Constraints   (Dict, requiringDict)
import           Control.Lens.Monadic  (ExistentialMonadicLens (ExistentialMonadicLens),
                                        MLensFor (MLensFor), MonadicLens,
                                        MonadicLens', getM, hoistM, monadicLens)
import           Control.Monad         (Monad (return, (>>), (>>=)), fmap)
import           Data.Function         (($), (.))
import           Data.Functor.Barbie   (ConstraintsB (AllB, baddDicts),
                                        FunctorB (bmap), TraversableB,
                                        bsequence)
import           Data.Functor.Compose  (Compose (Compose))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Functor.Product  (Product (Pair))
import           Data.Memory           (MemoryMonad, Offset,
                                        Pointer (addOffset, offsetSelf, unsafeCastOffset, unsafeCastPointer))
import           Data.Proxy            (Proxy (Proxy))
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
  readM :: Proxy abi -> p a -> MemoryMonad p a
  writeM :: Proxy abi -> p a -> a -> MemoryMonad p ()

type SizeOfAbi p abi = SizeOf (SizeOf' p abi) (AlignOf' p abi)

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
type SizedB p abi b = Container b (SizeOfAbi p abi)

getBM
  :: forall p abi b
  . (TraversableB b, ConstraintsB b, AllSizedB p abi b, Monad (MemoryMonad p))
  => Proxy abi
  -> OffsetB p b
  -> p (b Identity)
  -> (MemoryMonad p) (b Identity)
getBM p off ptr = let
  offsetToLens :: forall a. Dict (Sized p abi) a -> Offset p (b Identity) a -> MLensFor (MemoryMonad p) (p (b Identity)) a
  offsetToLens d o = (`requiringDict` d) $ MLensFor $ derefOffset' o (Proxy @abi)
  composeM :: MemoryMonad p a -> Compose (MemoryMonad p) Identity a
  composeM = Compose . fmap Identity
  readOffset :: MLensFor (MemoryMonad p) (p (b Identity)) a -> Compose (MemoryMonad p) Identity a
  readOffset (MLensFor l) = getM (hoistM composeM l) ptr
  in bsequence . bmap readOffset . bmap (\(Pair d o) -> offsetToLens d o) . baddDicts $ off

instance (Monad m) => Monad (Compose m Identity) where
  (>>=) a f = Compose . fmap Identity $ do
    let Compose a' = a
    Identity a'' <- a'
    let Compose b' = f a''
    Identity b'' <- b'
    return b''
