module Data.Memory.Scalars.Lens
  (
    readingUVia
  )
where

import           Control.Lens.Monadic   (ExistentialMonadicLens (ExistentialMonadicLens),
                                         MonadicLens', MonadicTraversal', getM,
                                         modifyM, monadicLens, monadicTraversal,
                                         putM)
import           Control.Monad          (Monad (return), forM_)
import           Data.Bits              (Bits (shiftL, shiftR), (.&.), (.|.))
import           Data.Function          (($), (&), (.))
import           Data.Memory            (Allocation (aLength, aStart),
                                         Pointer (MemoryMonad, addOffset, unsafeOffsetFromBytes),
                                         SizeOf, bitsInByte)
import           Data.Memory.Abi        (AbiLens, Sized (abi))
import           Data.Memory.Abi.Native (Native)
import           Data.Memory.Scalars    (NextNativeScalar,
                                         ScalarConvertible (convertDown, convertUp),
                                         ScalarType (BitSize), U)
import           Data.Proxy             (Proxy (Proxy), asProxyTypeOf)
import           GHC.Err                (undefined)
import           GHC.Num                (Num (fromInteger), (*), (+), (-))
import           GHC.Real               (Integral (quotRem), fromIntegral)
import           TypeLevel.Number.Nat   (Nat (toInt))

unalignedReadU
  :: forall i n p a
  . ( Monad (MemoryMonad p)
    , Allocation p a
    , Integral i
    , Nat (BitSize (U n))
    , Nat (BitSize (NextNativeScalar (U n)))
    , Sized p Native (NextNativeScalar (U n))
    , Bits (NextNativeScalar (U n))
    , Integral (NextNativeScalar (U n))
    , ScalarConvertible (U n)
    )
  => Proxy (p (), U n)
  -> i
  -> MonadicLens' (MemoryMonad p) a (NextNativeScalar (U n))
unalignedReadU _ bitoff_ = let
  nextNativeScalarBits = toInt (undefined `asProxyTypeOf` Proxy @(BitSize (NextNativeScalar (U n))))
  bitoff :: i
  (byteoff, bitoff) = bitoff `quotRem` (toInt bitsInByte * nextNativeScalarBits)
  natSc :: AbiLens p (NextNativeScalar (U n))
  natSc = abi @p @_ @(NextNativeScalar (U n)) (Proxy @Native)
  alignFirstHalf :: NextNativeScalar (U n) -> NextNativeScalar (U n)
  alignFirstHalf = let
    sh = fromIntegral bitoff
    in (.&. ((\a -> a - 1) . (`shiftL` sh) $ 1)) . (`shiftR` sh)
  unalignFirstHalf :: NextNativeScalar (U n) -> NextNativeScalar (U n) -> NextNativeScalar (U n)
  unalignFirstHalf base d' = let
    sh = fromIntegral bitoff
    d = (`shiftL` sh) . (.&. ((\a -> a - 1) . (`shiftL` sh) $ 1)) $ d'
    in (.|. d) . (.&. ((\a -> a - 1) . (`shiftL` (fromIntegral nextNativeScalarBits - sh)) $ 1)) $ base
  alignSecondHalf :: NextNativeScalar (U n) -> NextNativeScalar (U n)
  alignSecondHalf = let
    sh = fromIntegral (bitoff + toInt (undefined `asProxyTypeOf` Proxy @(BitSize (U n))) - nextNativeScalarBits)
    in (`shiftL` sh) . (.&. ((\a -> a - 1) . (`shiftL` sh) $ 1))
  unalignSecondHalf :: NextNativeScalar (U n) -> NextNativeScalar (U n)
  unalignSecondHalf = let
    sh = fromIntegral (bitoff + toInt (undefined `asProxyTypeOf` Proxy @(BitSize (U n))) - nextNativeScalarBits)
    in (.&. ((\a -> a - 1) . (`shiftL` sh) $ 1)) . (`shiftR` sh)
  read_ :: a -> (MemoryMonad p) (NextNativeScalar (U n), (a, NextNativeScalar (U n)))
  read_ alloc = let
    ptr :: p (NextNativeScalar (U n))
    ptr = aStart @p alloc `addOffset` unsafeOffsetFromBytes (Proxy @(p (), p (NextNativeScalar (U n)))) byteoff
    in do
    firstHalf <- ptr & getM natSc
    return (convertUp (Proxy @(U n)) . alignFirstHalf $ firstHalf, (alloc, firstHalf))
  write_ :: (a, NextNativeScalar (U n)) -> (NextNativeScalar (U n)) -> (MemoryMonad p) (a)
  write_ (alloc, base) (convertDown (Proxy @(U n)) -> d') = let
    ptr :: p (NextNativeScalar (U n))
    ptr = aStart @p alloc `addOffset` unsafeOffsetFromBytes (Proxy @(p (), p (NextNativeScalar (U n)))) byteoff
    d = unalignFirstHalf base d'
    in do
    _ <- ptr & natSc `putM` d
    return alloc
  in monadicLens (ExistentialMonadicLens read_ write_)

readingUVia
  :: forall n a p
  . ( Allocation p a
    , Monad (MemoryMonad p)
    , Integral (SizeOf p a)
    , Nat (BitSize (U n))
    , Nat (BitSize (NextNativeScalar (U n)))
    , Sized p Native (NextNativeScalar (U n))
    , Bits (NextNativeScalar (U n))
    , Integral (NextNativeScalar (U n))
    , ScalarConvertible (U n)
    )
  => Proxy (p (), U n)
  -> MonadicTraversal' (MemoryMonad p) a (NextNativeScalar (U n))
readingUVia p = let
  go_ :: ((NextNativeScalar (U n)) -> (MemoryMonad p) (NextNativeScalar (U n))) -> a -> (MemoryMonad p) a
  go_ focus alloc = let
    allocSize = aLength (Proxy @(p ())) alloc
    scalarBits = toInt @n @(SizeOf p a) (undefined `asProxyTypeOf` Proxy @(BitSize (U n)))
    bitOffs = [0, scalarBits..(allocSize * toInt bitsInByte)]
    in do
    forM_ bitOffs \bitOff -> do
      alloc & unalignedReadU p bitOff `modifyM` focus
    return alloc
  in monadicTraversal @(MemoryMonad p) go_

