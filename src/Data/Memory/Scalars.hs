module Data.Memory.Scalars where

import           GHC.Int              (Int16, Int32, Int64, Int8)
import           GHC.Word             (Word16, Word32, Word64, Word8)
import           TypeLevel.Number.Nat (Nat, natT)

data ByteOrder = Msb | Lsb
type Native = 'Msb
type Network = 'Lsb

data U n (bo :: ByteOrder) where
  U :: Nat n => U n bo

data I n (bo :: ByteOrder) where
  I :: Nat n => I n bo

data F n (bo :: ByteOrder) where
  F :: Nat n => F n bo

type family NativeScalar s where
  NativeScalar (U $(natT 8) 'Msb) = Word8
  NativeScalar (U $(natT 16) 'Msb) = Word16
  NativeScalar (U $(natT 32) 'Msb) = Word32
  NativeScalar (U $(natT 64) 'Msb) = Word64

  NativeScalar (I $(natT 8) 'Msb) = Int8
  NativeScalar (I $(natT 16) 'Msb) = Int16
  NativeScalar (I $(natT 32) 'Msb) = Int32
  NativeScalar (I $(natT 64) 'Msb) = Int64

  NativeScalar (F $(natT 32) 'Msb) = Float
  NativeScalar (F $(natT 64) 'Msb) = Double

