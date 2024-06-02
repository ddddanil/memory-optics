module Data.Memory.Scalars
  ( ByteOrder(Msb, Lsb)
  , NativeBO, NetworkBO
  , IsBitSize
  , ScalarType
  , BitSize
  , U(U), I(I), F(F)
  , ScalarConvertible(convertUp, convertDown)
  , NativeScalar
  , NextNative
  , NextNativeScalar
  )
where

import           Data.Eq              (Eq)
import           Data.Int             (Int16, Int32, Int64, Int8)
import           Data.Proxy           (Proxy)
import           Data.Word            (Word16, Word32, Word64, Word8)
import           GHC.Enum             (Enum)
import           GHC.Float            (Double, Float)
import           GHC.Generics         (Generic)
import           Text.Show            (Show)
import           TypeLevel.Number.Nat (Nat, natT)

data ByteOrder = Msb | Lsb
  deriving (Show, Eq, Enum, Generic)

type NativeBO = 'Msb
type NetworkBO = 'Lsb

type IsBitSize = Nat

class ScalarType s where
  type BitSize s :: n

data U n where
  U :: IsBitSize n => U n

deriving instance (IsBitSize n) => Show (U n)
instance ScalarType (U n) where
  type BitSize (U n) = n

data I n where
  I :: IsBitSize n => I n

deriving instance (IsBitSize n) => Show (I n)
instance ScalarType (I n) where
  type BitSize (I n) = n

data F n where
  F :: IsBitSize n => F n

deriving instance (IsBitSize n) => Show (F n)
instance ScalarType (F n) where
  type BitSize (F n) = n

class ScalarConvertible s where
  convertUp :: Proxy s -> NextNativeScalar s -> NextNativeScalar s
  convertDown :: Proxy s -> NextNativeScalar s -> NextNativeScalar s

type family NativeScalar s where
  NativeScalar (U $(natT 8)) = Word8
  NativeScalar (U $(natT 16)) = Word16
  NativeScalar (U $(natT 32)) = Word32
  NativeScalar (U $(natT 64)) = Word64

  NativeScalar (I $(natT 8)) = Int8
  NativeScalar (I $(natT 16)) = Int16
  NativeScalar (I $(natT 32)) = Int32
  NativeScalar (I $(natT 64)) = Int64

  NativeScalar (F $(natT 32)) = Float
  NativeScalar (F $(natT 64)) = Double

type NextNativeScalar s = NativeScalar (NextNative s)

type family NextNative s where
  NextNative (U $(natT 1)) = U $(natT 8)
  NextNative (U $(natT 2)) = U $(natT 8)
  NextNative (U $(natT 3)) = U $(natT 8)
  NextNative (U $(natT 4)) = U $(natT 8)
  NextNative (U $(natT 5)) = U $(natT 8)
  NextNative (U $(natT 6)) = U $(natT 8)
  NextNative (U $(natT 7)) = U $(natT 8)

  NextNative (U $(natT 9)) = U $(natT 16)
  NextNative (U $(natT 10)) = U $(natT 16)
  NextNative (U $(natT 11)) = U $(natT 16)
  NextNative (U $(natT 12)) = U $(natT 16)
  NextNative (U $(natT 13)) = U $(natT 16)
  NextNative (U $(natT 14)) = U $(natT 16)
  NextNative (U $(natT 15)) = U $(natT 16)

  NextNative (U $(natT 17)) = U $(natT 32)
  NextNative (U $(natT 18)) = U $(natT 32)
  NextNative (U $(natT 19)) = U $(natT 32)
  NextNative (U $(natT 20)) = U $(natT 32)
  NextNative (U $(natT 21)) = U $(natT 32)
  NextNative (U $(natT 22)) = U $(natT 32)
  NextNative (U $(natT 23)) = U $(natT 32)
  NextNative (U $(natT 24)) = U $(natT 32)
  NextNative (U $(natT 25)) = U $(natT 32)
  NextNative (U $(natT 26)) = U $(natT 32)
  NextNative (U $(natT 27)) = U $(natT 32)
  NextNative (U $(natT 28)) = U $(natT 32)
  NextNative (U $(natT 29)) = U $(natT 32)
  NextNative (U $(natT 30)) = U $(natT 32)
  NextNative (U $(natT 31)) = U $(natT 32)

  NextNative (U $(natT 33)) = U $(natT 64)
  NextNative (U $(natT 34)) = U $(natT 64)
  NextNative (U $(natT 35)) = U $(natT 64)
  NextNative (U $(natT 36)) = U $(natT 64)
  NextNative (U $(natT 37)) = U $(natT 64)
  NextNative (U $(natT 38)) = U $(natT 64)
  NextNative (U $(natT 39)) = U $(natT 64)
  NextNative (U $(natT 40)) = U $(natT 64)
  NextNative (U $(natT 41)) = U $(natT 64)
  NextNative (U $(natT 42)) = U $(natT 64)
  NextNative (U $(natT 43)) = U $(natT 64)
  NextNative (U $(natT 44)) = U $(natT 64)
  NextNative (U $(natT 45)) = U $(natT 64)
  NextNative (U $(natT 46)) = U $(natT 64)
  NextNative (U $(natT 47)) = U $(natT 64)
  NextNative (U $(natT 48)) = U $(natT 64)
  NextNative (U $(natT 49)) = U $(natT 64)
  NextNative (U $(natT 50)) = U $(natT 64)
  NextNative (U $(natT 51)) = U $(natT 64)
  NextNative (U $(natT 52)) = U $(natT 64)
  NextNative (U $(natT 53)) = U $(natT 64)
  NextNative (U $(natT 54)) = U $(natT 64)
  NextNative (U $(natT 55)) = U $(natT 64)
  NextNative (U $(natT 56)) = U $(natT 64)
  NextNative (U $(natT 57)) = U $(natT 64)
  NextNative (U $(natT 58)) = U $(natT 64)
  NextNative (U $(natT 59)) = U $(natT 64)
  NextNative (U $(natT 61)) = U $(natT 64)
  NextNative (U $(natT 62)) = U $(natT 64)
  NextNative (U $(natT 63)) = U $(natT 64)

  NextNative (I $(natT 1)) = I $(natT 8)
  NextNative (I $(natT 2)) = I $(natT 8)
  NextNative (I $(natT 3)) = I $(natT 8)
  NextNative (I $(natT 4)) = I $(natT 8)
  NextNative (I $(natT 5)) = I $(natT 8)
  NextNative (I $(natT 6)) = I $(natT 8)
  NextNative (I $(natT 7)) = I $(natT 8)

  NextNative (I $(natT 9)) = I $(natT 16)
  NextNative (I $(natT 10)) = I $(natT 16)
  NextNative (I $(natT 11)) = I $(natT 16)
  NextNative (I $(natT 12)) = I $(natT 16)
  NextNative (I $(natT 13)) = I $(natT 16)
  NextNative (I $(natT 14)) = I $(natT 16)
  NextNative (I $(natT 15)) = I $(natT 16)

  NextNative (I $(natT 17)) = I $(natT 32)
  NextNative (I $(natT 18)) = I $(natT 32)
  NextNative (I $(natT 19)) = I $(natT 32)
  NextNative (I $(natT 20)) = I $(natT 32)
  NextNative (I $(natT 21)) = I $(natT 32)
  NextNative (I $(natT 22)) = I $(natT 32)
  NextNative (I $(natT 23)) = I $(natT 32)
  NextNative (I $(natT 24)) = I $(natT 32)
  NextNative (I $(natT 25)) = I $(natT 32)
  NextNative (I $(natT 26)) = I $(natT 32)
  NextNative (I $(natT 27)) = I $(natT 32)
  NextNative (I $(natT 28)) = I $(natT 32)
  NextNative (I $(natT 29)) = I $(natT 32)
  NextNative (I $(natT 30)) = I $(natT 32)
  NextNative (I $(natT 31)) = I $(natT 32)

  NextNative (I $(natT 33)) = I $(natT 64)
  NextNative (I $(natT 34)) = I $(natT 64)
  NextNative (I $(natT 35)) = I $(natT 64)
  NextNative (I $(natT 36)) = I $(natT 64)
  NextNative (I $(natT 37)) = I $(natT 64)
  NextNative (I $(natT 38)) = I $(natT 64)
  NextNative (I $(natT 39)) = I $(natT 64)
  NextNative (I $(natT 40)) = I $(natT 64)
  NextNative (I $(natT 41)) = I $(natT 64)
  NextNative (I $(natT 42)) = I $(natT 64)
  NextNative (I $(natT 43)) = I $(natT 64)
  NextNative (I $(natT 44)) = I $(natT 64)
  NextNative (I $(natT 45)) = I $(natT 64)
  NextNative (I $(natT 46)) = I $(natT 64)
  NextNative (I $(natT 47)) = I $(natT 64)
  NextNative (I $(natT 48)) = I $(natT 64)
  NextNative (I $(natT 49)) = I $(natT 64)
  NextNative (I $(natT 50)) = I $(natT 64)
  NextNative (I $(natT 51)) = I $(natT 64)
  NextNative (I $(natT 52)) = I $(natT 64)
  NextNative (I $(natT 53)) = I $(natT 64)
  NextNative (I $(natT 54)) = I $(natT 64)
  NextNative (I $(natT 55)) = I $(natT 64)
  NextNative (I $(natT 56)) = I $(natT 64)
  NextNative (I $(natT 57)) = I $(natT 64)
  NextNative (I $(natT 58)) = I $(natT 64)
  NextNative (I $(natT 59)) = I $(natT 64)
  NextNative (I $(natT 61)) = I $(natT 64)
  NextNative (I $(natT 62)) = I $(natT 64)
  NextNative (I $(natT 63)) = I $(natT 64)

  NextNative (F $(natT 3)) = F $(natT 32)
  NextNative (F $(natT 4)) = F $(natT 32)
  NextNative (F $(natT 5)) = F $(natT 32)
  NextNative (F $(natT 6)) = F $(natT 32)
  NextNative (F $(natT 7)) = F $(natT 32)
  NextNative (F $(natT 8)) = F $(natT 32)
  NextNative (F $(natT 9)) = F $(natT 32)
  NextNative (F $(natT 10)) = F $(natT 32)
  NextNative (F $(natT 11)) = F $(natT 32)
  NextNative (F $(natT 12)) = F $(natT 32)
  NextNative (F $(natT 13)) = F $(natT 32)
  NextNative (F $(natT 14)) = F $(natT 32)
  NextNative (F $(natT 15)) = F $(natT 32)
  NextNative (F $(natT 16)) = F $(natT 32)
  NextNative (F $(natT 17)) = F $(natT 32)
  NextNative (F $(natT 18)) = F $(natT 32)
  NextNative (F $(natT 19)) = F $(natT 32)
  NextNative (F $(natT 20)) = F $(natT 32)
  NextNative (F $(natT 21)) = F $(natT 32)
  NextNative (F $(natT 22)) = F $(natT 32)
  NextNative (F $(natT 23)) = F $(natT 32)
  NextNative (F $(natT 24)) = F $(natT 32)
  NextNative (F $(natT 25)) = F $(natT 32)
  NextNative (F $(natT 26)) = F $(natT 32)
  NextNative (F $(natT 27)) = F $(natT 32)
  NextNative (F $(natT 28)) = F $(natT 32)
  NextNative (F $(natT 29)) = F $(natT 32)
  NextNative (F $(natT 30)) = F $(natT 32)
  NextNative (F $(natT 31)) = F $(natT 32)

  NextNative (F $(natT 33)) = F $(natT 64)
  NextNative (F $(natT 34)) = F $(natT 64)
  NextNative (F $(natT 35)) = F $(natT 64)
  NextNative (F $(natT 36)) = F $(natT 64)
  NextNative (F $(natT 37)) = F $(natT 64)
  NextNative (F $(natT 38)) = F $(natT 64)
  NextNative (F $(natT 39)) = F $(natT 64)
  NextNative (F $(natT 40)) = F $(natT 64)
  NextNative (F $(natT 41)) = F $(natT 64)
  NextNative (F $(natT 42)) = F $(natT 64)
  NextNative (F $(natT 43)) = F $(natT 64)
  NextNative (F $(natT 44)) = F $(natT 64)
  NextNative (F $(natT 45)) = F $(natT 64)
  NextNative (F $(natT 46)) = F $(natT 64)
  NextNative (F $(natT 47)) = F $(natT 64)
  NextNative (F $(natT 48)) = F $(natT 64)
  NextNative (F $(natT 49)) = F $(natT 64)
  NextNative (F $(natT 50)) = F $(natT 64)
  NextNative (F $(natT 51)) = F $(natT 64)
  NextNative (F $(natT 52)) = F $(natT 64)
  NextNative (F $(natT 53)) = F $(natT 64)
  NextNative (F $(natT 54)) = F $(natT 64)
  NextNative (F $(natT 55)) = F $(natT 64)
  NextNative (F $(natT 56)) = F $(natT 64)
  NextNative (F $(natT 57)) = F $(natT 64)
  NextNative (F $(natT 58)) = F $(natT 64)
  NextNative (F $(natT 59)) = F $(natT 64)
  NextNative (F $(natT 61)) = F $(natT 64)
  NextNative (F $(natT 62)) = F $(natT 64)
  NextNative (F $(natT 63)) = F $(natT 64)
