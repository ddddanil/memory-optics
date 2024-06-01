module Data.Memory.Scalars
  ( ByteOrder(Msb, Lsb)
  , NativeBO, NetworkBO
  , U(U), I(I), F(F)
  , NativeScalar
  )
where

import           Data.Eq              (Eq)
import           Data.Int             (Int16, Int32, Int64, Int8)
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

data U n where
  U :: Nat n => U n

deriving instance (Nat n) => Show (U n)

data I n  where
  I :: Nat n => I n

deriving instance (Nat n) => Show (I n)

data F n  where
  F :: Nat n => F n

deriving instance (Nat n) => Show (F n)

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
