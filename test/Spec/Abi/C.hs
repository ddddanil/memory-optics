{-# LANGUAGE UndecidableInstances #-}

module Spec.Abi.C where

import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Test.Tasty.Hedgehog

import           Data.Eq               (Eq)
import           Data.Function         (($), (.))
import           Data.Functor.Barbie   (AllBF, ApplicativeB, ConstraintsB,
                                        FunctorB, TraversableB)
import           Data.Functor.Const    (Const (Const))
import           Data.Functor.Identity (Identity)
import           Data.Memory           (OffsetFor (OffsetFor),
                                        Pointer (unsafeOffsetFromBytes))
import           Data.Memory.Abi       (SizeOf (SizeOf, alignOf, sizeOf))
import           Data.Memory.Abi.C     (greedyStructLayout)
import           Data.Proxy            (Proxy (Proxy))
import           Foreign.Ptr           (Ptr)
import           GHC.Err               (undefined)
import           GHC.Generics          (Generic)
import           GHC.Show              (Show)
import           GHC.Word              (Word16, Word8)

tests = fromGroup $$(discover)

-- | * Layout tests

-- | ** Only native fields

-- | ** Layout 01

data Layout_01 f
  = Layout_01
  { f1 :: f Word8
  , f2 :: f Word8
  }
  deriving ( Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB )

deriving instance AllBF Show f Layout_01 => Show (Layout_01 f)
deriving instance AllBF Eq   f Layout_01 => Eq   (Layout_01 f)

prop_Layout_01_expect_layout :: Property
prop_Layout_01_expect_layout = let
  (actualSize, actualLayout)
    = greedyStructLayout @Layout_01 @Ptr
      (Layout_01
        { f1 = Const undefined
        , f2 = Const undefined
        })
  expectSize = SizeOf
    { sizeOf = 2
    , alignOf = 1
    }
  expectLayout = Layout_01
    { f1 = OffsetFor . unsafeOffsetFromBytes (Proxy @(Ptr (Layout_01 Identity), Ptr Word8)) $ 0
    , f2 = OffsetFor . unsafeOffsetFromBytes (Proxy @(Ptr (Layout_01 Identity), Ptr Word8)) $ 1
    }
  in property . test $ do
    actualSize === expectSize
    actualLayout === expectLayout

-- | Layout 02

data Layout_02 f
  = Layout_02
  { f1 :: f Word16
  , f2 :: f Word8
  }
  deriving ( Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB )

deriving instance AllBF Show f Layout_02 => Show (Layout_02 f)
deriving instance AllBF Eq   f Layout_02 => Eq   (Layout_02 f)

prop_Layout_02_expect_layout :: Property
prop_Layout_02_expect_layout = let
  (actualSize, actualLayout)
    = greedyStructLayout @Layout_02 @Ptr
      (Layout_02
        { f1 = Const undefined
        , f2 = Const undefined
        })
  expectSize = SizeOf
    { sizeOf = 3
    , alignOf = 2
    }
  expectLayout = Layout_02
    { f1 = OffsetFor . unsafeOffsetFromBytes (Proxy @(Ptr (Layout_02 Identity), Ptr Word8)) $ 0
    , f2 = OffsetFor . unsafeOffsetFromBytes (Proxy @(Ptr (Layout_02 Identity), Ptr Word8)) $ 2
    }
  in property . test $ do
    actualSize === expectSize
    actualLayout === expectLayout

-- | Layout 03

data Layout_03 f
  = Layout_03
  { f1 :: f Word8
  , f2 :: f Word16
  }
  deriving ( Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB )

deriving instance AllBF Show f Layout_03 => Show (Layout_03 f)
deriving instance AllBF Eq   f Layout_03 => Eq   (Layout_03 f)

prop_Layout_03_expect_layout :: Property
prop_Layout_03_expect_layout = let
  (actualSize, actualLayout)
    = greedyStructLayout @Layout_03 @Ptr
      (Layout_03
        { f1 = Const undefined
        , f2 = Const undefined
        })
  expectSize = SizeOf
    { sizeOf = 4
    , alignOf = 2
    }
  expectLayout = Layout_03
    { f1 = OffsetFor . unsafeOffsetFromBytes (Proxy @(Ptr (Layout_03 Identity), Ptr Word8)) $ 0
    , f2 = OffsetFor . unsafeOffsetFromBytes (Proxy @(Ptr (Layout_03 Identity), Ptr Word8)) $ 2
    }
  in property . test $ do
    actualSize === expectSize
    actualLayout === expectLayout
