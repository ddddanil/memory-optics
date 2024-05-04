{-# LANGUAGE IncoherentInstances #-}
-- |
-- Description: Short description
-- Copyright:
--   (c) Jules Hedges, 2023
--   Danil Doroshin, 2024
--
-- From https://julesh.com/2023/06/07/monadic-lenses-are-the-optic-for-right-monad-modules-i/
module Control.Monad.RightModule
  ( RightModule(act) )
where

import           Control.Category   ((.))
import           Control.Monad      (Monad, join)
import           Data.Functor.Const (Const (Const, getConst))

class RightModule m f where
    act :: f (m a) -> f a

instance (Monad m) => RightModule m m where
    act = join

instance RightModule m (Const a) where
    act = Const . getConst
