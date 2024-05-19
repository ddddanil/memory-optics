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

import           Control.Applicative    (Applicative (pure))
import           Control.Monad          (Monad, join, (=<<))
import           Control.Monad.Monomial (Monomial (Monomial))
import           Control.Monad.Morph    (MonadTrans (lift))
import           Data.Function          (($), (.))
import           Data.Functor.Const     (Const (Const, getConst))
import           Polysemy.Internal      (Sem, Subsume, subsume_)

class RightModule m f where
    act :: f (m a) -> f a

instance (Monad m) => RightModule m m where
    act = join

instance (MonadTrans t, Monad m) => RightModule (m) (t m) where
  act = (lift =<<)

instance (Monad m) => RightModule m (Monomial m a b) where
  act (Monomial af) = Monomial $ do
    (a, f) <- af
    pure (a, join . f)

instance (Subsume rl rr) => RightModule (Sem rl) (Sem rr) where
  act = (subsume_ =<<)

instance RightModule m (Const a) where
    act = Const . getConst
