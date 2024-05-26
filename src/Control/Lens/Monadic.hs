-- |
-- Description: Short description
-- Copyright:
--   (c) Jules Hedges, 2023
--   Danil Doroshin, 2024
--
-- From https://julesh.com/2023/06/07/monadic-lenses-are-the-optic-for-right-monad-modules-i/
{-# LANGUAGE FieldSelectors #-}
module Control.Lens.Monadic
  ( ExistentialMonadicLens(ExistentialMonadicLens)
  , MonadicLens, MonadicLens'
  , monadicLens, unMonadicLens
  , MLensFor(MLensFor, getMLensFor)
  , MLensB
  , getM, putM, modifyM
  , hoistM
  )
where

import           Control.Lens               (LensLike)
import           Control.Monad              (Monad (return, (>>=)))
import           Control.Monad.Distributive (Distributive (distribute))
import           Control.Monad.Monomial     (Monomial (Monomial), runMonomial)
import           Control.Monad.RightModule  (RightModule (act))
import           Data.Function              (($), (.))
import           Data.Functor               (Functor (fmap))
import           Data.Functor.Identity      (Identity)
import           Data.Tuple                 (uncurry)

data ExistentialMonadicLens m s t a b where
  ExistentialMonadicLens :: forall m s t a b z. (s -> m (a, z)) -> (z -> b -> m t) -> ExistentialMonadicLens m s t a b

type MonadicLens m s t a b = forall f.
    (Functor f, RightModule m f, Distributive m f)
    => LensLike f s t a b

type MonadicLens' m s a = MonadicLens m s s a a

monadicLens :: (Monad m) => ExistentialMonadicLens m s t a b -> MonadicLens m s t a b
monadicLens (ExistentialMonadicLens g p) k s
  = act . fmap (>>= uncurry p) . distribute $ do
    (a, z) <- g s
    return (fmap (z, ) (k a))

runMonadicLens :: (Monad m) => MonadicLens m s t a b -> s -> m (a, b -> m t)
runMonadicLens l = runMonomial . l (\a -> Monomial (return (a, return)))

unMonadicLens :: (Monad m) => MonadicLens m s t a b -> ExistentialMonadicLens m s t a b
unMonadicLens l = ExistentialMonadicLens (runMonadicLens l) ($)

data MLensFor m s a = MLensFor
  { getMLensFor :: MonadicLens' m s a
  }

type MLensB m b = b (MLensFor m (b Identity))

getM :: (Monad m) => MonadicLens m s t a b -> s -> m a
getM l s = let
  get_ = runMonadicLens l
  in do
  (a, _) <- get_ s
  return a

putM :: (Monad m) => MonadicLens m s t a b -> b -> s -> m t
putM l b s = let
  get_ = runMonadicLens l
  in do
  (_, put_) <- get_ s
  put_ b

modifyM :: (Monad m) => MonadicLens m s t a b -> (a -> m b) -> s -> m t
modifyM l f s = let
  get_ = runMonadicLens l
  in do
  (a, put_) <- get_ s
  b <- f a
  put_ b

hoistM :: (Monad m, Monad n) => (forall x. m x -> n x) -> (MonadicLens m s t a b -> MonadicLens n s t a b)
hoistM f o = let
  get_ = runMonadicLens o
  read_ = fmap f get_
  write_ = fmap f
  in monadicLens (ExistentialMonadicLens read_ write_)
