-- |
-- Description: Short description
-- Copyright:
--   (c) Jules Hedges, 2023
--   Danil Doroshin, 2024
--
-- From https://julesh.com/2023/06/07/monadic-lenses-are-the-optic-for-right-monad-modules-i/
module Control.Lens.Monadic
  ( MonadicLens
  , monadicGet
  , monadicPut
  , monadicLens
  , ExistentialMonadicOptic(ExistentialMonadicOptic)
  , MonadicOptic
  , monadicOptic
  )
where

import           Control.Category           ((.))
import           Control.Lens               (LensLike)
import           Control.Monad              (Monad (return, (>>=)))
import           Control.Monad.Distributive (Distributive (distribute))
import           Control.Monad.RightModule  (RightModule (act))
import           Data.Function              (const, ($))
import           Data.Functor               (Functor (fmap))
import           Data.Functor.Const         (Const (Const, getConst))
import           Data.Tuple                 (uncurry)

type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b

monadicGet :: MonadicLens m s t a b -> s -> a
monadicGet l s = getConst (l Const s)

monadicPut :: (Monad m) => MonadicLens m s t a b -> s -> b -> m t
monadicPut l s b = l (const (return b)) s

monadicLens :: (s -> a) -> (s -> b -> m t) -> MonadicLens m s t a b
monadicLens g p k s = act (fmap (p s) (k (g s)))

data ExistentialMonadicOptic m s t a b where
  ExistentialMonadicOptic :: (s -> m (a, z)) -> (z -> b -> m t) -> ExistentialMonadicOptic m s t a b

type MonadicOptic m s t a b = forall f.
    (Functor f, RightModule m f, Distributive m f)
    => LensLike f s t a b

monadicOptic :: (Monad m) => ExistentialMonadicOptic m s t a b -> MonadicOptic m s t a b
monadicOptic (ExistentialMonadicOptic g p) k s
  = act . fmap (>>= uncurry p) . distribute $ do
    (a, z) <- g s
    return (fmap (z, ) (k a))
