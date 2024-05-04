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
  )
where

import           Control.Lens              (LensLike)
import           Control.Monad.RightModule (RightModule (act))
import           Data.Functor.Const        (Const (Const, getConst))

type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b

monadicGet :: MonadicLens m s t a b -> s -> a
monadicGet l s = getConst (l Const s)

monadicPut :: (Monad m) => MonadicLens m s t a b -> s -> b -> m t
monadicPut l s b = l (const (return b)) s

monadicLens :: (s -> a) -> (s -> b -> m t) -> MonadicLens m s t a b
monadicLens g p k s = act (fmap (p s) (k (g s)))
