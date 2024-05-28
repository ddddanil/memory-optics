module Data.Functor.Barbie.Utils
  (bmapAccumL)
where
import           Data.Functor        (Functor (fmap))
import           Data.Functor.Barbie (TraversableB (btraverse))
import           Data.Functor.Utils  (StateL (StateL, runStateL))


bmapAccumL
  :: forall b s f g
  . (TraversableB b)
  => (forall a. s -> f a -> (s, g a))
  -> s
  -> b f
  -> (s, b g)
bmapAccumL f' s'
  = let
  f :: forall a. f a -> (StateL s) (g a)
  f t = StateL (`f'` t)
  in fmap (`runStateL` s') ((btraverse @_ @_ @(StateL s)) f)
