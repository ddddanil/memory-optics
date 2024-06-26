module Data.Functor.Barbie.Utils
  (bmapAccumL, bvoid)
where
import           Data.Function       (($))
import           Data.Functor        (Functor (fmap))
import           Data.Functor.Barbie (ApplicativeB (bpure),
                                      TraversableB (btraverse))
import           Data.Functor.Const  (Const (Const))
import           Data.Functor.Utils  (StateL (StateL, runStateL))
import           Data.Void           (Void)
import           GHC.Err             (error)


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

bvoid
  :: forall b
  . (ApplicativeB b)
  => b (Const Void)
bvoid = bpure (Const $ error "void constructor")
