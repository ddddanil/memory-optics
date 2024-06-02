module Control.Monad.Distributive
  ( Distributive(distribute) )
where

import           Control.Applicative    (Applicative (pure),
                                         WrappedMonad (WrapMonad, unwrapMonad))
import           Control.Monad          (Monad, join)
import           Control.Monad.Monomial (Monomial (Monomial))
import           Data.Function          (($), (.))
import           Data.Functor           (Functor (fmap))

class Distributive m f where
    distribute :: m (f a) -> f (m a)

instance (Monad m) => Distributive m m where
    distribute = fmap pure . join

instance (Monad m) => Distributive m (WrappedMonad m) where
  distribute = WrapMonad . fmap unwrapMonad

instance (Monad m) => Distributive m (Monomial m a b) where
  distribute maf = Monomial $ do
    Monomial af <- maf
    (a, f) <- af
    pure (a, pure . f)

 -- law:
 -- distribute . fmap act = fmap pure . act . fmap join . distribute
