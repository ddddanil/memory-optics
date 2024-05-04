module Control.Monad.Distributive
  ( Distributive(distribute) )
where

import           Control.Applicative (Applicative (pure))
import           Control.Category    ((.))
import           Control.Monad       (Monad, join)
import           Data.Functor        (Functor (fmap))

class Distributive m f where
    distribute :: m (f a) -> f (m a)

instance (Monad m) => Distributive m m where
    distribute = fmap pure . join

 -- law:
 -- distribute . fmap act = fmap pure . act . fmap join . distribute
