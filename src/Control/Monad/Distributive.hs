module Control.Monad.Distributive
  ( Distributive(distribute) )
where

import           Control.Monad (join)

class Distributive m f where
    distribute :: m (f a) -> f (m a)

instance (Monad m) => Distributive m m where
    distribute = fmap pure . join
