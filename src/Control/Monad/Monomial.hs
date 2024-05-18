module Control.Monad.Monomial
  ( Monomial(Monomial)
  , runMonomial
  )
where

import           Control.Monad (Functor)

newtype Monomial m a b x = Monomial (m (a, b -> m x))
  deriving (Functor)

runMonomial :: Monomial m a b x -> m (a, b -> m x)
runMonomial (Monomial r) = r
