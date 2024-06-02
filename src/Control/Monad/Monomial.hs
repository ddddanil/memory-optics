module Control.Monad.Monomial
  ( Monomial(Monomial)
  , runMonomial
  )
where

import           Control.Applicative (Applicative (liftA2, pure))
import           Control.Monad       (Monad (return))
import           Data.Function       (const, ($), (.))
import           Data.Functor        (Functor)
import           Data.Monoid         (Monoid (mempty), (<>))

newtype Monomial m a b x = Monomial (m (a, b -> m x))
  deriving (Functor)

instance (Monad m, Monoid a) => Applicative (Monomial m a b) where
  pure :: x -> Monomial m a b x
  pure x = Monomial . pure $ (mempty, const (pure x))
  liftA2 :: (x -> y -> z) -> Monomial m a b x -> Monomial m a b y -> Monomial m a b z
  liftA2 f (Monomial ma) (Monomial mb) = Monomial $ do
    (a, fx) <- ma
    (a', fy) <- mb
    let f' b = do
          x <- fx b
          y <- fy b
          return (f x y)
    pure (a <> a', f')

runMonomial :: Monomial m a b x -> m (a, b -> m x)
runMonomial (Monomial r) = r
