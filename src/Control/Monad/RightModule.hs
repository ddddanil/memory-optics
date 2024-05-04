module Control.Monad.RightModule
  ( RightModule(act) )
where

import           Control.Monad
import           Data.Functor.Const

class RightModule m f where
    act :: f (m a) -> f a

instance (Monad m) => RightModule m m where
    act = join

instance RightModule m (Const a) where
    act = Const . getConst
