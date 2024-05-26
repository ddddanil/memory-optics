module Data.Functor.Identity.Lens (_id) where

import           Control.Lens.Iso      (Iso', iso)
import           Data.Functor.Identity (Identity (Identity, runIdentity))

_id :: Iso' a (Identity a)
_id = iso Identity runIdentity
