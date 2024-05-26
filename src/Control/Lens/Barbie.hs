-- based on https://reasonablypolymorphic.com/blog/free-lenses/

{-# LANGUAGE FieldSelectors #-}
module Control.Lens.Barbie
  ( LensFor (LensFor, getLensFor)
  , LensB
  , GLenses (glenses)
  , getLenses
  )
where

import           Control.Lens          (Lens', _1, _2)
import           Data.Function         (($), (.))
import           Data.Functor.Identity (Identity)
import           GHC.Err               (undefined)
import           GHC.Generics          (Generic (Rep), K1 (K1), M1 (M1),
                                        U1 (U1), V1, to, (:*:) ((:*:)))
import           GHC.Generics.Lens     (_K1, _M1, generic)

data LensFor s a = LensFor
  { getLensFor :: Lens' s a
  }

type LensB b = b (LensFor (b Identity))

class GLenses z i o where
  glenses :: Lens' (z Identity) (i p) -> o p

instance
  GLenses z
  (K1 _x a)
  (K1 _x (LensFor (z Identity) a))
  where
  glenses
    :: Lens' (z Identity) (K1 _x a p)
    -> (K1 _x (LensFor (z Identity) a) p)
  glenses l = K1 $ LensFor $ l . _K1
  {-# INLINE glenses #-}

instance
  (GLenses z i o)
  => GLenses z
  (M1 _a _b i)
  (M1 _a _b o)
  where
  glenses
    :: Lens' (z Identity) (M1 _x _b i p)
    -> (M1 _x _b o p)
  glenses l = M1 $ glenses $ l . _M1
  {-# INLINE glenses #-}

instance
  (GLenses z i o, GLenses z i' o')
  => GLenses z
  (i :*: i')
  (o :*: o')
  where
  glenses
    :: Lens' (z Identity) ((i :*: i') p)
    -> ((o :*: o') p)
  glenses l
    = glenses (l . _1) :*: glenses (l . _2)
  {-# INLINE glenses #-}

instance GLenses z V1 V1 where
  glenses _ = undefined

instance GLenses z U1 U1 where
  glenses _ = U1

getLenses
  :: forall z
  . ( Generic (z Identity)
    , Generic (LensB z)
    , GLenses z
      (Rep (z Identity))
      (Rep (LensB z))
  )
  => LensB z
getLenses = to $ glenses @z $ generic
