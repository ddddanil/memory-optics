{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData             #-}
module Data.Memory.Allocation
  ()
where

import           Control.Applicative (pure)
import           Control.Monad       (Monad, fmap)
import           Data.Function       (($))
import           Data.Kind           (Type)
import           Data.Memory         (Offset, Pointer)
import           Polysemy            (Embed, InterpreterFor, Member, bindT,
                                      embed, interpret, interpretH, makeSem,
                                      runT)
import           Polysemy.Resource   (bracket)

data Allocation p = Allocation
  { start :: p ()
  , size  :: Offset p () ()
  }
  deriving ()

class AllocationSource p s | s -> p where
  type AllocationMonad p s :: Type -> Type
  alloc :: forall p. s -> (AllocationMonad p s) (Allocation p)
  dealloc :: s -> Allocation p -> (AllocationMonad p s) ()

data Alloc m a where
  WithAllocation :: (AllocationSource p s) => s -> (s -> m a) -> Alloc m a

makeSem ''Alloc

-- runAllocAsNativeResource :: forall r p s. (Monad (AllocationMonad p s)) => InterpreterFor Alloc r
-- runAllocAsNativeResource = interpretH $ \case
--   WithAllocation s a -> do
--     let allocSem = embed (alloc s)
--     let deallocSem = fmap embed dealloc
--     al <- runT allocSem
--     de <- bindT deallocSem
--     u <- bindT a
--     bracket al d u
