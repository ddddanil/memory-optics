module Data.Memory.Effect
  ( Memory(ReadMem, WriteMem)
  , readMem, writeMem
  , runMemoryAsNative
  , runMemoryMonadIO
  )
where

import           Control.Category (id)
import           Data.Function    (($))
import           Data.Memory      (MemoryMonad,
                                   NativeType (readMemM, writeMemM))
import qualified Foreign.Ptr      as GHC
import           GHC.IO           (IO)
import           Polysemy         (Embed, InterpreterFor, Member, Sem, embed,
                                   interpret, makeSem)
import           Polysemy.Embed   (runEmbedded)

data Memory p m a where
  ReadMem :: (NativeType p a) => p a -> Memory p m a
  WriteMem :: (NativeType p a) => p a -> a -> Memory p m ()

makeSem ''Memory

-- read :: forall r p a b. (NativeType p a, NativeType p b, Member (Memory p) r) => MonadicOptic (Sem r) (p a) (p b) a b
-- read = readOffset offsetSelf

-- readOffset :: forall r p s t a b. (NativeType p a, NativeType p b, Member (Memory p) r) => Offset p s a -> MonadicOptic (Sem r) (p s) (p t) a b
-- readOffset o = let
--   read_ :: p s -> Sem r (a, (p t, Offset p t b))
--   read_ ptr = do
--     val <- readMem (ptr `addOffset` o)
--     return (val, (unsafeCastPointer ptr, unsafeCastOffset o))
--   write_ :: (p t, Offset p t b) -> b -> Sem r (p t)
--   write_ (ptr, off) d = writeMem (ptr `addOffset` off) d >> return ptr
--   in monadicOptic (ExistentialMonadicOptic read_ write_)

runMemoryAsNative :: (Member (Embed (MemoryMonad p)) r) => InterpreterFor (Memory p) r
runMemoryAsNative = interpret $ \case
  ReadMem ptr -> embed $ readMemM ptr
  WriteMem ptr d -> embed $ writeMemM ptr d

runMemoryMonadIO :: Member (Embed IO) r => InterpreterFor (Embed (MemoryMonad GHC.Ptr)) r
runMemoryMonadIO = runEmbedded id
