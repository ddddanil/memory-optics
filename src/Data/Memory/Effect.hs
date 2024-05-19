{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Memory.Effect
  ( Memory(ReadMem, WriteMem)
  , readMem, writeMem
  , runMemoryAsEmbed
  , derefOffset, derefOffset', deref, deref'
  )
where

import           Control.Lens.Monadic (ExistentialMonadicLens (ExistentialMonadicLens),
                                       MonadicLens, MonadicLens', monadicLens)
import           Control.Monad        (Monad (return, (>>)))
import           Data.Function        (($))
import           Data.Memory          (MemoryMonad,
                                       NativeType (readMemM, writeMemM), Offset,
                                       Pointer (addOffset, offsetSelf, unsafeCastOffset, unsafeCastPointer))
import           Polysemy             (Embed, InterpreterFor, Member, Sem,
                                       embed, interpret, makeSem)

data Memory p m a where
  ReadMem :: (NativeType p a) => p a -> Memory p m a
  WriteMem :: (NativeType p a) => p a -> a -> Memory p m ()

makeSem ''Memory

-- read :: forall r p a b. (NativeType p a, NativeType p b, Member (Memory p) r) => MonadicOptic (Sem r) (p a) (p b) a b
-- read = readOffset offsetSelf

derefOffset :: forall r p s t a b. (NativeType p a, NativeType p b, Member (Memory p) r) => Offset p s a -> MonadicLens (Sem r) (p s) (p t) a b
derefOffset o = let
  read_ :: p s -> Sem r (a, (p t, Offset p t b))
  read_ ptr = do
    val <- readMem (ptr `addOffset` o)
    return (val, (unsafeCastPointer ptr, unsafeCastOffset o))
  write_ :: (p t, Offset p t b) -> b -> Sem r (p t)
  write_ (ptr, off) d = writeMem (ptr `addOffset` off) d >> return ptr
  in monadicLens (ExistentialMonadicLens read_ write_)

derefOffset' :: forall r p s a. (NativeType p a, Member (Memory p) r) => Offset p s a -> MonadicLens' (Sem r) (p s) a
derefOffset' = derefOffset

deref :: forall r p a b. (NativeType p a, NativeType p b, Member (Memory p) r) => MonadicLens (Sem r) (p a) (p b) a b
deref = derefOffset @r offsetSelf

deref' :: forall r p a. (NativeType p a, Member (Memory p) r) => MonadicLens' (Sem r) (p a) a
deref' = derefOffset' @r offsetSelf

runMemoryAsEmbed :: (Member (Embed (MemoryMonad p)) r) => InterpreterFor (Memory p) r
runMemoryAsEmbed = interpret $ \case
  ReadMem ptr -> embed $ readMemM ptr
  WriteMem ptr d -> embed $ writeMemM ptr d
