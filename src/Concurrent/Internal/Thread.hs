{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Concurrent.Internal.Thread
  ( withCapability
  , currentThread
  , currentCapability
  ) where

import Concurrent.Primitive.Class
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Primitive
import GHC.Prim
import GHC.Exts

foreign import prim "pinThreadzh"   pinThread#   :: State# s -> (# State# s, Int# #)
-- The funny result type is used to hack around <https://ghc.haskell.org/trac/ghc/ticket/11032>
foreign import prim "unpinThreadzh" unpinThread# :: State# s -> (# State# s #)
foreign import prim "currentThreadzh" currentThread# :: State# s -> (# State# s, Int# #)
foreign import prim "currentCapabilityzh" currentCapability# :: State# s -> (# State# s, Int# #)

-- | Pin the current thread to whatever capability is executing it.
-- This returns the capability we are pinned to and if we were previously pinned.
pinThread :: MonadPrimIO m => m Bool
pinThread = primitive $ \s -> case pinThread# s of
  (# s', b #) -> (# s', isTrue# b #)
{-# INLINE pinThread #-}

unpinThread :: MonadPrimIO m => Bool -> m ()
unpinThread b = unless b $ primitive $ \s -> case unpinThread# s of
  (# s' #) -> (# s', () #)
{-# INLINE unpinThread #-}

-- | Pin the current thread to its capability and run a computation.
--
-- This lets us use "capability-local" variables and the like.
withCapability :: (MonadMask m, MonadPrimIO m) => m a -> m a
withCapability m = bracket pinThread unpinThread (const m)
{-# INLINE withCapability #-}

-- | Retrieve the current thread id
currentThread :: PrimMonad m => m Int
currentThread = primitive $ \s -> case currentThread# s of
  (# s', i #) -> (# s', I# i #)
{-# INLINE currentThread #-}

-- | Retrieve the current capability id
currentCapability :: PrimMonad m => m Int
currentCapability = primitive $ \s -> case currentCapability# s of
  (# s', i #) -> (# s', I# i #)
{-# INLINE currentCapability #-}
