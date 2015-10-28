{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Concurrent.Internal.Thread
  ( rawThreadId
  , withCapability
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Primitive
import Foreign.C.Types
import GHC.Conc.Sync
import GHC.Prim
import GHC.Exts

foreign import ccall unsafe "rts_getThreadId" rts_getThreadId :: ThreadId# -> CInt
foreign import prim "pinThreadzh"   pinThread#   :: State# s -> (# State# s, Int#, Int# #)
-- The funny result type is used to hack around <https://ghc.haskell.org/trac/ghc/ticket/11032>
foreign import prim "unpinThreadzh" unpinThread# :: State# s -> (# State# s #) 

rawThreadId :: ThreadId -> CInt
rawThreadId (ThreadId x) = rts_getThreadId x
{-# INLINE rawThreadId #-}

-- | Pin the current thread to whatever capability is executing it. 
-- This returns the capability we are pinned to and if we were previously pinned.
pinThread :: (PrimMonad m, PrimState m ~ RealWorld) => m (Bool, Int)
pinThread = primitive $ \s -> case pinThread# s of
  (# s', b, c #) -> (# s', (isTrue# b, I# c) #)
{-# INLINE pinThread #-}

unpinThread :: (PrimMonad m, PrimState m ~ RealWorld) => Bool -> m ()
unpinThread b = unless b $ primitive $ \s -> case unpinThread# s of
  (# s' #) -> (# s', () #)
{-# INLINE unpinThread #-}

-- | Pin the current thread to its capability and run a computation.
--
-- This lets us use "capability-local" variables and the like.
withCapability :: (MonadMask m, PrimMonad m, PrimState m ~ RealWorld) => (Int -> m a) -> m a 
withCapability f = bracket pinThread (unpinThread . fst) (f . snd)
