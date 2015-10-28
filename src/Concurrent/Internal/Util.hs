{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Concurrent.Internal.Util
  ( forN_
  , primIO
  , rawThreadId
  ) where

import Control.Monad.Primitive
import Foreign.C.Types
import GHC.Conc.Sync
import GHC.Exts

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
forN_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
forN_ start end _ | start > end = error "for_: start is greater than end"
forN_ start end fn = loop start
  where
   loop !i | i == end  = return ()
           | otherwise = do fn i; loop (i+1)
{-# INLINE forN_ #-}

primIO :: (PrimMonad m, PrimState m ~ RealWorld) => IO a -> m a
primIO = primToPrim
{-# INLINE primIO #-}

foreign import ccall unsafe "rts_getThreadId" rts_getThreadId :: ThreadId# -> CInt

rawThreadId :: ThreadId -> CInt
rawThreadId (ThreadId x) = rts_getThreadId x
{-# INLINE rawThreadId #-}
