{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Concurrent.Primitive.Class where

import Control.Monad.Primitive
import Control.Monad.ST

class (PrimMonad m, PrimState m ~ s) => MonadPrim s m | s -> m
instance (PrimMonad m, PrimState m ~ s) => MonadPrim s m

class MonadPrim RealWorld m => MonadPrimIO m
instance MonadPrim RealWorld m => MonadPrimIO m

primIO :: MonadPrimIO m => IO a -> m a 
primIO = primToPrim
{-# INLINE primIO #-}

primST :: PrimMonad m => ST (PrimState m) a -> m a
primST = primToPrim
{-# INLINE primST #-}
