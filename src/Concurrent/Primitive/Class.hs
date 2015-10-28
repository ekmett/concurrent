{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Concurrent.Primitive.Class where

import Control.Monad.Primitive

class (PrimMonad m, PrimState m ~ s) => MonadPrim s m | s -> m
instance (PrimMonad m, PrimState m ~ s) => MonadPrim s m

class MonadPrim RealWorld m => MonadPrimIO m
instance MonadPrim RealWorld m => MonadPrimIO m

primIO :: MonadPrimIO m => IO a -> m a 
primIO = primToPrim
{-# INLINE primIO #-}
