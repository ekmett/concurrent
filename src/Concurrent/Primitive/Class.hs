{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Concurrent.Primitive.Class where

import Control.Monad.Primitive

class (PrimMonad m, PrimState m ~ s) => MonadPrim s m | s -> m
instance (PrimMonad m, PrimState m ~ s) => MonadPrim s m

