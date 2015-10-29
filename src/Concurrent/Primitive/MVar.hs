{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Concurrent.Primitive.MVar where

import GHC.IO
import GHC.MVar
import GHC.Prim

foreign import prim "localTakeMVarzh" localTakeMVar# :: MVar# d a -> State# d -> (# State# d, a #)

localTakeMVar :: MVar a -> IO a
localTakeMVar (MVar m) = IO $ \s -> localTakeMVar# m s
