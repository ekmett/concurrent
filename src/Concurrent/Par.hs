{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Concurrent.Par
  ( Par
  , runPar
  , runParIO
  , Determinism(..)
  , Idempotence(..)
  , MonadPar(fork)
  ) where

import Concurrent.Par.Unsafe
import Control.Monad.IO.Class
import System.IO.Unsafe

runPar :: (forall s. Par 'Deterministic i s a) -> a
runPar (Par m) = unsafePerformIO m

runParIO :: MonadIO m => (forall s. Par d i s a) -> m a
runParIO (Par m) = liftIO m

