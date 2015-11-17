module Concurrent.IVar where

import Concurrent.Exception
import Concurrent.Par
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import GHC.IO

data IVar s a = IVar {-# UNPACK #-} !(MVar a) a

newIVar :: MonadPar d i s m => m (IVar s a)
newIVar = unsafeParIO $ do
  x <- newEmptyMVar
  IVar x <$> unsafeDupableInterleaveIO (readMVar x) `catch` \BlockedIndefinitelyOnMVar -> throw BlockedIndefinitelyOnIVar

readIVar :: IVar s a -> a
readIVar (IVar _ a) = a

writeIVar :: (MonadPar d i s m, Eq a) => IVar s a -> a -> m ()
writeIVar (IVar m _) a = unsafeParIO $ do
  t <- tryPutMVar m a
  unless t $ do
     b <- readMVar m
     unless (a == b) $ throwIO Contradiction

-- | Like 'writeIVar' but assumes (without checking) that the write is always consistent.
unsafeWriteIVar :: MonadPar d i s m => IVar s a -> a -> m ()
unsafeWriteIVar (IVar m _) a = unsafeParIO $ () <$ tryPutMVar m a

infixr 3 <&&>

-- | Maximally lazy &&
(<&&>) :: Bool -> Bool -> Bool
x <&&> y = runPar $ do
  z <- newIVar
  fork $
    if x
    then when y $ unsafeWriteIVar z True
    else unsafeWriteIVar z False
  fork $ unless y $ unsafeWriteIVar z False
  return $ readIVar z

infixr 2 <||>

-- | Maximally lazy ||
(<||>) :: Bool -> Bool -> Bool
x <||> y = runPar $ do
  z <- newIVar
  fork $
    if x
    then unsafeWriteIVar z True
    else unless y $ unsafeWriteIVar z False
  fork $ when y $ unsafeWriteIVar z True
  return $ readIVar z
