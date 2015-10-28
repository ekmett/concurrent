module Concurrent.IVar where

import Concurrent.Exception
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import GHC.IO

data IVar a = IVar {-# UNPACK #-} !(MVar a) a

newIVar :: IO (IVar a)
newIVar = do
  x <- newEmptyMVar
  IVar x <$> unsafeDupableInterleaveIO (readMVar x) `catch` \BlockedIndefinitelyOnMVar -> throw BlockedIndefinitelyOnIVar

readIVar :: IVar a -> a
readIVar (IVar _ a) = a

writeIVar :: Eq a => IVar a -> a -> IO ()
writeIVar (IVar m _) a = do
  t <- tryPutMVar m a
  unless t $ do
     b <- readMVar m
     unless (a == b) $ throwIO Contradiction

unsafeWriteIVar :: IVar a -> a -> IO ()
unsafeWriteIVar (IVar m _) a = () <$ tryPutMVar m a
