module Concurrent.Unamb.Unsafe where

import Concurrent.Par
import Concurrent.Par.Unsafe
import Concurrent.Promise
import Concurrent.Promise.Unsafe
import Control.Concurrent
import Control.Exception
import Data.Foldable (traverse_)
import System.IO.Unsafe
import System.Mem.Weak

unamb :: a -> a -> a
unamb x y = runPar $ do
  z <- newEmptyPromise
  t1 <- fork $ unsafeWritePromise z x
  t2 <- fork $ unsafeWritePromise z y
  -- Hold weak thread ids so that if both of these throw an exception we'll be (deterministically) BlockedIndefinitelyOnMVar
  unsafeParIO $ do
    wt1 <- mkWeakThreadId t1
    wt2 <- mkWeakThreadId t2
    unsafeInterleaveIO $ do
      r <- evaluate (readPromise z)
      deRefWeak wt1 >>= traverse_ killThread
      deRefWeak wt2 >>= traverse_ killThread
      return r
