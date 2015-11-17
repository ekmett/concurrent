module Concurrent.Bool where

import Concurrent.IVar
import Concurrent.Par
import Control.Monad

infixr 3 <&&>

-- | Maximally lazy &&
(<&&>) :: Bool -> Bool -> Bool
x <&&> y = runPar $ do
  z <- newIVar
  fork $ if x then when y $ unsafeWriteIVar z True
         else unsafeWriteIVar z False
  fork $ unless y $ unsafeWriteIVar z False
  return $ readIVar z

infixr 2 <||>

-- | Maximally lazy ||
(<||>) :: Bool -> Bool -> Bool
x <||> y = runPar $ do
  z <- newIVar
  fork $ if x then unsafeWriteIVar z True
         else unless y $ unsafeWriteIVar z False
  fork $ when y $ unsafeWriteIVar z True
  return $ readIVar z
