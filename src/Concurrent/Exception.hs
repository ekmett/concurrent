{-# LANGUAGE DeriveAnyClass #-}
module Concurrent.Exception where

import Control.Exception

data Contradiction = Contradiction deriving (Show,Exception)
