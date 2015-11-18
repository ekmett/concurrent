{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}

-- | This is a transient-style pairing heap. (You have to manually pass 
-- around the root. Almost every operation destroys the usability of
-- the old reference to the root.)
module Concurrent.Internal.Heap
  ( Heap(..)
  , Node
  , insert
  , extractMin
  , meld
  , delete
  , setKey
  , key
  , value
  ) where

import Control.Monad
import Data.Struct.Internal
import Data.Struct.TH
import GHC.ST

makeStruct [d|
  data Heap k v s = Heap
    { parent, child, sibling :: !(Heap k v s)
    , key :: k
    , value :: v
    } |]

type Node = Heap

-- | @'insert' k v h@ returns the pair @(n,h')@ of the node for the inserted entry as well as the new heap 
-- root.
insert :: Ord k => k -> v -> Heap k v s -> ST s (Node k v s, Heap k v s)
insert k v r
  | isNil r = do
    n <- newHeap Nil Nil Nil k v
    return (n,n)
  | otherwise = do
    n <- newHeap Nil Nil Nil k v 
    r' <- meld r n
    return (n,r')

extractMin :: Ord k => Heap k v s -> ST s (Node k v s, Heap k v s)
extractMin r
  | isNil r   = return (r, r)
  | otherwise = do
    r' <- combineSiblings r
    return (r, r')

-- | @'meld' l r@ merges the contents of two transient min-heaps @l@ and @r@
-- mutably. It destructively adds the contents of @r@ to @l@ and returns the modified @l@.
meld :: Ord k => Heap k v s -> Heap k v s -> ST s (Heap k v s)
meld p q = do
  a <- getField key p
  b <- getField key q
  if a < b then p <$ attachChild p q
  else q <$ attachChild q p

attachChild :: Node k v s -> Node k v s -> ST s ()
attachChild p c = do
  s <- get child p
  set child p c
  set sibling c s
  set parent c p

combineSiblings :: Ord k => Node k v s -> ST s (Node k v s)
combineSiblings p = do
  pairChildren p
  linkChildren p

pairChildren :: Ord k => Node k v s -> ST s ()
pairChildren p = do
  lc <- get child p 
  unless (isNil lc) $ do
    set child p Nil
    go lc 
 where
  go c1 = do
    c2 <- get sibling c1
    if isNil c2 then attachChild p c1
    else do
      set sibling c1 Nil 
      n <- get sibling c2 
      set sibling c2 Nil
      c3 <- meld c1 c2
      attachChild p c3
      unless (isNil n) $ go n

linkChildren :: Ord k => Node k v s -> ST s (Node k v s)
linkChildren p = do
  r <- get child p
  if isNil r then return Nil
  else do
     c <- get sibling r
     set sibling r Nil
     go r c
 where
   go r c
     | isNil c   = return r
     | otherwise = do
       c' <- get sibling c
       set sibling c Nil
       r' <- meld r c
       go r' c'

-- | Remove a node from the heap it is in.
delete :: Ord k => Heap k v s -> Node k v s -> ST s (Heap k v s)
delete r n = do
  if r == n then snd <$> extractMin n
  else do
    p <- get parent n
    cutParent n
    q <- snd <$> extractMin n
    if isNil q then return r
    else meld p q
    
-- | Generalized 'decreaseKey' (allowed to increase key)
setKey :: Ord k => Heap k v s -> Node k v s -> k -> ST s (Heap k v s)
setKey h n k = do
  ok <- getField key n
  setField key n k
  if n == h && k <= ok then return h
  else do
   cutParent n
   meld h n

-- | Assumes parent exists
cutParent :: Ord k => Node k v s -> ST s ()
cutParent n = do
  p <- get parent n
  x <- get child p
  if x == n then do
    ns <- get sibling n 
    set child p ns
  else deleteSibling n x
  set parent n Nil
  set sibling n Nil

deleteSibling :: Node k v s -> Node k v s -> ST s ()
deleteSibling n p = do
  q <- get sibling p 
  if n == q then do
    ns <- get sibling n
    set sibling q ns
  else deleteSibling n q
