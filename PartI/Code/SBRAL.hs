-- Skew Binary Random-Access List

module SBRAL where

import Prelude hiding (head, tail, lookup, drop)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)
type RList a = [(Int, Tree a)]


empty :: RList a
empty = []


isEmpty :: RList a -> Bool
isEmpty ts = null ts


cons :: a -> RList a -> RList a
cons x ((w1, t1) : (w2, t2) : wts) | w1 == w2 =
    (w1 * 2 + 1, Node t1 x t2) : wts
cons x wts = ((1, Leaf x) : wts)


head :: RList a -> a
head ((_, Leaf x)     : _) = x
head ((_, Node _ x _) : _) = x


tail :: RList a -> RList a
tail ((_, Leaf _): wts)        = wts
tail ((w, Node t1 _ t2) : wts) = (w', t1) : (w', t2) : wts
    where
        w' = w `div` 2


lookup :: Int -> RList a -> a
lookup i ((w, t) : wts) | i < w      = lookupTree i w t
                        | otherwise  = lookup (i - w) wts


lookupTree :: Int -> Int -> Tree a -> a
lookupTree _ _ (Leaf x) = x
lookupTree i w (Node t1 x t2)
    | i == 0    = x
    | i <= w'   = lookupTree (i - 1) w' t1
    | otherwise = lookupTree (i - w' - 1) w' t2
    where
        w' = w `div` 2

update :: Int -> a -> RList a -> RList a
update i x (wt@(w, t) : wts) | i < w      = (w, updateTree i x w t) : wts
                             | otherwise  = wt : update (i - w) x wts


updateTree :: Int -> a -> Int -> Tree a -> Tree a
updateTree _ x _ (Leaf _) = Leaf x
updateTree i x w (Node t1 y t2)
    | i == 0    = (Node t1 x t2)
    | i <= w'   = Node (updateTree (i - 1) x w' t1) y t2
    | otherwise = Node t1 y (updateTree (i - w' - 1) x w' t2)
    where
        w' = w `div` 2


drop :: Int -> RList a -> RList a
drop _ [] = []
drop 0 t  = t
drop i (t:ts)
  | i == fst t = ts
  | i > fst t  = drop (i - fst t) ts
  | otherwise  = drop' i (fst t) (snd t) ++  ts

drop' :: Int -> Int -> Tree a -> RList a
drop' 0 _ (Leaf a) = [(1, (Leaf a))]
drop' _ _ (Leaf _) = []
drop' 0 w n@(Node _ _ _) = [(w, n)]
drop' i w (Node l _ r)
  | i <= half = drop' (i - 1) half l ++ [(half, r)]
  | otherwise = drop' (i - half -1) half r
  where
    half = w `div` 2

testDrop :: Int -> Bool
testDrop i = l1 == l2
  where
    l1 = last $ take i $ iterate (cons 1) empty
    l2 = drop i $ last $ take (i*2) $ iterate (cons 1) empty

