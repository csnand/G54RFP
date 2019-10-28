
-- task I.1 hamming list
merge :: [Int] -> [Int] -> [Int]
merge [] _ = []
merge _ [] = []
merge xss@(x:xs) yss@(y:ys)
  | x == y = x : merge xs ys
  | x < y  = x : merge xs yss
  | x > y  = y : merge xss ys


hamming :: [Int]
hamming = 1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming))

