module Sheet where

import Data.Array

type CellRef = (Char, Int)

type Sheet a = Array CellRef a

data BinOp = Add | Sub | Mul | Div

-- code modified here
data Exp = Lit Double
         | Ref CellRef
-- added
         | Sum CellRef CellRef
         | Avg CellRef CellRef
--added
         | App BinOp Exp Exp
-- code modified here


-- code modified here
evalCell :: Sheet Double -> Exp -> Double
evalCell _ (Lit v)        = v
evalCell s (Ref r)        = s ! r
-- added
evalCell s (Sum c1 c2)    = (evalOp Add) (s ! c1) (s ! c2)
evalCell s (Avg c1 c2)    = (evalOp Div) (s ! c1 + s ! c2) 2.0
-- added
evalCell s (App op e1 e2) = (evalOp op) (evalCell s e1) (evalCell s e2)
-- code modified here




evalOp :: BinOp -> (Double -> Double -> Double)
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)
evalOp Div = (/)


evalSheet :: Sheet Exp -> Sheet Double
evalSheet s = s'
    where
        s' = array (bounds s) [ (r, evalCell s' (s ! r)) | r <- indices s ]

testSheet :: Sheet Exp
testSheet = array (('a', 1), ('c', 3))
                  [ (('a', 1), Lit 1.0),
                    (('a', 2), Ref ('b', 1)),
                    (('a', 3), Lit 3.0),
                    (('b', 1), App Add (Ref ('c', 2)) (Ref ('b', 2))),
                    (('b', 2), Lit 2.0),
                    (('b', 3), App Mul (Ref ('a', 1)) (Ref ('a', 2))),
                    (('c', 1), App Add (Ref ('a',1)) (Ref ('b',3))),
                    (('c', 2), Lit 3.0),
                    (('c', 3), Lit 7.0)
                  ]


testSheet1 :: Sheet Exp
testSheet1 = array (('a', 1), ('c', 3))
             [ (('a', 1), Lit 1.0),
               (('a', 2), Ref ('b', 1)),
               (('a', 3), Lit 3.0),
               (('b', 1), App Add (Ref ('c', 2)) (Ref ('b', 2))),
               (('b', 2), Lit 2.0),
               (('b', 3), App Mul (Ref ('a', 1)) (Ref ('a', 2))),
               (('c', 1), App Add (Ref ('a',1)) (Ref ('b',3))),
               (('c', 2), Lit 3.0),
               (('c', 3), Lit 7.0)
             ]
