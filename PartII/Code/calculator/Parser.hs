module Parser (eval) where
import Data.Char
import Control.Applicative

-- tree means what has parsed and string means remaining string
-- it will return a list to indicates the result of parsing
-- singleton list means success and empty means failure

-- parser with a dummy constructor
newtype Parser a = P (String -> [(a, String)])

-- apply constructor
parse :: Parser a -> String -> [(a,String)]
parse (P p) str = p str

-- parse string result first item and rest as result
-- or empty list as fail
item :: Parser Char
item = P(\str -> case str of
           []     -> []
           (x:xs) -> [(x,xs)])
-- make Parser functor, applicative and monad to combine several together
-- fmap applies function to the result of a parser
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\str -> case parse p str of
                   [] -> []
                   [(a, rest)] -> [(g a, rest)])


instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = P (\str -> [(a, str)])

  -- <*> :: Parser (a -> b) -> Parser a-> Parser b
  pa <*> pb = P (\str -> case parse pa str of
                    [] -> []
                    [(a, rest)] -> parse (fmap a pb) rest)

instance Monad Parser where
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\str -> case parse p str of
                  [] -> []
                  [(a, rest)] -> parse (f a) rest)

-- this part is about making chioces -> combine parser together for
-- more complex behaviours

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  pa <|> pb = P (\str -> case parse pa str of
                    [] -> parse pb str
                    [(a,rest)] -> [(a,rest)])

-- check if the input str satisfiy the predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- parser for single digit
digit :: Parser Char
digit = sat isDigit

-- parser for special chars
char :: Char -> Parser Char
char c = sat (== c)

-- parser for string
string :: String -> Parser String
string [] = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)

-- parser for spaces (space tab newline) and return empty tuple
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- handling spaces
token :: Parser a -> Parser a
token p = do space
             n <- p
             space
             return n

-- parser that ignore spaces around natural numbers
natural :: Parser Double
natural = token nat

--parser that ignore spaces around integers
integer :: Parser Double
integer = token int

-- parser for integers based on nat parser
int :: Parser Double
int = nat <|>
      do char '-'
         n <- nat
         return ((-1) *  n)

-- parser for natural numbers
nat :: Parser Double
nat = do xs <- some digit
         return (read xs)



double :: Parser Double
double = do char '-'
            d <- double'
            return (-d)
         <|> double' <|> integer


double' :: Parser Double
double' = do n <- some digit
             char '.'
             decimal <- some digit
             return ( (read $ n ++ '.' : decimal) * (-1) )

-- parser that ignore spaces around special symbols
symbol :: String -> Parser String
symbol str = token (string str)

-- expr ::= term + expr | term - expr | term
-- term ::= factor * term | factor / term | factor
-- factor ::= ( expr ) | double
-- double ::= ... -1.0 | 0.0 | 1.0 | 2.0 ...

expr :: Parser Double
expr = plus <|> minus <|> term

plus :: Parser Double
plus = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)


minus :: Parser Double
minus = do t <- term
           do symbol "-"
              e <- expr
              return (t - e)


term :: Parser Double
term = times <|> divide <|> factor

times :: Parser Double
times = do f <- factor
           do symbol "*"
              t <- term
              return (f * t)


divide :: Parser Double
divide = do f <- factor
            do symbol "/"
               t <- term
               return (f / t)


factor :: Parser Double
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> double




-- eval function is to handle invalid expr
eval :: String -> String
eval str = case parse expr str of
             [(a,[])]    -> show a
             [(_, rest)] -> "Ununsed Input: " ++ rest
             []          -> "Invalid Input"


