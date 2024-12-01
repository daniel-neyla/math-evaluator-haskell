module Parser where

import Data.Char
import Control.Applicative
import Control.Monad

-- Type parser
newtype Parser a = Parser (String -> Either String (a, String))

-- Apply a parser
apply :: Parser a -> String -> Either String (a, String)
apply (Parser f) s = f s

-- Return parsed value
parse :: Parser a -> String -> a
parse p s = case apply p s of
  Right (result, "") -> result -- success
  Right (_, rest)    -> error $ "Unconsumed input: " ++ rest
  Left err           -> error err

-- Define Monad instance to chain parsers
instance Monad Parser where 
  return x = Parser (\s -> Right (x, s)) -- Wraps value into the Parser monad
  m >>= k  = Parser (\s -> 
        case apply m s of
          Left err      -> Left err
          Right (x, s') -> apply (k x) s') -- Passes result into the next Parser



-- *******************************************************
-- The following instance declarations for Functor and Applicative
-- are required because of a change to the way that Monad
-- is defined in Haskell 7.10+.
-- see https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
-- and https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10
-- *******************************************************


instance Functor Parser where
  fmap = liftM
 
instance Applicative Parser where
  pure    = return
  (<*>)   = ap

instance MonadPlus Parser where
  mzero        =  Parser (\_ -> Left "No parse") -- Failed, no result
  m `mplus` n  =  Parser (\s ->
    case apply m s of
      Left _ -> apply n s -- If m fails, try n
      success -> success  -- If m succeeds, return its result   
    )


-- *******************************************************
-- The following instance declaration for Applicative, and a change
-- to the instance declaration for MonadPlus, are required because
-- of a change to the way that MonadPlus is defined in Haskell 7.10+.
-- see https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
-- and https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10
-- *******************************************************

instance Alternative Parser where
  empty  =  mzero
  (<|>)  =  mplus 

-- Parse one character
char :: Parser Char
char =  Parser f
  where
  f []     =  Left "Unexpected end of input"
  f (c:s)  =  Right (c,s)

-- guard :: MonadPlus m => Bool -> m ()
-- guard False  =  mzero
-- guard True   =  return ()

-- Parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p  =  do 
  
  guard (p c) -- Fails with mzero if predicate is false
  return c 

-- Match a given character
token :: Char -> Parser Char
token c  =  spot (== c)

-- Perform a list of commands, returning a list of values
-- sequence :: Monad m => [m a] -> m [a]
-- sequence []
-- sequence (m:ms)  =  do {
--                       x <- m;
--                       xs <- sequence ms;
--                       return (x:xs)
--                     }

-- match a given string
match :: String -> Parser String
match xs  =  sequence (map token xs)

-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p  =  plus p `mplus` return []

-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p  =  do x <- p
              xs <- star p
              return (x:xs)

-- match a natural number
parseNat :: Parser Int
parseNat =  do s <- plus (spot isDigit)
               return (read s)

-- match a negative number
parseNeg :: Parser Int
parseNeg =  do token '-'
               n <- parseNat
               return (-n)

-- match an integer
parseInt :: Parser Int
parseInt =  parseNat `mplus` parseNeg
