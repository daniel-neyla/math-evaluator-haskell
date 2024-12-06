module Exp where 

import Parser

data Exp = Num Float
          | Exp :+: Exp
          | Exp :-: Exp
          | Exp :*: Exp
          | Exp :/: Exp 
          | Exp :^: Exp
          | Neg Exp
          deriving (Eq)


par :: String -> String
par s = "(" ++ s ++ ")"

instance Show Exp where
  show (Num x)   = if x == 0 then "0.0" else show x

  show (x :+: y) = par (show x ++ "+" ++ show y)
  show (x :-: y) = par (show x ++ "-" ++ show y)
  show (x :*: y) = par (show x ++ "*" ++ show y)
  show (x :/: y) = par (show x ++ "/" ++ show y)
  show (x :^: y) = par (show x ++ "^" ++ show y)
  show (Neg x)   = "-" ++ show x



eval :: Exp -> Either String Float
eval (Num x)   = Right x
eval (x :+: y) = (+) <$> eval x <*> eval y
eval (x :-: y) = (-) <$> eval x <*> eval y
eval (x :*: y) = (*) <$> eval x <*> eval y
eval (x :/: y) = 
  case eval y of
  Right 0 -> Left "Error: Division by zero"
  Right d -> (/ d) <$> eval x
  Left err -> Left err
  
eval (x :^: y) = (**) <$> eval x <*> eval y
eval (Neg x)   = negate <$> eval x


test_tree :: Exp
test_tree = Neg ((Neg (Num 3)) :+: (Num 5)) :*: (Num 0.0)


-- parseExp :: Parser Exp
-- parseExp = parseNum <|> parsePlus <|> parseMinus <|> parseMul <|> parseDiv <|> parsePow
--   where
--     parseNum = do {
--       (Num . fromIntegral <$> parseInt) <|> (Num <$> parseFloat)

--     }
--     parsePlus = do {
--       token '(';
--       ex1 <- parseExp;
--       match " + "; 
--       ex2 <- parseExp;
--       token ')';
--       return (ex1 :+: ex2)
--     }
--     parseMinus = do {
--       token '(';
--       ex1 <- parseExp;
--       match " - ";
--       ex2 <- parseExp;
--       token ')';
--       return (ex1 :-: ex2)
--     }
--     parseMul = do {
--       token '(';
--       ex1 <- parseExp;
--       match " * "; 
--       ex2 <- parseExp;
--       token ')';
--       return (ex1 :*: ex2)
--     }
--     parseDiv = do {
--       token '(';
--       ex1 <- parseExp;
--       match " / "; 
--       ex2 <- parseExp;
--       token ')';
--       return (ex1 :/: ex2)
--     }
--     parsePow = do {
--       token '(';
--       ex1 <- parseExp;
--       match "^"; 
--       ex2 <- parseExp;
--       token ')';
--       return (ex1 :^: ex2)
--     }


parseExp :: Parser Exp
parseExp = parseAddSub
  where
    parseAddSub = parseTerm `chainl1` parseAddSubOp
    parseTerm = parseFactor `chainl1` parseMulDivOp
    parseFactor = parsePow <|> parseNum <|> parseParens

    parseAddSubOp = do { match " + "; return (:+:) }
               <|> do { match " - "; return (:-:) }
    parseMulDivOp = do { match " * "; return (:*:) }
               <|> do { match " / "; return (:/:) }

    -- Right-associative power operator
    parsePow = do
      base <- parseNum <|> parseParens
      rest base
      where
        rest base = (do
          match "^"
          exponent <- parsePow
          return (base :^: exponent))
          <|> return base

    parseNum = (Num . fromIntegral <$> parseInt) <|> (Num <$> parseFloat)

    parseParens = do
      token '('
      expr <- parseExp
      token ')'
      return expr



exp_roundtrip :: Exp -> Bool
exp_roundtrip exp  =  (parse parseExp (show exp) == exp)