module AST where 

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
