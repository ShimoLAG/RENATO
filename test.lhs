import MonadicParsing

-- Define the data structure for logical expressions
data LExp
  = LVal Bool
  | Not LExp
  | And LExp LExp
  | Or LExp LExp
  deriving Show

-- Parse 'T' as True and 'F' as False
lval :: Parser LExp
lval = (char 'T' ->> ret (LVal True))
   <|> (char 'F' ->> ret (LVal False))

-- Parse parentheses (e.g., (T | F))
parens :: Parser LExp
parens = do
  _ <- char '('
  e <- lexp
  _ <- char ')'
  ret e

-- Parse Not (!e)
notExp :: Parser LExp
notExp = do
  _ <- char '!'
  e <- factor
  ret (Not e)

-- Factor: handles literals, not expressions, and parentheses
factor :: Parser LExp
factor = lval <|> notExp <|> parens

-- Parse conjunction (e1 & e2)
andExp :: Parser LExp
andExp = chainl1 factor (char '&' ->> ret And)

-- Parse disjunction (e1 | e2)
orExp :: Parser LExp
orExp = chainl1 andExp (char '|' ->> ret Or)

-- Parse the full logical expression
lexp :: Parser LExp
lexp = orExp

-- Wrap with space handling
parse_lexp :: String -> LExp
parse_lexp inp = case apply lexp inp of
  [(res, "")] -> res
  _           -> error "Invalid expression"
