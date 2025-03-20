import MonadicParsing

-- Define data structure
data LExp
  = LVal Bool
  | Not LExp
  | And LExp LExp
  | Or LExp LExp
  deriving Show

-- Parsers for true/false literals
lvalParser :: Parser LExp
lvalParser = (char 'T' ->> ret (LVal True))
         <|> (char 'F' ->> ret (LVal False))

-- Parser for NOT expressions
notParser :: Parser LExp
notParser = (char '!' ->> parseLExp) =>> \e -> ret (Not e)

-- Parsers for AND and OR operators
andParser :: Parser (LExp -> LExp -> LExp)
andParser = (char '&' ->> ret And)

orParser :: Parser (LExp -> LExp -> LExp)
orParser = (char '|' ->> ret Or)

-- Full expression parser combining everything
parseLExp :: Parser LExp
parseLExp = chainl1 term (orParser <|> andParser)
  where
    term = lvalParser <|> notParser

-- Convert parser to the requested type
parse_lexp :: String -> LExp
parse_lexp s = case apply parseLExp s of
  [(exp, "")] -> exp
  _           -> error "Invalid expression!"
