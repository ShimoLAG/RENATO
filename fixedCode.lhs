> module MonadicParsing where

> newtype Parser a = Parser (String -> [(a, String)])

> item :: Parser Char
> item = Parser (\cs -> case cs of
>                         ""     -> []
>                         (x:xs) -> [(x,xs)])

> parse :: Parser a -> String -> [(a,String)]
> parse (Parser p) inp =  p inp

> class MyMonad m where
>   ret    :: a -> m a
>   (=>>)  :: m a -> (a -> m b) -> m b

> class MyMonad m => MonadZero m where
>   zero :: m a

> class MonadZero m => MonadPlus m where
>   (<+>) :: m a -> m a -> m a

> instance MyMonad Parser where
>   ret a    = Parser (\cs -> [(a, cs)])
>   p =>> f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

> instance MonadZero Parser where
>   zero = Parser (\cs -> [])

> instance MonadPlus Parser where
>   p <+> q = Parser (\cs -> case parse p cs of
>                               []         -> parse q cs
>                               [(v, out)] -> [(v, out)])

> (<|>)  :: Parser a -> Parser a -> Parser a
> p <|> q =  p <+> q

> (->>)  :: Parser a -> Parser b -> Parser b
> p ->> q = p =>> \_ -> q

> sat  :: (Char -> Bool) -> Parser Char
> sat p = item =>> \c -> if p c then ret c else zero

> digit :: Parser Char
> digit = sat (\c -> c >= '0' && c <= '9')

> char :: Char -> Parser Char
> char c = sat (== c)

> string :: String -> Parser String
> string "" = ret ""
> string (c:cs) = char c =>> \_ -> string cs =>> \_ -> ret (c:cs)

> many :: Parser a -> Parser [a]
> many p = many1 p <|> ret []

> many1 :: Parser a -> Parser [a]
> many1 p = p =>> \a -> many p =>> \as -> ret (a:as)

> sepby :: Parser a -> Parser b -> Parser [a]
> sepby p sep = sepby1 p sep <|> ret []

> sepby1 :: Parser a -> Parser b -> Parser [a]
> sepby1 p sep = p =>> \a -> many (sep ->> p) =>> \as -> ret (a:as)

> chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
> chainl p op a = chainl1 p op <|> ret a

> chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
> chainl1 p op = p =>> rest
>   where
>     rest a = (op =>> \f -> p =>> \b -> rest (f a b)) <|> ret a

> isSpace :: Char -> Bool
> isSpace c = c == ' '

> space :: Parser String
> space = many (sat isSpace)

> token :: Parser a -> Parser a
> token p = space ->> p =>> \a -> space ->> ret a

> symb :: String -> Parser String
> symb cs = token (string cs)

> apply :: Parser a -> String -> [(a, String)]
> apply p = parse (space ->> p)

> nat :: Parser Int
> nat = many1 digit =>> \xs -> ret (read xs)

> addop :: Parser (Int -> Int -> Int)
> addop = (symb "+" ->> ret (+)) <+> (symb "-" ->> ret (-))

> expr :: Parser Int
> expr = chainl1 nat addop
