> module MonadicParsing where

> newtype Parser a = Parser (String -> [(a, String)])

double x = x + x

double = \x.x+x

double = \x -> x + x

to use it:

double 2 = (\x -> x + x) 2 ==> 2 + 2 = 4

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

f = \c -> g

p =>> f ==> p =>> \c -> g ==> (\c -> g) p


 instance MonadPlus Parser where
    mzero        =  Parser (\inp -> [])
    p `mplus` q  =  Parser (\inp -> case parse p inp of
                                       []        -> parse q inp
                                       [(v,out)] -> [(v,out)])

> instance MonadZero Parser where
>   zero = Parser (\cs -> [])

> instance MonadPlus Parser where
>   p <+> q = Parser (\cs -> case parse p cs of
>                               []         -> parse q cs
>                               [(v, out)] -> [(v, out)])

Choice operator

> (<|>)  :: Parser a -> Parser a -> Parser a
> p <|> q =  p <+> q

Sequence monad but ignores the result for next function

> (->>)  :: Parser a -> Parser b -> Parser b
> p ->> q = p =>> \_ -> q

 (+++) :: Parser a -> Parser a -> Parser a
 p +++ q = Parser (\cs -> case parse (p ++ q) cs of
                             [] -> []
                             (x:xs) -> [x])

 sat :: (Char -> Bool) -> Parser Char
 sat p = do {c <- item; if p c then ret c else zero}

> sat  :: (Char -> Bool) -> Parser Char
> sat p = item =>> \c -> if p c then ret c else zero

> char :: Char -> Parser Char
> char c = sat (c ==)

 string :: String -> Parser String
 string "" = return ""
 string (c:cs) = do {char c; string cs; return (c:cs)}

> string :: String -> Parser String
> string "" = ret ""
> string (c:cs) = char c =>> \_ -> string cs =>> \_ -> ret (c:cs)

> many :: Parser a -> Parser [a]
> many p = many1 p <|> ret []

 many1 :: Parser a -> Parser [a]
 many1 p = do {a <- p; as <- many p; return (a:as)}

> many1 :: Parser a -> Parser [a]
> many1 p = p =>> \a -> many p =>> \as -> ret (a:as)

 sepby :: Parser a -> Parser b -> Parser [a]
 sepby p sep = (sepby1 p sep) <|> ret []

 sepby1 :: Parser a -> Parser b -> Parser [a]
 sepby1 p sep = do {a <- p; as <- many (do {sep; p}); return (a:as)}

 chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
 chainl p op a = (chainl1 p op) <|> ret a

 chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
 chainl1 p op = do {a <- p; rest a}
                where
                  rest a = (do {f <- op; b <- p; rest (f a b)})

> chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
> chainl1 p op = p =>> \a -> rest =>> \a -> op =>> \f -> p =>> \b -> rest (f a b)  



 chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
 chainl1 p op = ?

> isSpace :: Char -> Bool
> isSpace c = c == ' '

> space :: Parser String
> space = many (sat isSpace)

 token :: Parser a -> Parser a
 token p = do {space; a <- p; space; return a}

> token :: Parser a -> Parser a
> token p = space ->> p =>> \a -> space ->> ret a

> symb :: String -> Parser String
> symb cs = token (string cs)

 apply :: Parser a -> String -> [(a, String)]
 apply p = parse (do {space; p}) 

> apply :: Parser a -> String -> [(a, String)]
> apply p = parse (space =>> \_ -> p) 

