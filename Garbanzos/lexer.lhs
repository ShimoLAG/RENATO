> import MonadicParsing

digit = '0' | '1' | '2' | '3' | .. | '9'

> isMember :: Char -> [Char] -> Bool
> isMember _ [] = False
> isMember c (x:xs)
>   | c == x    = True
>   | otherwise = isMember c xs

> isDigit :: Char -> Bool
> isDigit c = isMember c ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

> digit :: Parser Char
> digit = sat (isDigit)

Grammar for natural numbers
---------------------------

nat = digit {digit}
  or
nat = digit+
  
nat = many1 digit

----------------
Sequential execution

p1 >>= \a1 ->
p2 >>= \a2 ->
...
pn >>= \an ->
f a1 a2 .. an

or

p1 >>= \a1 -> p2 >>= \a2 -> ... pn >>= \an -> f a1 a2 .. an

p1 >>= \a1.p2 >>= \a2.p3 >>= .. pn >>= \an.f a1 a2 .. an


(\a1.p2) p1

one :: Int

inc :: Int -> Int

Parser for natural number
-------------------------

> nat :: Parser Int
> nat  = many1 digit =>> \xs -> ret (read xs)

         do {xs <- many1 digit; ret (read xs)}

> natural :: Parser Int
> natural = token (nat)

Grammar for Arithmetic Expression
---------------------------------

expr ::= term '+' expr 
       | term '-' expr
       | term
       
term ::= factor '*' term
       | factor
       
factor ::= '(' expr ')'
         | number

number ::= nat

nat ::= digit {digit} # one or more digit (many1)

nat ::= digit+

Evaluator for Arithmetic Expression
-----------------------------------

> expr :: Parser Int

expr = do {t <- term; symb "+"; e <- expr; ret (t + e)} 
   <|> do {t <- term; symb "-"; e <- expr; ret (t - e)}
   <|> term

> expr = (term =>> \t -> symb "+" =>> \_ -> expr =>> \e -> ret (t + e)) 
>    <|> (term =>> \t -> symb "-" =>> \_ -> expr =>> \e -> ret (t - e))
>    <|> term

> term :: Parser Int
> term = (factor =>> \f -> symb "*" =>> \_ -> term =>> \t -> ret (f * t)) <|> factor

> factor :: Parser Int
> factor = (symb "(" =>> \_ -> expr =>> \e -> symb ")" =>> \_ -> ret e) <|> (natural =>> \n -> ret n)

> number :: Parser Int
> number = natural =>> \n -> ret n


> calc :: String -> Int
> calc expstr = 
>   case parse expr expstr of
>     [(res,"")]  -> res
>     [(res,str)] -> error ("unused input: " ++ str)
>     []          -> error ("invalid input: " ++ expstr)


1 + 2 + 3 + 4

5 * 4 * 6 * ..

5 - 3 - 5 - 7 



