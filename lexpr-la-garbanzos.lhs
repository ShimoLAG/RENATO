> import MonadicParsing

Example of logical Expression:

T for True
F for False
| for logical or
& for logical and
! For logical not




Concrete Syntax        Abstract Syntax
————————————————————————————————-
	“T | F & T" =      or (LVal True) (And (LVal False) (LVal True)


Expression Tree 




“F”  =  Lval False
“T”  =  Lval True
“T”|”F” =  or (LVal True) (LVal False)
“F & T”  =  and (LVal False) (LVal True)


> data LExp
>	= Lval Bool
>	| Not LExp
>	| And LExp LExp  
>	| Or LExp LExp
> 	deriving Show

Write A parser named parse_lexp given a logical expression as string and produces the abstract syntax 
of the expression using the defined data structure of logical expression.

Requirements:
1. Define the grammar for the unambiguous logical expression involving values T for true and F for False
2. Implement the parsers based on the grammar
3. Write the parser named parse_lexp to use the logical expression parsers with the type signature:

	parse_lexp :: String -> LExp

