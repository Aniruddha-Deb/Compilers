(* implementing a recursive descent calculator in SML
Grammar:
E : E+T | E-T | T        
T : T*D | T/D | T%D | D
D : int | (E)

removing left recursion and wrapping grammar for LL(1):
S : E$
E : TE'
E': +TE' | -TE' | eps
T : DT'
T': *DT' | /DT' | %DT' | eps
D : (E) | int

computing first and follow:
first(E)  = LPAREN | INT  
first(E') = PLUS | MINUS | eps 
first(T)  = LPAREN | INT
first(T') = MUL | DIV | MOD | eps
first(D)  = LPAREN | int

follow(E)  = RPAREN | $
follow(E') = RPAREN | $
follow(T)  = RPAREN | PLUS | MINUS | $
follow(T') = RPAREN | PLUS | MINUS | $
follow(D)  = RPAREN | PLUS | MINUS | DIV | SUB | MUL | $
*)

datatype Token = LPAREN | RPAREN | ADD | SUB | MUL | DIV | MOD | NUM of int;

exception LexerException;
exception ParserException;

fun tokNum num (c::S) = if Char.isDigit(c) then tokNum (num*10+(ord(c)-ord(#"0"))) S else (num,(c::S))
  | tokNum num [] = (num, []);

fun tokenize [] = []
  | tokenize (#"("::S) = LPAREN::(tokenize S)
  | tokenize (#")"::S) = RPAREN::(tokenize S)
  | tokenize (#"+"::S) = ADD::(tokenize S)
  | tokenize (#"-"::S) = SUB::(tokenize S)
  | tokenize (#"*"::S) = MUL::(tokenize S)
  | tokenize (#"/"::S) = DIV::(tokenize S)
  | tokenize (#"%"::S) = MOD::(tokenize S)
  | tokenize (c::S) = 
if Char.isSpace(c) then tokenize S 
else if Char.isDigit(c) then
	let 
		val (num, rest) = tokNum 0 (c::S)
	in
		(NUM num) :: (tokenize rest)
	end
else 
	raise LexerException;

fun parse expr = 
let
	fun D ans ((NUM n)::L) = (n, L)
	  | D ans (LPAREN::L) = E ans L
	  | D ans L = raise ParserException
	
	and T' ans (MUL::L) = let val (ans',L') = D 0 L in E' (ans*ans') L' end
	  | T' ans (DIV::L) = let val (ans',L') = D 0 L in E' (ans div ans') L' end
	  | T' ans (MOD::L) = let val (ans',L') = D 0 L in E' (ans mod ans') L' end
	  | T' ans (ADD::L) = (ans,(ADD::L))
	  | T' ans (SUB::L) = (ans,(ADD::L))
	  | T' ans [] = (ans, [])
	  | T' ans L = raise ParserException
	
	and T ans (LPAREN::L) = let val (ans',(RPAREN::L')) = D ans L in T' ans' L' end
	  | T ans ((NUM n)::L) = let val (ans',L') = D ans ((NUM n)::L) in T' ans' L' end
	  | T ans L = raise ParserException
	
	and E' ans (ADD::L) = let val (ans',L') = T 0 L in E' (ans+ans') L' end
	  | E' ans (SUB::L) = let val (ans',L') = T 0 L in E' (ans-ans') L' end
	  | E' ans [] = (ans, [])
	  | E' ans L = raise ParserException
	
	and E ans (LPAREN::L) = let val (ans',(RPAREN::L')) = T ans L in E' ans' L' end
	  | E ans ((NUM n)::L) = let val (ans',L') = T ans ((NUM n)::L) in E' ans' L' end
	  | E ans L = raise ParserException
		
	and S ans [] = (ans, [])
	  | S ans (LPAREN::L) = E 0 (LPAREN::L)
	  | S ans ((NUM n)::L) = E 0 ((NUM n)::L)
	  | S ans L = raise ParserException;
in
	S 0 expr
end

fun eval expr =
let
	val chars = String.explode expr
	val tokens = tokenize chars
	val (ans,[]) = parse tokens
in
	ans
end
