datatype mtree = empty | node of int * mtree list;

fun mtree_to_string empty = ""
  | mtree_to_string (node (n,L)) = 
let
    fun list_subtrees s [] = s ^ "]"
      | list_subtrees s (N::[]) = list_subtrees (s ^ (mtree_to_string N)) []
      | list_subtrees s (N::L) = list_subtrees (s ^ (mtree_to_string N) ^ ",") L
in
    ((Int.toString(n)) ^ (list_subtrees "[" L))
end

datatype token = LBKT | RBKT | INT of int | COMMA;

exception LexerException;

fun tokNum num sgn (#"-"::S) = tokNum num ~1 S
  | tokNum num sgn (c::S) = if Char.isDigit(c) then tokNum (num*10+(ord(c)-ord(#"0"))) sgn S else (num*sgn,(c::S))
  | tokNum num sgn [] = (num*sgn, []);

fun tokenize [] = []
  | tokenize (#"["::S) = LBKT::(tokenize S)
  | tokenize (#"]"::S) = RBKT::(tokenize S)
  | tokenize (#","::S) = COMMA::(tokenize S)
  | tokenize (c::S) = 
if Char.isSpace(c) then tokenize S 
else if Char.isDigit(c) orelse (c = #"-") then
	let 
		val (num, rest) = tokNum 0 1 (c::S)
	in
		(INT num) :: (tokenize rest)
	end
else 
	raise LexerException;

(* use a recursive descent parser with the fol. grammar:
S: INT SL | eps
SL: [SK
K: ] | ,SK

first(S) = INT | eps
first(SL) = LBKT
first(K) = COMMA | RBKT

follow(S) = COMMA | RBKT | $
follow(SL) = $
follow(K) = $
*)

fun parse s =
let
    fun S t ((INT n)::T) = let val (L,T') = SL t T in (node (n,L), T') end
      | S t (RBKT::T) = (empty, (RBKT::T))
      | S t [] = (t,[])

    and SL t (LBKT::T) = let val (N, T') = S t T val (L,T'') = K [N] T' in (L, T'') end
    
    and K (L : mtree list) (COMMA::T) = let val (N, T') = S empty T val (L',T'') = K (N::L) T' in (L', T'') end
      | K (L : mtree list) (RBKT::T) = (L, T)

    val tokens = tokenize (String.explode s)
    val (ans, []) = S empty tokens
in
    ans
end
