functor CalcLexFun(structure Tokens:Calc_TOKENS)=
   struct
    structure UserDeclarations =
      struct
structure Tokens = Tokens
 
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token

val pos = ref 0
val col = ref 0
val output = ref("[") 

exception UnknownToken

val eof = fn () => (let 
						val row = !pos;
						val result = String.substring(!output, 0, String.size(!output) - 2) ^ "]" ^ "\n"
					in 
						output := "[";
						col := 0;
						pos := 0;
                    	TextIO.output(TextIO.stdOut, result ); 
                    	Tokens.EOF(row, row)
                    end)
val error = fn (e, l:int, _) => (TextIO.output(TextIO.stdOut, e); raise UnknownToken)

fun revfold _ nil b = b
| revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\016\018\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\016\003\003\003\003\003\003\003\015\014\003\003\003\012\003\003\
\\010\010\010\010\010\010\010\010\010\010\009\008\003\006\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\017\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 28)], trans = 0},
{fin = [(N 18),(N 28)], trans = 4},
{fin = [(N 18)], trans = 4},
{fin = [(N 12),(N 28)], trans = 6},
{fin = [(N 26)], trans = 0},
{fin = [(N 6),(N 28)], trans = 0},
{fin = [(N 20),(N 28)], trans = 0},
{fin = [(N 15),(N 18),(N 28)], trans = 10},
{fin = [(N 15),(N 18)], trans = 10},
{fin = [(N 28)], trans = 12},
{fin = [(N 23)], trans = 0},
{fin = [(N 10),(N 28)], trans = 0},
{fin = [(N 8),(N 28)], trans = 0},
{fin = [(N 4),(N 28)], trans = 16},
{fin = [(N 4)], trans = 16},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (col := !col + 1; lex())
| 10 => (col := !col + 1; output := !output ^ "RPAREN " ^ "\"" ^ ")" ^ "\"" ^ ", ";  Tokens.RPAREN(!pos,!pos) )
| 12 => (col := !col + 1; output := !output ^ "EQ " ^ "\"" ^ "=" ^ "\"" ^ ", ";  Tokens.EQ(!pos,!pos) )
| 15 => let val yytext=yymktext() in col := !col + String.size(yytext); output := !output ^ "NUM " ^ "\"" ^ yytext ^ "\"" ^ ", ";
			Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!pos, !pos) end
| 18 => let val yytext=yymktext() in 
if (yytext = "TRUE") then (col := !col + 4; output := !output ^ "CONST " ^ "\"" ^ "TRUE" ^ "\"" ^ ", "; Tokens.CONST(true, !pos,!pos) )
else if (yytext = "FALSE") then (col := !col + 5; output := !output ^ "CONST " ^ "\"" ^ "FALSE" ^ "\"" ^ ", "; Tokens.CONST(false, !pos,!pos) )
else if (yytext = "AND") then (col := !col + 3; output := !output ^ "AND " ^ "\"" ^ "AND" ^ "\"" ^ ", "; Tokens.AND(!pos,!pos) )
else if (yytext = "OR") then (col := !col + 2; output := !output ^ "OR " ^ "\"" ^ "OR" ^ "\"" ^ ", "; Tokens.OR(!pos,!pos) )
else if (yytext = "XOR") then (col := !col + 3; output := !output ^ "XOR " ^ "\"" ^ "XOR" ^ "\"" ^ ", "; Tokens.XOR(!pos,!pos) )
else if (yytext = "EQUALS") then (col := !col + 6; 
							output := !output ^ "EQUALS " ^ "\"" ^ "EQUALS" ^ "\"" ^ ", "; 
							Tokens.EQUALS(!pos,!pos))
else if (yytext = "IMPLIES") then (col := !col + 7; 
							output := !output ^ "IMPLIES " ^ "\"" ^ "IMPLIES" ^ "\"" ^ ", "; 
							Tokens.IMPLIES(!pos,!pos))
else if (yytext = "NOT") then (col := !col + 3; output := !output ^ "NOT " ^ "\"" ^ "NOT" ^ "\"" ^ ", "; Tokens.NOT(!pos,!pos) )

else if (yytext = "if") then (col := !col + 2; output := !output ^ "IF " ^ "\"" ^ "if" ^ "\"" ^ ", "; Tokens.IF(!pos,!pos) )
else if (yytext = "then") then (col := !col + 4; output := !output ^ "THEN " ^ "\"" ^ "then" ^ "\"" ^ ", "; Tokens.THEN(!pos,!pos))
else if (yytext = "else") then (col := !col + 4; output := !output ^ "ELSE " ^ "\"" ^ "else" ^ "\"" ^ ", "; Tokens.ELSE(!pos,!pos))
else if (yytext = "fi") then (col := !col + 2; output := !output ^ "FI " ^ "\"" ^ "fi" ^ "\"" ^ ", "; Tokens.FI(!pos,!pos))

else if (yytext = "let") then (col := !col + 3; output := !output ^ "LET " ^ "\"" ^ "let" ^ "\"" ^ ", "; Tokens.LET(!pos,!pos))
else if (yytext = "in") then (col := !col + 2; output := !output ^ "IN " ^ "\"" ^ "in" ^ "\"" ^ ", "; Tokens.IN(!pos,!pos))
else if (yytext = "end") then (col := !col + 3; output := !output ^ "END " ^ "\"" ^ "end" ^ "\"" ^ ", "; Tokens.END(!pos,!pos))

else if (yytext = "PLUS") then (col := !col + 4; output := !output ^ "PLUS " ^ "\"" ^ "PLUS" ^ "\"" ^ ", "; Tokens.PLUS(!pos,!pos))
else if (yytext = "MINUS") then (col := !col + 5; output := !output ^ "MINUS " ^ "\"" ^ "MINUS" ^ "\"" ^ ", "; Tokens.MINUS(!pos,!pos))
else if (yytext = "TIMES") then (col := !col + 5; output := !output ^ "TIMES " ^ "\"" ^ "TIMES" ^ "\"" ^ ", "; Tokens.TIMES(!pos,!pos))
else if (yytext = "NEGATE") then (col := !col + 6; 
								output := !output ^ "NEGATE " ^ "\"" ^ "NEGATE" ^ "\"" ^ ", "; 
								Tokens.NEGATE(!pos,!pos))
else if (yytext = "LESSTHAN") then (col := !col + 8; 
								output := !output ^ "LESSTHAN " ^ "\"" ^ "LESSTHAN" ^ "\"" ^ ", ";
								Tokens.LESSTHAN(!pos,!pos))
else if (yytext = "GREATERTHAN") then (col := !col + 11; 
								output := !output ^ "GREATERTHAN " ^ "\"" ^ "GREATERTHAN" ^ "\"" ^ ", "; 
								Tokens.GREATERTHAN(!pos,!pos))

else if (yytext = "fn") then (col := !col + 2; 
								output := !output ^ "Fn " ^ "\"" ^ "fn" ^ "\"" ^ ", "; 
								Tokens.Fn(!pos,!pos))
else if (yytext = "fun") then (col := !col + 3; 
								output := !output ^ "Fun " ^ "\"" ^ "fun" ^ "\"" ^ ", "; 
								Tokens.Fun(!pos,!pos))
else if (yytext = "int") then (col := !col + 3; 
								output := !output ^ "INT " ^ "\"" ^ "int" ^ "\"" ^ ", "; 
								Tokens.INT(!pos,!pos))
else if (yytext = "bool") then (col := !col + 4; 
								output := !output ^ "BOOL " ^ "\"" ^ "bool" ^ "\"" ^ ", "; 
								Tokens.BOOL(!pos,!pos))

else (col := !col + String.size(yytext); output := !output ^ "ID " ^ "\"" ^ yytext ^ "\"" ^ ", "; Tokens.ID(yytext,!pos,!pos) )
 end
| 20 => (col := !col + 1; 
			output := !output ^ "COLON " ^ "\"" ^ ":" ^ "\"" ^ ", "; 
			Tokens.COLON(!pos,!pos))
| 23 => (col := !col + 2; 
			output := !output ^ "ARROW " ^ "\"" ^ "->" ^ "\"" ^ ", "; 
			Tokens.ARROW(!pos,!pos))
| 26 => (col := !col + 2; 
			output := !output ^ "EQARROW " ^ "\"" ^ "=>" ^ "\"" ^ ", "; 
			Tokens.EQARROW(!pos,!pos))
| 28 => let val yytext=yymktext() in let
				val row = !pos + 1; val column = !col + 1;
			in 
				output := "[";
				col := 0;
		    	error ("Unknown Token:" ^ (Int.toString row) ^ ":" ^ (Int.toString column) ^ ":" ^ yytext, !pos, !pos);
		    	pos := 0;
                lex() 
            end  end
| 4 => let val yytext=yymktext() in col := !col + String.size(yytext); lex() end
| 6 => (col := 0; pos := (!pos) + 1; output := !output ^ "TERM " ^ "\"" ^ ";" ^ "\"" ^ ", ";  Tokens.TERM(!pos,!pos) )
| 8 => (col := !col + 1; output := !output ^ "LPAREN " ^ "\"" ^ "(" ^ "\"" ^ ", ";  Tokens.LPAREN(!pos,!pos) )
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
