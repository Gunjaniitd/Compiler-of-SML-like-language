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
  
%%

%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
alpha_digit = [A-Za-z0-9];
ws = [\ \t];

%%

\n       => (col := !col + 1; lex());
{ws}+    => (col := !col + String.size(yytext); lex());

";"      => (col := 0; pos := (!pos) + 1; output := !output ^ "TERM " ^ "\"" ^ ";" ^ "\"" ^ ", ";  Tokens.TERM(!pos,!pos) );
"("      => (col := !col + 1; output := !output ^ "LPAREN " ^ "\"" ^ "(" ^ "\"" ^ ", ";  Tokens.LPAREN(!pos,!pos) );
")"      => (col := !col + 1; output := !output ^ "RPAREN " ^ "\"" ^ ")" ^ "\"" ^ ", ";  Tokens.RPAREN(!pos,!pos) );
"="      => (col := !col + 1; output := !output ^ "EQ " ^ "\"" ^ "=" ^ "\"" ^ ", ";  Tokens.EQ(!pos,!pos) );

{digit}+ => (col := !col + String.size(yytext); output := !output ^ "NUM " ^ "\"" ^ yytext ^ "\"" ^ ", ";
			Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!pos, !pos));

{alpha_digit}+ => (
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
);

":"    => (col := !col + 1; 
			output := !output ^ "COLON " ^ "\"" ^ ":" ^ "\"" ^ ", "; 
			Tokens.COLON(!pos,!pos));
"->"   => (col := !col + 2; 
			output := !output ^ "ARROW " ^ "\"" ^ "->" ^ "\"" ^ ", "; 
			Tokens.ARROW(!pos,!pos));
"=>"   => (col := !col + 2; 
			output := !output ^ "EQARROW " ^ "\"" ^ "=>" ^ "\"" ^ ", "; 
			Tokens.EQARROW(!pos,!pos));
			
.      => (let
				val row = !pos + 1; val column = !col + 1;
			in 
				output := "[";
				col := 0;
		    	error ("Unknown Token:" ^ (Int.toString row) ^ ":" ^ (Int.toString column) ^ ":" ^ yytext, !pos, !pos);
		    	pos := 0;
                lex() 
            end );
