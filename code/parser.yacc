
%%

%name Calc

%term EOF | TERM | CONST of bool | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | FI | LPAREN | RPAREN | ID of string | NUM of int | PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN | LET | IN | END | EQ | Fn | Fun | INT | BOOL | COLON | ARROW | EQARROW

%nonterm start of AST.exp list | program of AST.exp list | statement of AST.exp | formula of AST.exp | DECL of AST.decl | TYP of AST.typ | TYPE of AST.typ | Separator of AST.exp

%pos int

%eop EOF
%noshift EOF

%right EQARROW Fn Fun ARROW

%right IF THEN ELSE FI LET IN END
%left IMPLIES 
%left AND OR XOR EQUALS
%left NOT

%nonassoc LESSTHAN GREATERTHAN

%left MINUS PLUS
%left TIMES 
%left NEGATE

%start start

%verbose

%%

start: program (program)

program : statement ([statement])
		| program TERM statement (program @ [statement])

statement : Separator (Separator)

DECL: ID EQ formula (AST.ValDecl(ID, formula))

TYP: TYPE (TYPE)
	| TYP ARROW TYP (AST.ARROW(TYP, TYP))
	| LPAREN TYP RPAREN (TYP)

TYPE: INT (AST.INT)
	| BOOL (AST.BOOL)

Separator : Fun ID LPAREN ID COLON TYP RPAREN COLON TYP EQARROW formula (AST.Fun(ID1, ID2, TYP1, TYP2, formula))
		| formula (formula)
	
formula : CONST (AST.BoolExp(CONST))
		| ID (AST.VarExp(ID))
		| NUM (AST.NumExp(NUM))
		| NOT formula (AST.UniExp(AST.Not, formula))
		| formula AND formula (AST.BinExp(AST.And, formula1,  formula2))
		| formula OR formula (AST.BinExp(AST.Or, formula1,  formula2))
		| formula XOR formula (AST.BinExp(AST.Xor, formula1,  formula2))
		| formula EQUALS formula (AST.BinExp(AST.Equals, formula1, formula2))
		| formula IMPLIES formula (AST.BinExp(AST.Implies, formula1, formula2))
		| IF formula THEN formula ELSE formula FI (AST.IfExp(formula1, formula2, formula3))
		| LET DECL IN formula END (AST.LetExp(DECL, formula))
		| LPAREN formula RPAREN (formula)
		| NEGATE formula (AST.UniExp(AST.Negate, formula))
		| formula PLUS formula (AST.BinExp(AST.Plus, formula1,  formula2))
		| formula MINUS formula (AST.BinExp(AST.Minus, formula1, formula2))
		| formula TIMES formula (AST.BinExp(AST.Times, formula1, formula2))
		| formula LESSTHAN formula (AST.BinExp(AST.LessThan, formula1, formula2))
		| formula GREATERTHAN formula (AST.BinExp(AST.GreaterThan, formula1, formula2))
		| Fn LPAREN ID COLON TYP RPAREN COLON TYP EQARROW formula (AST.Fn(ID, TYP1, TYP2, formula))
		| LPAREN formula formula RPAREN (AST.AppExp(formula1, formula2))

