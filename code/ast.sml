structure AST =
struct

type id = string

datatype typ = Unit | BOOL | INT | ARROW of typ * typ

datatype binop = Plus | Minus | Times | Equals | GreaterThan | LessThan | And | Or | Xor | Implies

datatype uniop = Not | Negate

datatype decl = ValDecl of id * exp

and exp = NumExp of int
		| StringExp of string
    	| VarExp of id
    	| BoolExp of bool
		| BinExp of binop * exp * exp 
		| UniExp of uniop * exp
		| LetExp of decl * exp
		| IfExp of exp * exp * exp
		| AppExp of exp * exp
		| Fn of id * typ * typ * exp
        | Fun of id * id * typ * typ * exp

and value = IntVal of int
		| StringVal of string
        | BoolVal of bool
        | FuncVal of id * exp * (id * value) list

type environment = (id * value) list

type typEnvironment = (id * typ) list

fun envAdd (var:id, v:value, env:environment) = (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
		SOME (x, v) => v
		| NONE => raise Fail ("Value Environment lookup error: " ^ var ^ " not found")			

fun add (var:id, t:typ, env:typEnvironment) = (var,t)::env

fun lookup (var:id, env:typEnvironment) =
    case List.find(fn (x, _) => x = var) env of
    SOME (x, v) => v
    | NONE => raise Fail ("Type Environment lookup error: " ^ var ^ " not found")
                   
end


