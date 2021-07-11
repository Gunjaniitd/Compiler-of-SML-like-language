structure TYPECHECKER  =
struct
open AST

fun typCheckExp(e:exp, env:typEnvironment): typ =
    case e of
	    NumExp i => INT
      | VarExp x => lookup (x, env) 			
      | BoolExp b => BOOL 
      | BinExp (b, e1, e2)  => typCheckBinExp(b, e1, e2, env)
      | UniExp (u, e) => typCheckUniExp(u, e, env)
      | LetExp(ValDecl(x, e1), e2) => let
                                  	    val v1 = case e1 of 
                                          Fn (y, s1, s2, expr) => typCheckExp (e1, add(x, ARROW(s1, s2), env))
                                          | _ => typCheckExp (e1, env)
                                      in
                                  	    typCheckExp(e2, add (x, v1, env))
                                      end	  
      | Fn (x, s1, s2, expr) => if typCheckExp(expr, add(x, s1, env)) = s2 then ARROW(s1, s2) 
      							else raise  Fail "Error in Type Checking: Expression of Fn does not match required type"
      | Fun (x, y, s1, s2, expr) => if typCheckExp(expr, add(x, ARROW(s1, s2), add(y, s1, env))) = s2 then lookup(x, add(x, ARROW(s1, s2), env)) 
      								else raise Fail "Error in Type Checking: Expression of Fun does not match required type"
      | IfExp (e1, e2, e3) => let
                                val t = typCheckExp (e1, env)
                              in
                                if t = BOOL then 
                                	if typCheckExp(e2, env) = typCheckExp(e3, env) then typCheckExp(e2, env) 
                                	else raise Fail "Error in Type Checking: Both Arms of IfExp don't have same type"
                                else raise Fail "Error in Type Checking: Condition of IfExp is not Bool"
                              end
      | AppExp (x, e) =>  (case typCheckExp(x, env) of 
                            ARROW(t1, t2) => (if t1 = typCheckExp(e, env) then t2 
                                              else raise Fail "Error in Type Checking: Actual Parameter of Function is not of required type")
                            | _ => raise Fail "Error in Type Checking: Expected function not of required type"
                          )
                          (*let
                            val ARROW(t1, t2) = lookup(x, env)
                          in
                            if t1 = typCheckExp(e, env) then t2 
                            else raise Fail "Actual Parameter of Function is not of required type"
                          end*)

    and

	typCheckBinExp(b:binop, e1:exp, e2:exp, env:typEnvironment):typ =
	    case (b, typCheckExp(e1, env), typCheckExp(e2, env))  of
	    (Plus, INT , INT) => INT
	    | (Minus, INT, INT) => INT
	    | (Times, INT, INT) => INT
	    | (Equals, INT, INT)  => BOOL
	    | (GreaterThan, INT, INT) => BOOL
      | (LessThan, INT, INT) => BOOL
	    | (Equals, BOOL, BOOL) => BOOL
	    | (And, BOOL, BOOL) => BOOL
	    | (Or, BOOL, BOOL) => BOOL
	    | (Xor, BOOL, BOOL) => BOOL
	    | (Implies, BOOL, BOOL) => BOOL
	    | _  => raise Fail "Operands of BinExp does not match required type"

  	and

  	typCheckUniExp(u:uniop, e:exp, env:typEnvironment):typ =
    	case (u, typCheckExp(e, env)) of 
     	(Not, BOOL) => BOOL
    	| (Negate, INT) => INT
    	| _ => raise Fail "Operands of UniExp does not match required type"

fun typCheckProgram (e:exp list) : typ list =
	let
		fun findfunc(e, env) = 
			case e of 
				[] => env
				| Fun(a, b, c, d, e) :: tl => findfunc(tl, env) @ add(a, typCheckExp(Fun(a, b, c, d, e), []), env)
				| _ :: tl => findfunc(tl, env)

		val env = findfunc(e, [])

		fun helper (e:exp list, env:typEnvironment, ans:typ list) : typ list =
			case e of 
				[] => ans
				| hd :: tl => ans @ [typCheckExp(hd, env)] @ helper(tl, env, ans)
	in
		helper(e, env, [])
	end

end

