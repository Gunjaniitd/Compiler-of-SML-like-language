structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

fun evalExp(e:exp, env:environment):value =
    case e of
	    NumExp i => IntVal i
      | StringExp s => StringVal s
      | VarExp x => envLookup (x, env) 			
      | BoolExp b => BoolVal b  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | UniExp (u, e) => evalUniExp(u, e, env)
      | LetExp(ValDecl(x, e1), e2) => let
                                  	    val v1 = evalExp (e1, env)
                                      in
                                  	    evalExp(e2, envAdd (x, v1, env))
                                      end	  
      | Fn (x, s1, s2, expr) => FuncVal (x, expr, env)
      | Fun (x, y, s1, s2, expr) => FuncVal(y, expr, env)
      | IfExp (e1, e2, e3) => let
                                val BoolVal i1 = evalExp (e1, env)
                              in
                                if i1 then evalExp(e2, env) else evalExp(e3, env)
                              end
      | AppExp (x, e) =>  (case evalExp(x, env) of 
                            FuncVal(y, expr, env2) => evalExp(expr, envAdd (y, evalExp (e, env), env) @ env2 )
                            | _ => raise Fail ("Error in evaluation! Expected function not found") 
                          )

                          (*let
                            val FuncVal(y, expr, env2) = envLookup(evalExp(x, env) , env)
                          in
                            evalExp(expr, envAdd (y, evalExp (e, env), env) @ env2 )
                          end*)
                                            	
  and

  evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
    case (b, evalExp(e1, env), evalExp(e2, env))  of
      (Plus, IntVal i1, IntVal i2) => IntVal (i1+i2)
      | (Minus, IntVal i1, IntVal i2) => IntVal (i1-i2)
      | (Times, IntVal i1, IntVal i2) => IntVal (i1*i2)
      | (Equals, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
      | (GreaterThan, IntVal i1, IntVal i2) => BoolVal (i1 > i2)
      | (LessThan, IntVal i1, IntVal i2) => BoolVal (i1 < i2)
      | (Equals, StringVal s1, StringVal s2) => BoolVal (s1 = s2)
      | (Equals, BoolVal i1, BoolVal i2) => BoolVal (i1 = i2)
      | (And, BoolVal i1, BoolVal i2) => BoolVal (i1 andalso i2)
      | (Or, BoolVal i1, BoolVal i2) => BoolVal (i1 orelse i2)
      | (Xor, BoolVal i1, BoolVal i2) => BoolVal ( (i1 andalso not(i2)) orelse (not(i1) andalso i2) )
      | (Implies, BoolVal i1, BoolVal i2) => BoolVal (not(i1) orelse i2)
      | _  => raise brokenTypes

  and

  evalUniExp(u:uniop, e:exp, env:environment):value =
    case (u, evalExp(e, env)) of 
      (Not, BoolVal i) => BoolVal (not(i))
      | (Negate, IntVal i) => IntVal (~1 * i)
      | _ => raise brokenTypes

fun evalProgram (e:exp list) : value list =
	let
		fun findfunc(e, env) = 
			case e of 
				[] => env
				| Fun(a, b, c, d, e) :: tl => findfunc(tl, env) @ envAdd(a, evalExp(Fun(a, b, c, d, e), []), env)
				| _ :: tl => findfunc(tl, env)

		val env = findfunc(e, [])

		fun helper (e:exp list, env:environment, ans:value list) : value list =
			case e of 
				[] => ans
				| hd :: tl => ans @ [evalExp(hd, env)] @ helper(tl, env, ans)
	in
		helper(e, env, [])
	end
  
end
