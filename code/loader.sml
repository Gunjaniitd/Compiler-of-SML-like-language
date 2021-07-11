CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "evaluator.sml";
use "typechecker.sml";
use "parser.yacc.sig";
use "parser.yacc.sml";
use "lexer.lex.sml";
use "load-calc.sml";
open EVALUATOR;
open TYPECHECKER;
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
