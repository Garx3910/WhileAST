
CM.make("$/basis.cm"); (* loads SML basis library *)
CM.make("$/ml-yacc-lib.cm"); (* loads SML YACC library *)
use "ASTRules.sml"; (* datatype for integer expression abstract syntax trees *)
use "whileAST.yacc.sig"; (* defines Intexp_TOKENS
and other datatypes *)
use "whileAST.yacc.sml"; (* defines shift-reduce parser *)
use "whileAST.lex.sml"; (* load lexer *after* parser since it uses
tokens defined by parser *)
use "whileAST.sml"; (* load top-level parser interface *)
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details of parse trees *)
Control.Print.stringDepth := 1000; (* and strings *)
open whileAST; (* open the parsing module so that we can use parseString
and parseFile without qualification. *)
