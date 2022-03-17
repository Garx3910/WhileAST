open Tokens
type pos = int
type lexresult = (svalue,pos) token
fun eof () = Tokens.EOF(0,0)

fun integer(str,lexPos) =
case Int.fromString(str) of
NONE => raise Fail("Shouldn't happen: sequence of digits not recognized as integer -- " ^ str)
| SOME(n) => INT(n,lexPos,lexPos)

fun init() = ()
%%
%header (functor whileASTLexFun(structure Tokens: whileAST_TOKENS));
alpha=[a-zA-Z];
digit=[0-9];
alphaNumUnd=[a-zA-Z0-9_];
whitespace=[\ \t\n];
any= [^];
%%

"program" => (PROGRAM(yypos,yypos));
"var" => (VARDEC(yypos,yypos));
"int" => (TYPEINT(yypos,yypos));
"bool" => (TYPEBOOL(yypos,yypos));
"read" => (GETVAR(yypos,yypos));
"write" => (PRINT(yypos,yypos));
"if" => (IFCOND(yypos,yypos));
"then" => (IFEXE(yypos,yypos));
"else" => (ELSEEXE(yypos,yypos));
"endif" => (ENDIF(yypos,yypos));
"while" => (WHILECOND(yypos,yypos));
"do" => (WHILEEXE(yypos,yypos));
"endwh" => (ENDWHILE(yypos,yypos));
"tt" => (TRUE(yypos,yypos));
"ff" => (FALSE(yypos,yypos));
{alpha}{alphaNumUnd}* => (ID(yytext,yypos,yypos));
{digit}+ => (integer(yytext,yypos));
"+" => (ADD(yypos,yypos));
"-" => (Sub(yypos,yypos));
"*" => (MUL(yypos,yypos));
"/" => (Div(yypos,yypos));
"%" => (Mod(yypos,yypos));
"<" => (Lt(yypos,yypos));
">" => (Gt(yypos,yypos));
"<=" => (Leq(yypos,yypos));
">=" => (Geq(yypos,yypos));
"=" => (Eq(yypos,yypos));
"<>" => (Neq(yypos,yypos));
"(" => (LPAREN(yypos,yypos));
")" => (RPAREN(yypos,yypos));
"," => (COMMA(yypos,yypos));
";" => (SEMI(yypos,yypos)); 
":=" => (GETS(yypos,yypos));
"::" => (DOUBLECOLON(yypos,yypos));
":" => (COLON(yypos,yypos));
"{" => (LCURL(yypos,yypos));
"}" => (RCURL(yypos,yypos));
"~" => (Minus(yypos,yypos));
"||" => (Or(yypos,yypos));
"&&" => (And(yypos,yypos));
"!" => (Not(yypos,yypos));
{whitespace} => (lex());
{any} => (raise Fail("Slip scanner: unexpected character \"" ^ yytext ^ "\""));