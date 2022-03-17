structure whileAST = struct (* Module for interfacing with parser and lexer *)
(* Most of the following is "boilerplate" code that needs to be slightly edited on a per parser basis. *)
structure whileASTLrVals = whileASTLrValsFun(structure Token = LrParser.Token)
structure whileASTLex = whileASTLexFun(structure Tokens = whileASTLrVals.Tokens);
structure whileASTParser =
Join(structure LrParser = LrParser
structure ParserData = whileASTLrVals.ParserData
structure Lex = whileASTLex)
val invoke = fn lexstream => (* The invoke function invokes the parser given a lexer *)
let val print_error = fn (str,pos,_) =>
TextIO.output(TextIO.stdOut,
"***whileAST Parser Error at character position " ^ (Int.toString pos)
^ "***\n" ^ str^ "\n")
in whileASTParser.parse(0,lexstream,print_error,())
end
fun newLexer fcn = (* newLexer creates a lexer from a given input-reading function *)
let val lexer = whileASTParser.makeLexer fcn
val _ = whileASTLex.UserDeclarations.init()
in lexer
end

fun stringToLexer str = (* creates a lexer from a string *)
let val done = ref false
in newLexer (fn n => if (!done) then "" else (done := true; str))
end
fun fileToLexer filename = (* creates a lexer from a file *)
let val inStream = TextIO.openIn(filename)
in newLexer (fn n => TextIO.inputAll(inStream))
end
fun lexerToParser (lexer) = (* creates a parser from a lexer *)
let val dummyEOF = whileASTLrVals.Tokens.EOF(0,0)
val (result,lexer) = invoke lexer
val (nextToken,lexer) = whileASTParser.Stream.get lexer
in if whileASTParser.sameToken(nextToken,dummyEOF) then
result
else (TextIO.output(TextIO.stdOut,
"*** WHILEAST PARSER WARNING -- unconsumed input ***\n");
result)
end
fun read (infile:string) =
   let 
        val instream = TextIO.openIn infile
	fun loop instream =
	    String.implode(String.explode(TextIO.inputAll instream))

    in
	    loop instream before TextIO.closeIn instream
    end
val parseString = lexerToParser o stringToLexer (* parses a string *)
val parseFile = lexerToParser o stringToLexer o read (* parses a file *)
end (* struct *)