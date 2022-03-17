# WhileAST

# Files in the Submission
The following files are present in the submission: 
## whileAST.lex
The lex file for WHILE language. It defines the lexer rules for token recognition of our language.
## whileAST.yacc
The yacc file for WHILE language. It defines the parser rules for our language which produces the AST.
## whileAST.sml
It integrates the lexer and parser to build the parser which can parse a string from the input string or from the file whose name is the input.
## ASTRules.sml
It defines the datatype AST and its various components.
## load.sml
It runs the lex and yacc files to generate the lexer and parser.
## helloworld.while
A sample program of the WHILE language used for testing
# Running the Code
The following commands are to be entered in the terminal to run the code:

ml-lex whileAST.lex
ml-yacc whileAST.yacc
sml load.sml 

After this, in the sml terminal, the following commands parse the given strings or files respectively:
parseString("<WHILE language code to be parsed>");
parseFile("<Filename containing WHILE language code to be parsed>");

AST produced for code in "helloworld.while" : 

PROG
    ("helloworld",
     BLOCK
       (DecList
          (Dec
             (VarList
                ("a",
                 VarList
                   ("b",VarList ("c",VarList ("d",VarList ("e",Var "f"))))),
              INT),
           DecList
             (Dec (VarList ("m",Var "n"),BOOL),
              Decl (Dec (VarList ("c1",Var "c2"),INT)))),
        CmdSeq
          (CmdList
             (Print (IEXP (PLUS,Int 1,IEXP (DIV,Int 3,Int 5))),
              CmdList
                (Rd "a",
                 CmdList
                   (IfThenElse
                      (BiBEXP
                         (AND,TT,
                          BiBEXP
                            (OR,FF,
                             UnBEXP
                               (NOT,RELEXP (GT,UnIEXP (MINUS,Int 1),Int 2)))),
                       CmdSeq (Cmd (Print (Int 1))),
                       CmdSeq (Cmd (Print (Int 0)))),
                    Cmd
                      (Loop
                         (BiBEXP (OR,VarBool "m",TT),
                          CmdSeq
                            (CmdList
                               (Rd "c",
                                Cmd
                                  (Print
                                     (IEXP
                                        (MOD,
                                         IEXP (TIMES,VarInt "c1",VarInt "c2"),
                                         VarInt "f")))))))))))))

# Context-free grammar
The context-free grammar for our language is: G=<N,T,P,S>, where
T(terminals)={
INT of type int, "+" , "-" , "*" , "/" , "%" , "(" , ")" , "{" , "}" , ID of type string, "program" , "var" , "int" , "bool" , "read" , "write" , "if" , "then" , "else" , "endif", "while" , "do" , "endwh" , "tt" , "ff", "<" , ">" , "<=" , ">=" , "=" , "<>", "&&" , "||" , "!" , "," , ";" , ":=" , "::" , "~" , ":" 
},
N(non terminals) = {
Start , Block , DeclarationSequence , Declaration , CommandSequence , Command , CommandList , Type , VariableList , Exp , IntEXP , IntTerm , IntFactor , BoolEXP , BoolTerm , BoolFactor , Comparison 
},
S = Start,

P = {
    Start: "program" ID "::" Block 

Block : DeclarationSequence CommandSequence
| CommandSequence

DeclarationSequence : Declaration DeclarationSequence
| Declaration

Declaration : "var" VariableList ":" Type ";"

Type : "int"
| "bool"

VariableList : ID "," VariableList
|  ID

CommandSequence : "{" CommandList "}"
| "{" "}"

CommandList : Command ";" CommandList 
| Command ";"

Command : ID ":=" Exp
| "read" ID 
| "write" IntEXP 
| "if" BoolEXP "then" CommandSequence "else" CommandSequence "endif" 
| "while" BoolEXP "do" CommandSequence "endwh" 


Exp: IntEXP
| BoolEXP

IntEXP : IntTerm 
(* The following rules specify left associativity *)
| IntEXP "+" IntTerm
| IntEXP "-" IntTerm 

IntTerm : IntFactor 
| IntTerm "*" IntFactor 
| IntTerm "/" IntFactor 
| IntTerm "%" IntFactor 

IntFactor : INT 
| "(" IntEXP ")" 
| "~" IntFactor
| ID 

BoolEXP : BoolTerm 
| BoolEXP "||" BoolTerm 

BoolTerm : BoolFactor 
| BoolTerm "&&" BoolFactor 

BoolFactor : "tt" 
| "ff"
| Comparison
| "(" BoolEXP ")"
| "!" BoolFactor
| ID 

Comparison: IntEXP "<=" IntEXP
| IntEXP ">=" IntEXP
| IntEXP "<" IntEXP
| IntEXP ">" IntEXP
| IntEXP "<>" IntEXP
| IntEXP "=" IntEXP
}

# AST datatype definition
The AST dataype consists of the following definitions defined in the file ASTRules.sml as:

1. datatype intexp = Int of int
    |VarInt of string
    | IEXP  of addop * intexp * intexp
    | UnIEXP of unaddop*intexp
    and addop = PLUS | SUB | TIMES | DIV | MOD
    and unaddop = MINUS

    This dataype defines the integer expressions as : ( Operator, operand1, operand2) or (unary_operator, operand) or (Integer) or (Variable of type int)

2. datatype boolexp = TT
    | FF
    | VarBool of string
    | BiBEXP of logop * boolexp * boolexp
    | UnBEXP of unlogop * boolexp
    | RELEXP of relop * intexp * intexp
    and relop = LEQ | GEQ | LT | GT | EQ | NEQ 
    and logop = AND | OR
    and unlogop = NOT

    This datatype defines the boolean expressions as : (binary_operator, operand1, operand 2) or (unary_oprator, operand) or (true) or (false) or (Relational_Operator, operand1, operand2) or (Variable of type bool)

3. datatype exp = IntExp of intexp | BoolExp of boolexp

    This datatype defines an expression as an integer expression or a boolean expression

4. datatype cmdList = CmdList of cmd*cmdList
    | Cmd of cmd
    and cmd = Assign of string*exp
    | Rd of string  
    | Print of intexp
    | IfThenElse of boolexp*cmdSeq*cmdSeq
    | Loop of boolexp*cmdSeq
    and cmdSeq = CmdSeq of cmdList
    | Null

    This datatype defines the command sequence and command

5. datatype typ = INT | BOOL

    This datatype defines the edclaration sequence

6. datatype varList = VarList of string*varList
    | Var of string
    
    This datatype defines the variable list 

7. datatype decl = Dec of varList*typ
    This datatypre defines the declarations

8. datatype declSeq = DecList of decl*declSeq
    | Decl of decl

    This dataype defines the declaration sequence

9. datatype blk = BLOCK of declSeq*cmdSeq
    | BLK of cmdSeq

    This datatype defines the Block

10. datatype prog = PROG of string*blk

    This datatype defines the program


# Syntax-directed translation

Start: PROGRAM ID DOUBLECOLON Block production rule generates (ASTRules.PROG(ID,Block))

Block : DeclarationSequence CommandSequence production rule generates (ASTRules.BLOCK(DeclarationSequence,CommandSequence))
| CommandSequence production rule generates (ASTRules.BLK(CommandSequence))

DeclarationSequence : Declaration DeclarationSequence production rule generates (ASTRules.DecList(Declaration,DeclarationSequence))
| Declaration production rule generates (ASTRules.Decl(Declaration))

Declaration : VARDEC VariableList COLON Type SEMI production rule generates (ASTRules.Dec(VariableList,Type))

Type : TYPEINT  production rule generates (ASTRules.INT)
| TYPEBOOL production rule generates (ASTRules.BOOL)

VariableList : ID COMMA VariableList production rule generates (ASTRules.VarList(ID,VariableList))
|  ID production rule generates (ASTRules.Var(ID))

CommandSequence : LCURL CommandList RCURL production rule generates (ASTRules.CmdSeq(CommandList))
| LCURL RCURL production rule generates (ASTRules.Null)

CommandList : Command SEMI CommandList production rule generates (ASTRules.CmdList(Command,CommandList))
| Command SEMI production rule generates (ASTRules.Cmd(Command))

Command : ID GETS Exp production rule generates (ASTRules.Assign(ID,Exp))
| GETVAR ID production rule generates (ASTRules.Rd(ID))
| PRINT IntEXP production rule generates (ASTRules.Print(IntEXP))
| IFCOND BoolEXP IFEXE CommandSequence ELSEEXE CommandSequence ENDIF production rule generates (ASTRules.IfThenElse(BoolEXP,CommandSequence1,CommandSequence2))
| WHILECOND BoolEXP WHILEEXE CommandSequence ENDWHILE production rule generates (ASTRules.Loop(BoolEXP,CommandSequence))


Exp: IntEXP production rule generates (ASTRules.IntExp(IntEXP))
| BoolEXP production rule generates (ASTRules.BoolExp(BoolEXP))

IntEXP : IntTerm production rule generates (IntTerm)
(* The following rules specify left associativity *)
| IntEXP ADD IntTerm production rule generates (ASTRules.IEXP(ASTRules.PLUS,IntEXP,IntTerm))
| IntEXP Sub IntTerm production rule generates (ASTRules.IEXP(ASTRules.SUB,IntEXP,IntTerm))

IntTerm : IntFactor production rule generates (IntFactor)
| IntTerm MUL IntFactor production rule generates (ASTRules.IEXP(ASTRules.TIMES,IntTerm,IntFactor))
| IntTerm Div IntFactor production rule generates (ASTRules.IEXP(ASTRules.DIV,IntTerm,IntFactor))
| IntTerm Mod IntFactor production rule generates (ASTRules.IEXP(ASTRules.MOD,IntTerm,IntFactor))

IntFactor : INT production rule generates (ASTRules.Int(INT))
| LPAREN IntEXP RPAREN production rule generates (IntEXP)
| Minus IntFactor production rule generates (ASTRules.UnIEXP(ASTRules.MINUS,IntFactor))
| ID production rule generates (ASTRules.VarInt(ID))

BoolEXP : BoolTerm production rule generates (BoolTerm)
| BoolEXP Or BoolTerm production rule generates (ASTRules.BiBEXP(ASTRules.OR,BoolEXP,BoolTerm))

BoolTerm : BoolFactor production rule generates (BoolFactor)
| BoolTerm And BoolFactor  production rule generates (ASTRules.BiBEXP(ASTRules.AND,BoolTerm,BoolFactor))

BoolFactor : TRUE production rule generates (ASTRules.TT)
| FALSE production rule generates (ASTRules.FF)
| Comparison production rule generates (Comparison)
| LPAREN BoolEXP RPAREN production rule generates (BoolEXP)
| Not BoolFactor production rule generates (ASTRules.UnBEXP(ASTRules.NOT,BoolFactor))
| ID production rule generates (ASTRules.VarBool(ID))

Comparison: IntEXP Leq IntEXP production rule generates (ASTRules.RELEXP(ASTRules.LEQ,IntEXP1,IntEXP2))
| IntEXP Geq IntEXP production rule generates (ASTRules.RELEXP(ASTRules.GEQ,IntEXP1,IntEXP2))
| IntEXP Lt IntEXP production rule generates (ASTRules.RELEXP(ASTRules.LT,IntEXP1,IntEXP2))
| IntEXP Gt IntEXP production rule generates (ASTRules.RELEXP(ASTRules.GT,IntEXP1,IntEXP2))
| IntEXP Neq IntEXP production rule generates (ASTRules.RELEXP(ASTRules.NEQ,IntEXP1,IntEXP2))
| IntEXP Eq IntEXP production rule generates (ASTRules.RELEXP(ASTRules.EQ,IntEXP1,IntEXP2))

# Auxiliary functions and Data

auxiliary datatypes (with the given names) for
Blocks : BLK
Declarations : DEC (for list of variable declarations along with their types e.g. INT(x), BOOL(b)).
Commands : CMD
Integer expressions : IEXP
Boolean expressions : BEXP

# Other Design Decisions

- Identifier to Variable caused 2 reduce-reduce conflicts, to solve which, the grammar would have to be changed, but it is not changed, thus, identifier reduction to variable in IntFactor or BoolFactor is ambiguous.

- Including bool expressions in relational expressions will cause many conflicts to remove which, the grammar would have to be changed significantly, which was causing increase in comlexity. Thus, that part was not implemented.

# Other Implementation Decisions

- CommandSequence is treated as a list of commands enclosed in curly brackets. Thus, extra non terminals and datatypes for command lists were implemented.

- Similarly, DeclarationSequence is treated as a list of declarations, whose head is a declaration, tail is another declaration sequence or just a single declaration

- The above implementation of DeclarationSequence did not cover the case of no declarations, to include which, block was modified to be either  DeclarationSequence followed by CommandSequence, or just a CommandSequence.

- To cover the case of no commands, which would appear as a pair of curly braces with nothing between them, CommandSequence was modified to be either a list of commands, or just Null

- Variable list was similarly implemented, that is, a list of identifiers

# Acknowledgements

Some of the code for whileAST.sml and load.sml were borrowed from the following link(source was Google):
[GLUE](http://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf)

This link was used to understand ML-Lex and ML-Yacc(souce was the course website):
[Lex_Yacc](http://rogerprice.org/ug/ug.pdf)
