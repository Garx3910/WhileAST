(* no SML code in this case *)
%% (* separates initial section from middle section of .yacc file *)
%name whileAST
%pos int
%term
INT of int
| ADD | Sub | MUL | Div | Mod
| LPAREN | RPAREN 
| LCURL | RCURL
| ID of string
| PROGRAM | VARDEC | TYPEINT | TYPEBOOL | GETVAR | PRINT 
| IFCOND | IFEXE | ELSEEXE | ENDIF | WHILECOND | WHILEEXE | ENDWHILE
| TRUE | FALSE
| Lt | Gt | Leq | Geq | Eq | Neq | And | Or | Not
| COMMA | SEMI | GETS | DOUBLECOLON | Minus | COLON
| EOF (* End of file*)
%nonterm
Start of ASTRules.prog
| Block of ASTRules.blk
| DeclarationSequence of ASTRules.declSeq
| Declaration of ASTRules.decl
| CommandSequence of ASTRules.cmdSeq
| Command of ASTRules.cmd
| CommandList of ASTRules.cmdList
| Type of ASTRules.typ
| VariableList of ASTRules.varList
| Exp of ASTRules.exp
| IntEXP of ASTRules.intexp
| IntTerm of ASTRules.intexp
| IntFactor of ASTRules.intexp
| BoolEXP of ASTRules.boolexp
| BoolTerm of ASTRules.boolexp
| BoolFactor of ASTRules.boolexp
| Comparison of ASTRules.boolexp

%start Start
%keyword 
%eop EOF
%noshift EOF
%nodefault
%verbose
%value INT(0)

%%

(* Grammar productions and associated semantic actions (in parens) go here *)
Start: PROGRAM ID DOUBLECOLON Block (ASTRules.PROG(ID,Block))

Block : DeclarationSequence CommandSequence(ASTRules.BLOCK(DeclarationSequence,CommandSequence))
| CommandSequence(ASTRules.BLK(CommandSequence))

DeclarationSequence : Declaration DeclarationSequence(ASTRules.DecList(Declaration,DeclarationSequence))
| Declaration(ASTRules.Decl(Declaration))

Declaration : VARDEC VariableList COLON Type SEMI(ASTRules.Dec(VariableList,Type))

Type : TYPEINT(ASTRules.INT)
| TYPEBOOL(ASTRules.BOOL)

VariableList : ID COMMA VariableList(ASTRules.VarList(ID,VariableList))
|  ID(ASTRules.Var(ID))

CommandSequence : LCURL CommandList RCURL (ASTRules.CmdSeq(CommandList))
| LCURL RCURL (ASTRules.Null)

CommandList : Command SEMI CommandList (ASTRules.CmdList(Command,CommandList))
| Command SEMI(ASTRules.Cmd(Command))

Command : ID GETS Exp (ASTRules.Assign(ID,Exp))
| GETVAR ID (ASTRules.Rd(ID))
| PRINT IntEXP (ASTRules.Print(IntEXP))
| IFCOND BoolEXP IFEXE CommandSequence ELSEEXE CommandSequence ENDIF (ASTRules.IfThenElse(BoolEXP,CommandSequence1,CommandSequence2))
| WHILECOND BoolEXP WHILEEXE CommandSequence ENDWHILE (ASTRules.Loop(BoolEXP,CommandSequence))


Exp: IntEXP(ASTRules.IntExp(IntEXP))
| BoolEXP(ASTRules.BoolExp(BoolEXP))

IntEXP : IntTerm (IntTerm)
(* The following rules specify left associativity *)
| IntEXP ADD IntTerm (ASTRules.IEXP(ASTRules.PLUS,IntEXP,IntTerm))
| IntEXP Sub IntTerm (ASTRules.IEXP(ASTRules.SUB,IntEXP,IntTerm))

IntTerm : IntFactor (IntFactor)
| IntTerm MUL IntFactor (ASTRules.IEXP(ASTRules.TIMES,IntTerm,IntFactor))
| IntTerm Div IntFactor (ASTRules.IEXP(ASTRules.DIV,IntTerm,IntFactor))
| IntTerm Mod IntFactor (ASTRules.IEXP(ASTRules.MOD,IntTerm,IntFactor))

IntFactor : INT (ASTRules.Int(INT))
| LPAREN IntEXP RPAREN (IntEXP)
| Minus IntFactor(ASTRules.UnIEXP(ASTRules.MINUS,IntFactor))
| ID (ASTRules.VarInt(ID))

BoolEXP : BoolTerm (BoolTerm)
| BoolEXP Or BoolTerm (ASTRules.BiBEXP(ASTRules.OR,BoolEXP,BoolTerm))

BoolTerm : BoolFactor (BoolFactor)
| BoolTerm And BoolFactor (ASTRules.BiBEXP(ASTRules.AND,BoolTerm,BoolFactor))

BoolFactor : TRUE (ASTRules.TT)
| FALSE (ASTRules.FF)
| Comparison(Comparison)
| LPAREN BoolEXP RPAREN (BoolEXP)
| Not BoolFactor(ASTRules.UnBEXP(ASTRules.NOT,BoolFactor))
| ID (ASTRules.VarBool(ID))

Comparison: IntEXP Leq IntEXP(ASTRules.RELEXP(ASTRules.LEQ,IntEXP1,IntEXP2))
| IntEXP Geq IntEXP(ASTRules.RELEXP(ASTRules.GEQ,IntEXP1,IntEXP2))
| IntEXP Lt IntEXP(ASTRules.RELEXP(ASTRules.LT,IntEXP1,IntEXP2))
| IntEXP Gt IntEXP(ASTRules.RELEXP(ASTRules.GT,IntEXP1,IntEXP2))
| IntEXP Neq IntEXP(ASTRules.RELEXP(ASTRules.NEQ,IntEXP1,IntEXP2))
| IntEXP Eq IntEXP(ASTRules.RELEXP(ASTRules.EQ,IntEXP1,IntEXP2))




