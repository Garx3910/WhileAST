structure ASTRules =
struct

datatype intexp = Int of int
|VarInt of string
| IEXP  of addop * intexp * intexp
| UnIEXP of unaddop*intexp
and addop = PLUS | SUB | TIMES | DIV | MOD
and unaddop = MINUS

datatype boolexp = TT
| FF
| VarBool of string
| BiBEXP of logop * boolexp * boolexp
| UnBEXP of unlogop * boolexp
| RELEXP of relop * intexp * intexp
and relop = LEQ | GEQ | LT | GT | EQ | NEQ 
and logop = AND | OR
and unlogop = NOT

datatype exp = IntExp of intexp | BoolExp of boolexp


datatype cmdList = CmdList of cmd*cmdList
| Cmd of cmd
and cmd = Assign of string*exp
| Rd of string
| Print of intexp
| IfThenElse of boolexp*cmdSeq*cmdSeq
| Loop of boolexp*cmdSeq
and cmdSeq = CmdSeq of cmdList
| Null

datatype typ = INT | BOOL

datatype varList = VarList of string*varList
| Var of string

datatype decl = Dec of varList*typ
datatype declSeq = DecList of decl*declSeq
| Decl of decl

datatype blk = BLOCK of declSeq*cmdSeq
| BLK of cmdSeq

datatype prog = PROG of string*blk

end

