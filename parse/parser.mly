%token <string> ID
%token EOF
%token EQ
%token NEQ
%token LE
%token LT
%token GE
%token GT
%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token IMPLIES
%token IMPLIEDBY
%token IFF
%token FORALL
%token EXISTS
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token EXP
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LANGLE
%token RANGLE
%token ASSIGN
%token ASSIGNANY
%token PRIME
%token TEST
%token SEMICOLON
%token CHOICE
%token REPEAT
%token PROB_CHOICE
%token COMMA

%left CHOICE PROB_CHOICE
%left SEMICOLON
%left LOOP (* dummy *)
%left EQUIV
%left IMPLIES IMPLIEDBY
%left OR
%left AND
%left NOT
%left DIAMOND (* dummy *)
%left BOX (* dummy *)
%left EXISTS
%left FORALL
%left EQ NEQ LE LT GE GT
%left PLUS MINUS
%left NEG (* dummy *)
%left TIMES DIV
%right EXP

%start <Ndsdl.Formula.t option> top_level
%%

top_level:
| p = formula; EOF { Some p }
| EOF { None }

term:
| x = ID { Ndsdl.Term.Var x }
| MINUS; e = term { Ndsdl.Term.Neg e } %prec NEG
| e1 = term; PLUS; e2 = term { Ndsdl.Term.Plus (e1,e2) }
| e1 = term; MINUS; e2 = term { Ndsdl.Term.Minus (e1,e2) }
| e1 = term; TIMES; e2 = term { Ndsdl.Term.Times (e1,e2) }
| e1 = term; DIV; e2 = term { Ndsdl.Term.Div (e1,e2) }
| e1 = term; EXP; e2 = term { Ndsdl.Term.Exp (e1,e2) }
| LPAREN; e = term; RPAREN { e }

program:
| TEST; p = formula { Ndsdl.Program.Test p }
| a = program; SEMICOLON; b = program { Ndsdl.Program.Compose (a,b) }
| a = program; CHOICE; b = program { Ndsdl.Program.Choice (a,b) }

formula:
| TRUE { Ndsdl.Formula.True }
| FALSE { Ndsdl.Formula.False }
| p = formula; OR; q = formula { Ndsdl.Formula.And (p,q) }
| p = formula; AND; q = formula { Ndsdl.Formula.Or (p,q) }
| e1 = term; EQ; e2 = term { Ndsdl.Formula.Eq (e1,e2) }
| e1 = term; GT; e2 = term { Ndsdl.Formula.Gt (e1,e2) }
| e1 = term; GE; e2 = term { Ndsdl.Formula.Ge (e1,e2) }
| e1 = term; LT; e2 = term { Ndsdl.Formula.Lt (e1,e2) }
| e1 = term; LE; e2 = term { Ndsdl.Formula.Le (e1,e2) }
| e1 = term; NEQ; e2 = term { Ndsdl.Formula.Neq (e1,e2) }
| LBRACKET; a = program; RBRACKET; p = formula { Ndsdl.Formula.Box (a,p) } %prec BOX
| LANGLE; a = program; RANGLE; p = formula { Ndsdl.Formula.Diamond (a,p) } %prec DIAMOND
| LPAREN; p = formula; RPAREN { p }
