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
%token STAR
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
%left TIMES DIV (* dummy *)

%start <Ndsdl.Formula.t option> top_level
%%

top_level:
| p = formula; EOF { Some p }
| EOF { None }

term:
| x = ID { Ndsdl.Term.Var x }

program:
| TEST; p = formula { Ndsdl.Program.Test p }

formula:
| TRUE { Ndsdl.Formula.True }
| FALSE { Ndsdl.Formula.False }
| p = formula; OR; q = formula { Ndsdl.Formula.And (p,q) }
| p = formula; AND; q = formula { Ndsdl.Formula.Or (p,q) }
| LBRACKET; a = program; RBRACKET; p = formula { Ndsdl.Formula.Box (a,p) } %prec BOX
| LANGLE; a = program; RANGLE; p = formula { Ndsdl.Formula.Diamond (a,p) } %prec DIAMOND
| LPAREN; p = formula; RPAREN { p }
