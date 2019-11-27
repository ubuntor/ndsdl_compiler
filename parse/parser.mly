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

%start <Ndsdl.Formula.t> formula
%%

term:
| x = ID { Ndsdl.Term.Var x }

program:
| TEST; p = formula { Ndsdl.Program.Test p }

formula:
| TRUE { Ndsdl.Formula.True }
| FALSE { Ndsdl.Formula.False }
| LBRACKET; a = program; RBRACKET; p = formula { Ndsdl.Formula.Box (a,p)}
| LPAREN; p = formula; RPAREN { p }
