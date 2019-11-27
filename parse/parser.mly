%token <string> ID
%token EOF
%token EQ
%token NEQ
%token LE
%token LT
%token GE
%token GT
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

%start <Ndsdl.Formula.t option> formula
%%

formula:
| x = ID { Ndsdl.Formula.Var x }
