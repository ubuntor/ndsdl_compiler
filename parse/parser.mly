%token <string> ID
%token EQ
%token LE
%token LT
%token GE
%token GT
%token NOT
%token AND
%token OR
%token IMPLIES
%token BIIMP
%token FORALL
%token EXISTS
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_ANGLE
%token RIGHT_ANGLE
%token ASSIGN
%token STAR
%token PRIME
%token TEST
%token SEMICOLON
%token CHOICE
%token REPEAT
%token PROB_CHOICE
%token COMMA
%token EOF

%right ARROW

%start <Ndsdl.File.t option> file
%%

file:
| EOF { None }
| e = expr EOF { Some e }

program:
|

formula:
| 
