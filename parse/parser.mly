(* tokens and aliases *)
%token <string> ID
%token <string> NUMBER
%token EOF
%token EQ "="
%token NEQ "!="
%token LE "<="
%token GE ">="
%token TRUE "true"
%token FALSE "false"
%token NOT "!"
%token AND "&"
%token OR "|"
%token IMPLIES "->"
%token IFF "<->"
%token FORALL "forall"
%token EXISTS "exists"
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token EXP "^"
%token LBRACKET "["
%token RBRACKET "]"
%token LBRACE "{"
%token RBRACE "}"
%token LPAREN "("
%token RPAREN ")"
%token LANGLE "<"
%token RANGLE ">"
%token ASSIGN ":="
%token PRIME "'"
%token TEST "?"
%token SEMICOLON ";"
%token CHOICE "++"
%token PROBCHOICE "+++"
%token COMMA ","
%token COLON ":"
%token BERNOULLI "Bernoulli"
%token GEOMETRIC "Geometric"

(* precedence, lowest to highest *)
%right "++" "+++"
%right ";"
%right ","
%right LOOP PROBLOOP
%nonassoc "<->"
%right "->"
%right "|"
%right "&"
%right "!"
%right DIAMOND
%right BOX
%right "exists"
%right "forall"
%right "=" "!=" "<=" ">=" LT GT
%left "+" "-"
%left NEG
%left "*" "/"
%right "^"

%start <Ndsdl_extra.Formula.t option> top_level
%%

top_level:
| p = formula; EOF { Some p }
| EOF { None }

term:
| x = ID { Ndsdl_extra.Term.Var x }
| c = NUMBER { Ndsdl_extra.Term.Number c }
| "-" e = term { Ndsdl_extra.Term.Unop (`Neg, e) } %prec NEG
| e1 = term "+" e2 = term { Ndsdl_extra.Term.Binop (`Plus,e1,e2) }
| e1 = term "-" e2 = term { Ndsdl_extra.Term.Binop (`Minus,e1,e2) }
| e1 = term "*" e2 = term { Ndsdl_extra.Term.Binop (`Times,e1,e2) }
| e1 = term "/" e2 = term { Ndsdl_extra.Term.Binop (`Div,e1,e2) }
| e1 = term "^" e2 = term { Ndsdl_extra.Term.Binop (`Exp,e1,e2) }
| "(" e = term ")" { e }

program:
| "?" p = formula { Ndsdl_extra.Program.Test p }
| a = program ";" b = program { Ndsdl_extra.Program.Compose (a,b) }
| a = program "++" b = program { Ndsdl_extra.Program.Choice (a,b) }
| x = ID ":=" e = term { Ndsdl_extra.Program.Assign (x,e) }
| x = ID ":=" "*" { Ndsdl_extra.Program.Assignany x }
| x = ID ":=" "{" p = separated_nonempty_list(",", pmf) "}" { Ndsdl_extra.Program.Assignpmf (x,p) }
| x = ID ":=" "Bernoulli" "(" p = term ")" { Ndsdl_extra.Program.Assignbernoulli (x,p) }
| x = ID ":=" "Geometric" "(" p = term ")" { Ndsdl_extra.Program.Assigngeometric (x,p) }
| "{" a = program "}" "*" { Ndsdl_extra.Program.Loop a } %prec LOOP
| "{" a = program "}" "*" ":" p = term { Ndsdl_extra.Program.Probloop (p, a) } %prec PROBLOOP
| "{" o = separated_nonempty_list(",", ode) "}" { Ndsdl_extra.Program.Ode (o, None)}
| "{" o = separated_nonempty_list(",", ode) "&" f = formula "}" { Ndsdl_extra.Program.Ode (o, Some f)}
| "{" p = separated_nonempty_list("+++", probchoice) "}" { Ndsdl_extra.Program.Probchoice p }
| "{" a = program "}" { a }

ode:
| x = ID "'" "=" e = term { (x,e) }

probchoice:
| p = term ":" a = program { (p, a) }

pmf:
| p = term ":" e = term { (p, e) }

formula:
| "true" { Ndsdl_extra.Formula.True }
| "false" { Ndsdl_extra.Formula.False }
| p = formula "&" q = formula { Ndsdl_extra.Formula.Logicalbinop (`And, p,q) }
| p = formula "|" q = formula { Ndsdl_extra.Formula.Logicalbinop (`Or, p,q) }
| p = formula "->" q = formula { Ndsdl_extra.Formula.Logicalbinop (`Implies, p,q) }
| p = formula "<->" q = formula { Ndsdl_extra.Formula.Logicalbinop (`Equiv, p,q) }
| "!" p = formula { Ndsdl_extra.Formula.Logicalunop (`Not, p) }
| e1 = term "=" e2 = term { Ndsdl_extra.Formula.Compare (`Eq,e1,e2) }
| e1 = term "<" e2 = term { Ndsdl_extra.Formula.Compare (`Lt,e1,e2) } %prec LT
| e1 = term "<=" e2 = term { Ndsdl_extra.Formula.Compare (`Le,e1,e2) }
| e1 = term ">" e2 = term { Ndsdl_extra.Formula.Compare (`Gt,e1,e2) } %prec GT
| e1 = term ">=" e2 = term { Ndsdl_extra.Formula.Compare (`Ge,e1,e2) }
| e1 = term "!=" e2 = term { Ndsdl_extra.Formula.Compare (`Neq,e1,e2) }
| "forall" x = ID; p = formula { Ndsdl_extra.Formula.Forall (x,p) } %prec FORALL
| "exists" x = ID; p = formula { Ndsdl_extra.Formula.Exists (x,p) } %prec EXISTS
| "[" a = program "]" p = formula { Ndsdl_extra.Formula.Box (a,p) } %prec BOX
| "<" a = program ">" p = formula { Ndsdl_extra.Formula.Diamond (a,p) } %prec DIAMOND
| "(" p = formula ")" { p }
