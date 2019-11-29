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

(* precedence, lowest to highest *)
%right "++" "+++"
%right ";"
%right ","
%right LOOP
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

%start <Ndsdl.Formula.t option> top_level
%%

top_level:
| p = formula; EOF { Some p }
| EOF { None }

term:
| x = ID { Ndsdl.Term.Var x }
| c = NUMBER { Ndsdl.Term.Number c }
| "-" e = term { Ndsdl.Term.Neg e } %prec NEG
| e1 = term "+" e2 = term { Ndsdl.Term.Plus (e1,e2) }
| e1 = term "-" e2 = term { Ndsdl.Term.Minus (e1,e2) }
| e1 = term "*" e2 = term { Ndsdl.Term.Times (e1,e2) }
| e1 = term "/" e2 = term { Ndsdl.Term.Div (e1,e2) }
| e1 = term "^" e2 = term { Ndsdl.Term.Exp (e1,e2) }
| "(" e = term ")" { e }

program:
| "?" p = formula { Ndsdl.Program.Test p }
| a = program ";" b = program { Ndsdl.Program.Compose (a,b) }
| a = program "++" b = program { Ndsdl.Program.Choice (a,b) }
| x = ID ":=" e = term { Ndsdl.Program.Assign (x,e) }
| x = ID ":=" "*" { Ndsdl.Program.Assignany x }
| x = ID ":=" "{" p = separated_nonempty_list(",", pmf) "}" { Ndsdl.Program.Assignpmf (x,p) }
| "{" a = program "}" "*" { Ndsdl.Program.Loop a } %prec LOOP
| "{" o = separated_nonempty_list(",", ode) "}" { Ndsdl.Program.Ode (o, None)}
| "{" o = separated_nonempty_list(",", ode) "&" f = formula "}" { Ndsdl.Program.Ode (o, Some f)}
| "{" p = separated_nonempty_list("+++", probchoice) "}" { Ndsdl.Program.Probchoice p }
| "{" a = program "}" { a }

ode:
| x = ID "'" "=" e = term { (x,e) }

probchoice:
| p = term ":" a = program { (p, a) }

pmf:
| p = term ":" e = term { (p, e) }

formula:
| "true" { Ndsdl.Formula.True }
| "false" { Ndsdl.Formula.False }
| p = formula "&" q = formula { Ndsdl.Formula.And (p,q) }
| p = formula "|" q = formula { Ndsdl.Formula.Or (p,q) }
| p = formula "->" q = formula { Ndsdl.Formula.Implies (p,q) }
| p = formula "<->" q = formula { Ndsdl.Formula.Equiv (p,q) }
| "!" p = formula { Ndsdl.Formula.Not p }
| e1 = term "=" e2 = term { Ndsdl.Formula.Eq (e1,e2) }
| e1 = term "<" e2 = term { Ndsdl.Formula.Lt (e1,e2) } %prec LT
| e1 = term "<=" e2 = term { Ndsdl.Formula.Le (e1,e2) }
| e1 = term ">" e2 = term { Ndsdl.Formula.Gt (e1,e2) } %prec GT
| e1 = term ">=" e2 = term { Ndsdl.Formula.Ge (e1,e2) }
| e1 = term "!=" e2 = term { Ndsdl.Formula.Neq (e1,e2) }
| "forall" x = ID; p = formula { Ndsdl.Formula.Forall (x,p) } %prec FORALL
| "exists" x = ID; p = formula { Ndsdl.Formula.Exists (x,p) } %prec EXISTS
| "[" a = program "]" p = formula { Ndsdl.Formula.Box (a,p) } %prec BOX
| "<" a = program ">" p = formula { Ndsdl.Formula.Diamond (a,p) } %prec DIAMOND
| "(" p = formula ")" { p }
