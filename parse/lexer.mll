{
(* based on tokens and regexes from KeYMaera X, with additional tokens for NDSdL extensions *)
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*['_']?['_']?['0'-'9']*
let number = ['0'-'9']+['.']?['0'-'9']*

rule read = parse
| whitespace {read lexbuf}
| "/*" { read lexbuf }
| newline { next_line lexbuf; read lexbuf }
| eof { EOF }
| "=" { EQ }
| "!=" { NEQ }
| "<=" { LE }
| "<" { LT }
| ">=" { GE }
| ">" { GT }
| "true" { TRUE }
| "false" { FALSE }
| "!" { NOT }
| "&" { AND }
| "|" { OR }
| "->" { IMPLIES }
| "<-" { IMPLIEDBY }
| "<->" { IFF }
| "\\forall" { FORALL }
| "\\exists" { EXISTS }
| "+" { PLUS }
| "-" { MINUS }
| "/" { DIV }
| "^" { EXP }
| "[" { LBRACKET }
| "]" { RBRACKET }
| '{' { LBRACE }
| '}' { RBRACE }
| '(' { LPAREN }
| ')' { RPAREN }
| "<" { LANGLE }
| ">" { RANGLE }
| ":=" { ASSIGN }
| ":=*" { ASSIGNANY }
| "*" { STAR }
| "'" { PRIME }
| "?" { TEST }
| ";" { SEMICOLON }
| "++" { CHOICE }
| "*" { REPEAT }
| "+++" { PROB_CHOICE }
| "," { COMMA }
| id { ID (Lexing.lexeme lexbuf) }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}

and comment = parse
| "*/" { read lexbuf }
| "\n" { next_line lexbuf; comment lexbuf }
| eof { raise (SyntaxError "Unterminated comment")}
| _ { comment lexbuf }
