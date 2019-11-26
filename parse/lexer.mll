{
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
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
| whitespace {read lexbuf}
| newline {next_line lexbuf; read lexbuf}
| "=" { EQ }
| "<=" { LE }
| "<" { LT }
| ">=" { GE }
| ">" { GT }
| "!" { NOT }
| "&" { AND }
| "|" { OR }
| "->" { IMPLIES }
| "<->" { BIIMP }
| "\\forall" { FORALL }
| "\\exists" { EXISTS }
| "[" { LEFT_BRACKET }
| "]" { RIGHT_BRACKET }
| '{' { LEFT_BRACE }
| '}' { RIGHT_BRACE }
| '(' { LEFT_PAREN }
| ')' { RIGHT_PAREN }
| "<" { LEFT_ANGLE }
| ">" { RIGHT_ANGLE }
| ":=" { ASSIGN }
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
| eof { EOF }
