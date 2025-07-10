{
  open Batteries;;

  open Lexing;;
  open Parser;;
  open Printf;;

  exception LexerError of string;;

  let comment_depth = ref 0;;
  let inc r = (r := !r + 1; ())
  let dec r = (r := !r - 1; ())
}

let digit = ['0'-'9']
let integer = digit+

let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_cont = ident_start | ['0'-'9']
let ident = ident_start ident_cont*

let whitespace = [' ' '\t']+

let comment_start = "(*"
let comment_end = "*)"

rule token = parse
  | whitespace { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | integer as x { INTEGER(x) }
  | comment_start { inc comment_depth; comment lexbuf }
  | comment_end { raise (LexerError ("Errant end-of-comment symbol")) }
  | "after" { AFTER }
  | "before" { BEFORE }
  | "print" { PRINT }
  | "isint" { ISINT }
  | "isbool" { ISBOOL }
  | "istuple" { ISTUPLE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "," { COMMA }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "<" { LESS }
  | ">" { GREATER }
  | "&&" { BOOL_AND }
  | "||" { BOOL_OR }
  | "let" { LET }
  | "=" { EQUAL }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "sleep" { SLEEP }
  | "parallel" { PARALLEL }
  | "send" { SEND }
  | "receive" { RECEIVE }
  | "def" { DEF }
  | "end" { END }
  | ident as x { IDENTIFIER x }
  | eof { EOF }
  | _ as c { raise (LexerError (sprintf "unrecognized character: %c" c)) }

and comment = parse
  | comment_start { inc comment_depth; comment lexbuf }
  | comment_end {
        dec comment_depth;
        (if !comment_depth>0 then comment else token) lexbuf
    }
  | eof { raise (LexerError "unterminated comment") }
  | '\n' { new_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }
