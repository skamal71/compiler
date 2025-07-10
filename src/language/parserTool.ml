open Batteries;;

open Lexing;;
open Printf;;
open Parser;;

exception ParseFailure of string;;

(* A function to parse some text as a program.  This wrapper works with the
   generated lexer and parser but hides some messy OCaml details.  The given
   filename should be the origin of the provided text. *)
let parse (filename : string) (text : string) =
  let error_message lexbuf (err_type : string) (message : string option) =
    let p = lexbuf.lex_curr_p in
    sprintf "%s error in %s at line %d, column %d%s"
      err_type p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
      (match message with
       | None -> ""
       | Some s -> ": " ^ s)
  in
  let lexbuf = Lexing.from_string text in
  try
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    program Lexer.token lexbuf
  with
  | Lexer.LexerError msg ->
    raise (ParseFailure(error_message lexbuf "Lexer" (Some msg)))
  | Parser.Error ->
    raise (ParseFailure(error_message lexbuf "Parser" None))
;;
