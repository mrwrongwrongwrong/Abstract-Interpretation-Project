
(* The type of tokens. *)

type token = 
  | WHILE
  | SEMICOLON
  | RPAREN
  | RBRACKET
  | PLUS
  | NUM of (int)
  | NEQ
  | NAND
  | MINUS
  | LT
  | LPAREN
  | LBRACKET
  | IF
  | IDENT of (string)
  | GT
  | EQ
  | END
  | ELSE
  | BREAK
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (AbstractTree.bare_tree)
