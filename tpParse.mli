
(* The type of tokens. *)

type token = 
  | WALRUS
  | TIMES
  | THIS
  | THEN
  | SUPER
  | STATIC
  | SEMICOLON
  | RPAREN
  | RETURN
  | RESULT
  | RELOP of (Ast.opComp)
  | RCURL
  | PLUS
  | OVERRIDE
  | NEW
  | MINUS
  | LPAREN
  | LCURL
  | IS
  | IF
  | IDCLASS of (string)
  | ID of (string)
  | EXTENDS
  | EOF
  | ELSE
  | DOT
  | DIV
  | DEF
  | CSTES of (string)
  | CSTE of (int)
  | COMMA
  | COLON
  | CLASS
  | AUTO
  | AMPERSAND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.progType)
