{
open Ast
open TpParse
open Lexing
exception Eof

(* gere les positions numero de ligne + decalage dans la ligne *)
let next_line lexbuf = Lexing.new_line lexbuf
}

rule
 comment = parse
    "*/"           { token lexbuf }
  | '\n'           { new_line lexbuf; comment lexbuf }

  | _              { comment lexbuf }
and
 token = parse
      ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
      { ID id }
  | [' ''\t''\r']     { token lexbuf }     (* skip blanks *)
  | '\n'              { next_line lexbuf; token lexbuf}
  | ['0'-'9']+ as lxm { CSTE(int_of_string lxm) }
  | "/*"              { comment lexbuf }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | ';'               { SEMICOLON }
  | ":="              { ASSIGN }
  | "<"		      { RELOP (Ast.Lt) }
  | "<="              { RELOP (Ast.Le) }
  | ">"               { RELOP (Ast.Gt) }
  | ">="              { RELOP (Ast.Ge) }
  | "="               { RELOP (Ast.Eq) }
  | "<>"              { RELOP (Ast.Neq) }
  | eof               { EOF }

