{
open Ast
open TpParse
open Lexing
exception Eof of string
exception SyntaxError of string

(* gere les positions numero de ligne + decalage dans la ligne *)
let next_line lexbuf = Lexing.new_line lexbuf

let parse_id id = match id with 
          "if"      -> IF
        | "then"    -> THEN
        | "else"    -> ELSE
        | "return"  -> RETURN
        | "def"     -> DEF
        | "class"   -> CLASS
        | "is"      -> IS
        | "extends" -> EXTENDS
        | "auto"    -> AUTO
        | "static"  -> STATIC
        | "override"-> OVERRIDE
        | "this"    -> THIS
        | "super"   -> SUPER
        | "result"  -> RESULT
        | "new"     -> NEW
        |   _       -> ID id
}

rule
 comment = parse
    "*/"           { token lexbuf }
  | '\n'           { new_line lexbuf; comment lexbuf }
  | eof            { raise (Eof ("Comment is not terminated")) }
  | _              { comment lexbuf }
and
parse_string buf = parse
    '"'       { CSTES (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; parse_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; parse_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; parse_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; parse_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; parse_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; parse_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      parse_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Eof ("String is not terminated")) }
and
 token = parse
    ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id { parse_id id }
  | ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as idclass { IDCLASS idclass }
  | [' ''\t''\r']     { token lexbuf }     (* skip blanks *)
  | '\n'              { next_line lexbuf; token lexbuf}
  | ['0'-'9']+ as lxm { CSTE(int_of_string lxm) }
  | "/*"              { comment lexbuf }
  | '"'               { parse_string (Buffer.create 17) lexbuf }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | ';'               { SEMICOLON }
  | eof               { EOF }
  | '{'               { LCURL }
  | '}'               { RCURL }
  | ','               { COMMA }
  | ':'               { COLON }
  | ":="              { WALRUS }
  | '.'               { DOT }
  | "<"		          { RELOP (Ast.Lt) }
  | "<="              { RELOP (Ast.Le) }
  | ">"               { RELOP (Ast.Gt) }
  | ">="              { RELOP (Ast.Ge) }
  | "="               { RELOP (Ast.Eq) }
  | "<>"              { RELOP (Ast.Neq) }
  | '&'               { AMPERSAND }

