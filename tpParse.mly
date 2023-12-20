%{
open Ast
%}

%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token CONCAT
%token LPAREN RPAREN SEMICOLON
%token ASSIGN
%token IF THEN ELSE BEGIN END

%token EOF

%start <Ast.progType> prog
%%
prog: list(classe) block EOF { }


(*================ CLASS ==================*)
%token CLASS
%token IS
%token EXTENDS
%token LCURL RCURL COMMA COLON (* RCURL : {   LCURL : } *)
%token AUTO STATIC
%token WALRUS (*  :=  (morse) *)
%token OVERRIDE



classe :CLASS nomClass = ID LPAREN lparamOPT RPAREN heriteOPT IS LCURL corpsClassOPT RCURL { }

(*definition des parametres de la class*)
lparamOPT : { }
    | lparam { }

lparam: param COMMA lparam { } (* la virgule est un delimitateur*)
    | param { }

param : ID { }
    | ID COLON ID { }

(* class mere *)
heriteOPT : { }
    | EXTENDS nomClassParent = ID  { }

(* corp de la class *)
corpsClassOPT : list(?????) { }




(*=============== DECLARATION CHAMP ===================*)

champ : staticOPT autoOPT nom = ID COLON classe' = ID { }

staticOPT : { }
    | STATIC

autoOPT : { }
    | AUTO

(*================= DECLARATION METHODE =================*)


methode : DEF overrideOPT staticOPT nom = ID LPAREN lparamOPT RPAREN suiteMethode { }

suiteMethode : COLON classe' = ID WALRUS expression  { }
    | classOPT IS bloc

classOPT : { }
    | COLON classe' = ID { }

overrideOPT : { }
    | OVERRIDE { }


expression : { }




(*==================================*)






(*==================================*)

block : { }
