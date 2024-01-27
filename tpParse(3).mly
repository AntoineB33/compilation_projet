%{
open Ast
%}

(*=============== DECLARATION TOKEN ===================*)

%token <string> ID
%token <string> IDCLASS
%token <int> CSTE
%token TIMES DIV PLUS MINUS
%token LPAREN RPAREN SEMICOLON
%token IF THEN ELSE
%token RETURN
%token EOF
%token DEF
%token CLASS
%token IS
%token EXTENDS
%token LCURL RCURL COMMA COLON
%token AUTO STATIC
%token WALRUS
%token OVERRIDE
%token THIS SUPER RESULT
%token DOT
%token NEW
%token <Ast.opComp> RELOP
%token AMPERSAND

(*=============== PRIORITE TOKEN ===================*)

%left DOT
%left TIMES DIV
%left PLUS MINUS
%left RELOP
%left AMPERSAND

(*=============== START GRAMMAIRE ===================*)

%start <Ast.progType> prog
%%
prog: list(classe) bloc EOF { }


(*================ CLASS ==================*)


classe :
    | CLASS IDCLASS LPAREN lparamOpt RPAREN heriteOpt IS LCURL corpsClasse RCURL { }

lparamOpt:
    | separated_list(COMMA, param) { }

param :
    | separated_nonempty_list(COMMA, ID) COLON IDCLASS { }

(* class mere *)
heriteOpt : 
    | option(extClasse) { }

extClasse : EXTENDS IDCLASS { }

corpsClasse :
    | lchamp lmeth { }

lmeth : list(methode) { }

lchamp : list(champ) { }

(*=============== DECLARATION CHAMP ===================*)

champ : option(STATIC) separated_nonempty_list(COMMA,ident) COLON IDCLASS SEMICOLON { }

ident : option(AUTO) ID { }

(*================= DECLARATION METHODE =================*)


methode : DEF option(OVERRIDE) option(STATIC) ID LPAREN lparamOpt RPAREN suiteMethode { }
    | DEF IDCLASS LPAREN lparamOpt RPAREN option(super) IS bloc { }

super : COLON IDCLASS LPAREN lparamOpt RPAREN { }


suiteMethode :
    | COLON IDCLASS WALRUS expression { }
    | classOpt IS bloc { }

classOpt :
    | option(estClass) { }

estClass : COLON IDCLASS { }

(*================ DECLARATION EXPRESSION ==================*)

acces :
    | THIS { }
    | SUPER { }
    | RESULT { }

largOpt :
    | separated_list(COMMA, expression) { }

expression :
    | ID { }
    | CSTE { }
    | LPAREN expression RPAREN { }
    | LPAREN IDCLASS expression RPAREN { }
    | expression DOT ID { }
    | acces { }
    | NEW IDCLASS LPAREN largOpt RPAREN { }
    | expression DOT ID LPAREN largOpt RPAREN { }
    | expression PLUS expression { }
    | expression MINUS expression { }
    | expression TIMES expression { }
    | expression DIV expression { }
    | expression RELOP expression { }
    | expression AMPERSAND expression { }


(*================= DECLARATION INSTRUCTION =================*)

instruction :
    | expression SEMICOLON { }
    | bloc { }
    | RETURN SEMICOLON { }
    | expression WALRUS expression SEMICOLON { }
    | IF expression THEN instruction ELSE instruction { }

(*============= DECLARATION BLOC =====================*)

bloc :
    | LCURL  list(instruction) RCURL { }
    | LCURL nonempty_list(declaVar) IS nonempty_list(instruction) RCURL { }

declaVar :
    | separated_list(COMMA, ID) COLON IDCLASS SEMICOLON { }
































