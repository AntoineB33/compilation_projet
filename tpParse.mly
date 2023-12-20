%{
open Ast
%}

%token <string> ID IDCLASS
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token CONCAT
%token LPAREN RPAREN SEMICOLON
%token ASSIGN
%token IF THEN ELSE BEGIN END

%token EOF


(*=============== START GRAMMAIRE ===================*)

%start <Ast.progType> prog
%%
prog: list(classe) bloc EOF { }


(*================ CLASS ==================*)
%token CLASS
%token IS
%token EXTENDS
%token LCURL RCURL COMMA COLON (* RCURL : {   LCURL : } *)
%token AUTO STATIC
%token WALRUS (*  :=  (morse) *)
%token OVERRIDE

classe :
    | CLASS nomClass = IDCLASS LPAREN lparamOpt RPAREN heriteOpt IS LCURL construcCorpsOpt RCURL { }

lparamOpt:
    | separated_list(COMMA, param)

param :
    | separated_nonempty_list(COMMA, ID) COLON IDCLASS { }

(* class mere *)
heriteOpt :
    | option(EXTENDS nomClassParent = IDCLASS)


construcCorpsOpt :
    | construcOblCorpsOpt { }
    | lcorps

(* corp de la class *)
construcOblCorpsOpt :
    | champ construcOblCorpsOpt { }
    | methode construcOblCorpsOpt { }
    | constructeur lcorps { }

lcorps : list(corps)

corps : 
    | methode    
    | champ

(*=============== DECLARATION CONSTRUCTEUR ===================*)

constructeur : 
    | DEF nomClass = IDCLASS LPAREN lparamOpt RPAREN superOpt IS bloc { }

superOpt :
    | option(COLON nomClass = IDCLASS LPAREN lparamOpt RPAREN) { }

(*=============== DECLARATION CHAMP ===================*)

champ : option(STATIC) option(AUTO) nom = ID COLON nomClasse = IDCLASS SEMICOLON { }


(*================= DECLARATION METHODE =================*)


methode : DEF option(OVERRIDE) option(STATIC) nom = ID LPAREN lparamOpt RPAREN suiteMethode { }

suiteMethode : 
    | COLON nomClasse = IDCLASS WALRUS expression { }
    | classOPT IS bloc { }

classOpt :
    | option(COLON nomClasse = IDCLASS) { }


(*================ DECLARATION EXPRESSION ==================*)
%token THIS SUPER RESULT
%token DOT
%token NEW

acces :
    | THIS { }
    | SUPER { }
    | RESULT { }

largOpt :
    | separated_list(COMMA, expression) { }

expression : 
    | nom = ID { }
    | x = CSTE { }
    | LPAREN expression RPAREN { }
    | LPAREN nomClasse = IDCLASS expression RPAREN { }
    | expression DOT nomChamp = ID  { }  
    | acces
    | NEW nomClasse = IDCLASS LPAREN largOpt RPAREN { }
    | expression DOT nomMethode = ID LPAREN largOpt RPAREN { }
    | g = expression PLUS d = expression { }
    | g = expression MINUS d = expression { }   
    | g = expression TIMES d = expression { }
    | g = expression DIV d = expression { } 
        (*manque des operateurs*)


(*================= DECLARATION INSTRUCTION =================*)

instruction : 
    | expression SEMICOLON { }
    | bloc { }
    | RETURN SEMICOLON { }
    | nomVar = ID WALRUS expression SEMICOLON { }
    | IF expression THEN t = instruction ELSE e = instruction

(*============= DECLARATION BLOC =====================*)

bloc :
    | LCURL blocInner RCURL { }
    | LCURL bloc RCURL { }

blocInner : 
    | list(instruction) { }
    | nonempty_list(declaVar) IS nonempty_list(instruction) { }

declaVar :
    | separated_list(COMMA, nom = ID) COLON nomClasse = IDCLASS SEMICOLON { }
































