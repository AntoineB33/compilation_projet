%{
open Ast
%}

(*=============== DECLARATION TOKEN ===================*)

%token <string> ID IDCLASS CSTES
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

%left IDCLASS
%left AMPERSAND
%nonassoc RELOP
%left PLUS MINUS
%left TIMES DIV
%left DOT


(*=============== START GRAMMAIRE ===================*)

%start <Ast.progType> prog
%%
prog: lO = list(classe) b = bloc EOF { lO,b }


(*================ CLASS ==================*)


classe : 
    | CLASS n = IDCLASS LPAREN lO = lparamOpt RPAREN h = heriteOpt IS LCURL c = corpsClasse RCURL { 
let (lc, lm) = c in(n, lO, h, lc, lm)}

lparamOpt:
    | li = separated_list(COMMA, param) { li }

param :
    | li = separated_nonempty_list(COMMA, ID) COLON n = IDCLASS { (li,n) }

(* class mere *)
heriteOpt : 
    | o1 = option(extClasse) { o1 }

extClasse : EXTENDS n = IDCLASS { n }

corpsClasse :
    | lO = lchamp lI = lmeth { lO, lI }

lmeth : lO = list(methode) { lO }

lchamp : lO = list(champ) { lO }

(*=============== DECLARATION CHAMP ===================*)

champ : o1 = boption(STATIC) lO = separated_nonempty_list(COMMA,ident) COLON n = IDCLASS SEMICOLON { o1,lO,n  }

ident : o1 = boption(AUTO) n = ID { o1,n }

(*================= DECLARATION METHODE =================*)


methode : 
      DEF o1 = boption(OVERRIDE) o2 = boption(STATIC) n = ID LPAREN lO = lparamOpt RPAREN suite = suiteMethode { 
let ( optN, instr) = suite in ( o1, o2, n,lO ,optN, None, instr)}
(*{doesOverride = o1; isStatic = o2; nom = n; lparamOpt = lO; nomClass = s; bloc = [],[]}*)
   
     | DEF n = IDCLASS LPAREN lO = lparamOpt RPAREN su = option(super) IS b = bloc { 
(*
let (supN, supL) = (Some su) in(None, None, n, lO, (supN, supL), B(b) )

let (supN, supL) = su in(None, None, n, lO, supN, supL, B(b) )
*)
      (false, false, n, lO, None, su, b)
}




super : COLON n = IDCLASS LPAREN lO = largOpt RPAREN { CallSuper(n, lO) }


suiteMethode :
    | COLON n = IDCLASS WALRUS e = expression { (Some n), Epr(e) }
    | c = option(estClass) IS b = bloc { c, b }

estClass : COLON n = IDCLASS {n}



(*================ DECLARATION EXPRESSION ==================*)

acces :
    | THIS { This }
    | SUPER { Super }
    | RESULT { Result }

largOpt :
    | s = separated_list(COMMA, expression) { s }

expression : 
    | nom = ID { Id(nom) }
    | nom = IDCLASS { IdClass(nom)}
    | x = CSTE { Cste(x) }
    | x = CSTES { Cstes(x) } 
    | LPAREN e = expression RPAREN { e }
    | LPAREN nomClasse = IDCLASS e = expression RPAREN { ExpClass(nomClasse, [e]) } (*cast*)
    | e = expression DOT nomChamp = ID  { ExpString(e, nomChamp) }  
    | a = acces { a }
    | NEW nomClasse = IDCLASS del = delimited(LPAREN, largOpt, RPAREN) { ExpClass(nomClasse, del) }
    | e = expression DOT nomMethode = ID LPAREN lO = largOpt RPAREN { ExpMethode(e, nomMethode, lO) }
    | g = expression PLUS d = expression { Plus(g,d) }
    | g = expression MINUS d = expression { Minus(g,d) }
    | g = expression TIMES d = expression { Times(g,d) }
    | g = expression DIV d = expression { Div(g,d) }
    | e1 = expression RELOP e2 = expression { Relop(e1 ,e2) }
    | e1 = expression AMPERSAND e2 = expression { Ampersand(e1 ,e2)}
(* conflit avec operateur unaires *)
    | MINUS e = expression { Minus(Cste(0), e) }
    | PLUS e = expression { e }



(*================= DECLARATION INSTRUCTION =================*)

instruction : 
    | e = expression SEMICOLON { Epr(e) }
    | b = bloc { b }
    | RETURN SEMICOLON { ReturnSemi }
    | nomVar = expression WALRUS e = expression SEMICOLON { Assign(nomVar, e) }
    | IF i = expression THEN t = instruction ELSE e = instruction { ITE(i,t,e) }

(*============= DECLARATION BLOC =====================*)

bloc :
    | LCURL  li = list(instruction) RCURL { Bloc([],li) }
    | LCURL li1 = nonempty_list(declaVar) IS li2 = nonempty_list(instruction) RCURL { Bloc(li1,li2) }

declaVar :
    | l = separated_list(COMMA, ID) COLON n = IDCLASS SEMICOLON { (l,n) }
































