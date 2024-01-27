type opComp = Eq | Neq | Lt | Le | Gt | Ge


type 
(*



type classe = {nomClass : string; lparamOpt : param list; heriteOpt : bool; construcCorpsOpt : construcCorpsOpt}

type param = string list

type construcCorpsOpt = 
    | A of lchamp*lmeth*constructeur*lmeth
    | B of lchamp*lmeth
and
lmeth = methode list
and
lchamp = champ list
and
constructeur = {nomClass : string; lparamOpt : param list; superOpt : bool*string*(param list); bloc : bloc}

(*type champ = boolean*boolean*string*)
(*type champ = boolean*((boolean*string) list)*string*)
and
champ = {isStatic : bool; noms : (bool*string) list; nomClasse : string}


*)




(*
methode =  {doesOverride : bool; isStatic : bool; nom : string; lparamOpt : param list; nomClass : string option; bloc : instruc}
and*)
classe = string* (param list) *(string option) * lchamp * lmeth
and
lmeth = methode list
and
lchamp = champ list
and
champ = bool * (bool*string) list * string
and
ident = bool option * string
and
methode =  bool*bool*string*(param list)*(string option)*(expType option) *instruc
and 
param = string list* string

and
expType =
        This | Super | Result (* correspond a acces *)
    | Id of string
    | IdClass of string
    | Cstes of string
    | Cste of int
    | Plus of expType*expType
    | Minus of expType*expType
    | Times of expType*expType
    | Div of expType*expType
    | Comp of opComp*expType*expType
    (*| Exp of expType*)
    | ExpString of expType*string
    (*| ExpClass of string*expType            cas particulier de la ligne d'en dessous*)
    | ExpClass of string*(expType list)
    | ExpMethode of expType*string*(expType list)
    | Relop of expType*expType
    | Ampersand of expType*expType
    | CallSuper of string*(expType list)
and 
instruc = 
    | Epr of expType
    | Bloc of declaV list * (instruc list)
    | ReturnSemi  (*pas besoin de None car juste une constante*)
    | Assign of expType*expType
    | ITE of expType*instruc*instruc
(* and 
bloc = declaV list * (instruc list)
*)
and 
declaV = string list*string  


type progType = classe list * instruc

exception Decl_error of string
exception VC_error of string
exception Internal_error of string

  





(*


largOpt :
    | s = separated_list(COMMA, expression) { s }

expression : 
    | nom = ID { ID(nom) }
    | x = CSTE { CSTE(x) }
    | LPAREN e = expression RPAREN { e }
    | LPAREN nomClasse = IDCLASS e = expression RPAREN { (nomClasse, e) }
    | e = expression DOT nomChamp = ID  { (e, nomChamp) }  
    | a = acces { a }
    | NEW nomClasse = IDCLASS del = delimited(LPAREN, largOpt, RPAREN) { ExpClass(nomClasse, del) }
    | e = expression DOT nomMethode = ID LPAREN lO = largOpt RPAREN { ExpMethode(e, nomMethode, lO) }
    | g = expression PLUS d = expression { PLUS(g,d) }
    | g = expression MINUS d = expression { MINUS(g,d) }   
    | g = expression TIMES d = expression { Times(g,d) }
    | g = expression DIV d = expression { DIV(g,d) } 
    | expression RELOP expression { }
        (*manque des operateurs*)


(*================= DECLARATION INSTRUCTION =================*)

instruction : 
    | e = expression SEMICOLON { Epr = e }
    | b = bloc { B(b) }
    | RETURN SEMICOLON { ReturnSemi }
    | nomVar = expression WALRUS e = expression SEMICOLON { Asign(nomVar, e) }
    | IF i = expression THEN t = instruction ELSE e = instruction { ITE(i,t,e) }


*)













