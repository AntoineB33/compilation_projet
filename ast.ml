type opComp = Eq | Neq | Lt | Le | Gt | Ge


type 
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
and 
declaV = string list*string  


type progType = classe list * instruc

exception Decl_error of string
exception VC_error of string
exception Internal_error of string