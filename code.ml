(*open Ast
open Verif


(*  CONVENTIONS



*)



(* prochainement ici vos fonctions pour la génération de code
 * chan: canal ("channel") dans lequel écrire (associé à un fichier ouvert
 * en écriture dans lequel on veut stocker le code produit.
 * Voir dans main.ml
 *)

let codeOP op : opComp -> string =
    match op with
        | Eq    -> "EQUAL"
        | Neq   -> "EQUAL\nNOT"
        | Lt    -> "INF"
        | Le    -> "INFEQ"
        | Gt    -> "SUP"
        | Ge    -> "SUPEQ"


(*
//////////////////////////////////////EXPRESSION
*)
let rec codeEXPT exTy e : expType -> env -> string =
    match exTy with
        | Id x      -> "PUSHG " ^ x ^ "\n"(*STOREL x   <- dans instruction pour Assign*)
        | IdClass x -> "PUSHG " ^ x ^ "\n" (*pointeur*)
        | Cstes s   -> "=> CSTES " ^ s ^ "\n"
        | Cste x    -> "=> CSTE " ^ x ^ "\n"

        | This   -> "=> THIS\n"(*ATTRIBUT*)
        | Super  -> "=> SUPER\n"(*ATTRIBUT*)
        | Result -> "=> RESULT\n"(*ATTRIBUT*)

        | Plus (e1, e2)      -> (codeEXPT e1) ^ (codeEXPT e2) ^ "ADD\n"
        | Minus (e1, e2)     -> (codeEXPT e1) ^ (codeEXPT e2) ^ "SUB\n"
        | Times (e1, e2)     -> (codeEXPT e1) ^ (codeEXPT e2) ^ "MUL\n"
        | Div (e1, e2)       -> (codeEXPT e1) ^ (codeEXPT e2) ^ "DIV\n"
        | Comp (op, e1 ,e2)  -> (codeEXPT e1) ^ (codeEXPT e2) ^ (codeOP op) ^ "\n" 

        | Exp e            -> (codeEXPT e) ^ "\n"
        | ExpString (e, s) -> (codeEXPT e) ^ "PUSHS " ^ s ^ "\n"
        | ExpClass (s, le) -> "=> CAST " ^ (codeListeEXPT le) ^ "\n" (* PUSHA =nomLabel= *)

        | ExpMethode (e, s, le) -> (code e) ^ (* pointeur : label ^*)  (codeEXPT le) ^ "\n"

        | Relop (e1, e2)     -> (codeEXPT e1) ^ (codeEXPT e2) ^ "\n" 
        | Ampersand (e1, e2) -> (codeEXPT e1) ^ (codeEXPT e2) ^ "\n"
        (*| CallSuper (s, le)  -> ??????? ^(codeListeEXPT le) ^ "\n" *)
       
and
rec codeListeEXPT listExTy =
    match listExpTy with 
        | e::le -> (codeEXPT e) ^ (codeListeEXPT le) ^ "\n"
        | []    -> "NOP\n"     
(*
//////////////////////////////////////INSTRUCTION
*)

let rec codeINST inst =
    match inst with
        | Epr e         -> (codeEXPT e) ^ "\n" 
        | Bloc(ld,li)   -> codeDe
        | ReturnSemi    -> 
        | Assign(e1,e2)  -> (codeEXPT e2) ^ (codeEXPT e1) ^ "STOREG " ^ e1 ^ "\n" 
        | ITE(e,i1,i2)  -> (codeEXPT e) ^ (codeINST i1) ^ (codeINST i2) ^ "\n" /////////// A REVOIR car condition

and
codeListeINST listInst =
    match listInst with 
        | i::li -> (codeINST i)  ^ (codeListeINST li) ^ "\n" 
        | []    -> "NOP\n"            


and
codeListeDeclaV ldec =
    (* pour un declav*)
    let codeDeClaV dec = 
        let (listString, str) = dec in
        match listString with 
            | s::ls -> ?????????????
            | []    -> "NOP\n" 

    in
    (* pour une liste de declav*)
    match ldec with
        | d::ld -> codeDeclaV d ^codeListeDeclaV ld
        | []    -> "NOP\n" 

(*
//////////////////////////////////////METHODE
*)
let codeListeParam listP =
    let codePARAM param =
        let (lstr , str) = param in
        match lstr with
            | s::ls -> ??????????????????
            | []    -> "NOP\n" 
    in
    match listP with
        | p::lp -> codePARAM p ^codeListeParam lp
        | [] -> "NOP\n" 

let codeMETH meth =
    let (over, stat, nom, lparam, strOP, exp, inst) = meth in
    let code = ???? (* pour le nom de la méthode*)
    in
    
    



(*
//////////////////////////////////////FONCTION PRINCIPALE DE GENERATION DU CODE
*)
*)
let genCode ast  chan = ()
  

