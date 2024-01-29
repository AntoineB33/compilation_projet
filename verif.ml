(*open Ast

type id = string
type idClass = string

type valueType = INT of int | STR of string | CLASS of idClass

module IdMap = Map.Make(struct
  type t = id
  let compare = Stdlib.compare
end)

module IdClassMap = Map.Make(struct
  type t = idClass
  let compare = Stdlib.compare
end)

type champData = {
  name : string;
  value : valueType;
}

type methodeData = {
  name : string;
  param : valueType list;
  returnType : valueType;

  (*override : bool;*)
  static : bool;
}

type classData = {

(*compile pas à cause de IdMap*)

  champ : string*champData IdMap.t;
  methode : methodeData IdMap.t;


  construct : methodeData option;
  parent : idClass option;
}


(*pb a cause de IdClassMap*)

type env = classData IdClassMap.t






(* Pour cast *)
let rec is_child_of (idClassChild : idClass) (idClassParent : idClass) (e : env)  : bool =
  match IdClassMap.find_opt idClassChild e with
    | None -> false
    | Some c ->
        match c.parent with
            | None -> false
            | Some p -> if idClassParent = p
            then true
            else is_child_of p idClassParent e



let is_methode (methode : methodeData) (idMethode : id)  (lparam : valueType list) (typeRetour : valueType ) : bool =

    let verif_param (p1 : valueType) (p2 : valueType) : bool =
            match (p1,p2) with
                | (INT i1, INT i2)  -> i1 = i2
                | (STR i1, STR i2)  -> i1 = i2
                | (CLASS i1,CLASS i2) -> i1 = i2
                | _ -> false
    in
    let rec verif_lParam (lp1 : valueType list) (lp2 : valueType list) : bool =
        if List.length lp1 = List.length lp2
        then match (lp1,lp2) with
            | ([],[]) -> true
            | (p1::tp1,p2::tp2)->
                if (verif_param p1 p2)
                then (verif_lParam tp1 tp2)
                else false
            | _ -> false
        else false
    in
    
       
    if (verif_param typeRetour methode.returnType)
        && (idMethode = methode.name)
    then verif_lParam methode.param lparam   
    else false


    
    

(*parent uniquement*)        
let rec is_methode_of_parent (e : env) (idClassNow : idClass) (idMethode : id) (lparam : valueType list) (typeRetour : valueType ) : bool =
    (* Recherche dans le parent suivant *)
    let rec search_in  (e : env) (idClassOPT : idClass option) (idMethode : id) (lparam : valueType list) (typeRetour : valueType ) : bool =
        match idClassOPT with
        | None -> false
        | Some i -> is_methode_of_parent e i idMethode lparam typeRetour
    in
    
    (* Récupération des informations de la classe *)
    let give_ClassData  (e : env) (idClassOPT : idClass option): classData option =
        match idClassOPT with
        | None -> None (*pas de recherche possible*)
        | Some i -> IdClassMap.find_opt i e (*recherche dans la base*)
     in

    (* chercher la class dans map*)
    match IdClassMap.find_opt idClassNow e with
    | None   -> false     (*la classe n'existe pas*)
    | Some dn ->
        match give_ClassData e dn.parent with
        | None -> false (*pas de parent ou pas trouvé dans la base*)
        | Some dp -> (*parent trouvé, recherche de la methode dans le parent*)
                match IdMap.find_opt idMethode dp.methode with
                | None -> (*on cherche dans le prochain parent*)
                    search_in e dp.parent idMethode lparam typeRetour
                | Some m -> if is_methode m idMethode lparam typeRetour
                        then true  (* méthode trouvée*)
                        else (*on cherche dans le prochain parent*)
                        search_in e dp.parent idMethode lparam typeRetour

     
(*classe uniquement sans parent*)
let rec is_methode_of (e : env) (idClassNow : idClass) (idMethode : id) (lparam : valueType list) (typeRetour : valueType )  : bool=
  (* chercher la class dans map*)
  match IdClassMap.find_opt idClassNow e with
    | None   -> false     (*la classe n'existe pas*)
    | Some dn -> (*recherche de la methode*)
        match IdMap.find_opt idMethode dn.methode with
        | None -> false
        | Some m -> if is_methode m idMethode lparam typeRetour
                    then true  (* méthode trouvée*)
                    else false
    
let rec can_override (e : env) (idClassNow : idClass) (idMethode : id)  (lparam : valueType list) (typeRetour : valueType )  : bool =
  is_methode_of_parent e idClassNow idMethode lparam typeRetour
*)
let runVC _ = ()
