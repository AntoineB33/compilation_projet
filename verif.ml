open Ast

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
  param : champData list;
  returnType : valueType;

  (*override : bool;*)
  static : bool;
}

type classData = {
  champ : champData list;
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



let is_methode (methode : methodeData) (idMethode : id)  (lparam : champData list) (typeRetour : valueType ) : bool =
  (*verification du nom du param*)  
  let verif_name_param (p1 : string) (p2 : string) : bool =
      if p1 = p2 then true else false
  in
  (*verification du type du param*)
  let verif_type_param (p1 : valueType) (p2 : valueType) : bool =
          match (p1,p2) with
              | (INT i1, INT i2)  -> i1 = i2
              | (STR i1, STR i2)  -> i1 = i2
              | (CLASS i1,CLASS i2) -> i1 = i2
              | _ -> false
  in
  (*verification du param : nom + type*)
  let verif_param (p1 : champData) (p2 : champData) : bool =
    if (verif_type_param p1.value p2.value) then (verif_name_param p1.name p2.name)
    else false
  in
  (*verifiaction de la liste de param*)
  let rec verif_lParam (lp1 : champData list) (lp2 : champData list) : bool =
    if List.length lp1 = List.length lp2
    then match (lp1,lp2) with
        | ([],[]) -> true
        | (p1::tp1,p2::tp2)->
            if (verif_name_param p1.name p2.name)
            then  (verif_lParam tp1 tp2)
            else false
        | _ -> false
    else false
  in
    
  (*verification du retour et nom de la methode*)     
  if (verif_type_param typeRetour methode.returnType)
      && (idMethode = methode.name)
  
  then verif_lParam methode.param lparam   
  else false


    
    

(*parent uniquement*)        
let rec is_methode_of_parent (e : env) (idClassNow : idClass) (idMethode : id) (lparam : champData list) (typeRetour : valueType ) : bool =
    (* Recherche dans le parent suivant *)
    let rec search_in  (e : env) (idClassOPT : idClass option) (idMethode : id) (lparam : champData list) (typeRetour : valueType ) : bool =
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
let rec is_methode_of (e : env) (idClassNow : idClass) (idMethode : id) (lparam : champData list) (typeRetour : valueType )  : bool=
  (* chercher la class dans map*)
  match IdClassMap.find_opt idClassNow e with
    | None   -> false     (*la classe n'existe pas*)
    | Some dn -> (*recherche de la methode*)
        match IdMap.find_opt idMethode dn.methode with
        | None -> false
        | Some m -> if is_methode m idMethode lparam typeRetour
                    then true  (* méthode trouvée*)
                    else false
    
let rec can_override (e : env) (idClassNow : idClass) (idMethode : id)  (lparam : champData list) (typeRetour : valueType )  : bool =
  is_methode_of_parent e idClassNow idMethode lparam typeRetour




let is_constructor_of (e :env) (idClassNow : idClass) (idConstruct : id) (lparam : champData list) : bool =
  match IdClassMap.find_opt idClassNow e with
  | None  -> false
  | Some dn -> 
      match dn.construct with
      | None -> false (*zero constructeur*)
      | Some c -> if is_methode c idConstruct lparam c.returnType
          then true
          else false



let has_constructor (e: env) (idClassNow : idClass) : bool = 
  match IdClassMap.find_opt idClassNow e with
  | None -> false
  | Some dn ->
      match dn.construct with
      | None -> false (* creer un constructeur*)
      | Some mapConst -> true

      
let runVC2 ast =
  (match ast with
    |(class_decl::rest, instruc) -> 
        (match class_decl with
        |(class_name, params, parent, lchamp, lmeth) ->
            let rec getChamps lchamp0 = 
              (match lchamp0 with
                  (o1, l0, n) -> raise (VC_error "Method declarations in class are not valid."))
            in
            let champ0 = getChamps lchamp in
            raise (VC_error "Method declarations in class are not valid.")
        | _ -> raise (VC_error "Method declarations in class are not valid."))
    | _ -> raise (VC_error "Method declarations in class are not valid."))

let runVC ast =
  let rec runVCRec env ast =
    (* Helper function to check if a given method is valid in the current class or its parents *)
    let rec check_method_validity e idClassNow idMethode lparam typeRetour =
      if is_methode_of e idClassNow idMethode lparam typeRetour then
        true
      else if can_override e idClassNow idMethode lparam typeRetour then
        true
      else
        false
    in

    (match ast with
      (class_decl::rest, instruc) ->
        (match class_decl with
          (class_name, params, parent, lchamp, lmeth) ->
            let rec getIdent env l0 vlt =
              (match l0 with
                ((o1,n)::rest) ->
                  let env = env @ [(n, vlt)]
                  in getIdent env rest vlt
                | [] -> env)
            in
            let rec getChamps env lchamp0 = 
              (match lchamp0 with
                ((o1, l0, n)::rest) -> 
                  let env = getIdent env l0 n
                  in getChamps env lchamp0
                | [] -> env
              )
            in
            let champ0 = getChamps [] lchamp
            in
            let class_decl : classData = {
              champ = champ0;
              methode = IdMap.empty;
              construct = None;
              parent = parent;
            } in
            let env = IdClassMap.add class_name class_decl env in
            (* Check the validity of method declarations *)
            let check_methods_validity e class_decl =
              let check_method_validity_wrapper idMethode methode_data =
                true
                (* let lparam = methode_data.param in
                let typeRetour = methode_data.returnType in
                if check_method_validity e class_name idMethode lparam typeRetour then
                  true
                else
                  raise (VC_error ("Method '" ^ idMethode ^ "' in class '" ^ class_name ^ "' is not valid.")) *)
              in
              IdMap.for_all check_method_validity_wrapper class_decl.methode
            in

            if check_methods_validity env class_decl then
              runVCRec env (rest, instruc)
            else
              raise (VC_error "Method declarations in class are not valid.")
          | _ -> Printf.printf "Handling other cases\n")
      | _ -> Printf.printf "Handling other cases\n")
  in
  runVCRec IdClassMap.empty ast