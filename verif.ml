open Ast

type id = string
type idClass = string

type valueType = VOID | INT | STR | CLASS of idClass

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

type env = classData IdClassMap.t

(*environement par defaut*)
let init_env () = 
    let e = IdClassMap.empty in

    (*declarer par defaut Integer avec la methode toSrting() *)
    let toString : methodeData = {name= "toString"; param= []; returnType= STR; static= false} in
    let integerMethodes = IdMap.empty in
    let integer : classData = {
        champ= [];
        methode= (IdMap.add "toString" toString integerMethodes);
        construct= None; (* pas besoin car on ne peut appeler Integer *)
        parent= None
    } in

    (*declarer par defaut String avec les methodes print() println() *)
    let print : methodeData = {name= "print"; param= []; returnType= VOID; static= false} in
    let println : methodeData = {name= "println"; param= []; returnType= VOID; static= false} in
    let strMethodes = IdMap.empty in
    let str : classData = {
        champ= [];
        methode= (IdMap.add "print" print (IdMap.add "println" println strMethodes));
        construct= None; (* pas besoin car on ne peut appeler String *)
        parent= None
    } in

    IdClassMap.add "Integer" integer (IdClassMap.add "String" str e)

(*=======================================================================*)

let add_methode (e :env) (idClassNow : idClass) (cl : classData) (listmd : methodeData list) : env =
  (*fonction pour ajouter les nouvelles methodes*)
  let add_list_map map list =
    List.fold_left(
      fun acc element ->
        IdMap.add element.name element acc
      )map list
  in
  (*nouvelle map de methode*)
  let mapMethode = add_list_map cl.methode listmd in
  (*classe avec les nouvelles méthodes*)
  let newcl : classData = {cl with methode = mapMethode } in
  (*mise à jour environement*)
  let newC e key newElement = 
    IdClassMap.add key newElement (IdClassMap.remove key e)
  in
  newC e idClassNow newcl

let create_construct (e : env) (idClassNow : idClass) (idConstruct : id) : env =
  let add_const (cd : classData) (idConstruct : id) : env =
    (* création du constructeur *)
    let constructData : methodeData = {name = idConstruct; param = cd.champ; returnType = CLASS(idClassNow); static = false} in
    (* création de la classe mise à jour *)
    let newClassData = {cd with construct = Some constructData} in
    (* ajout de la classData à l env *)
    IdClassMap.add idClassNow newClassData (IdClassMap.remove idClassNow e)
  in
  match IdClassMap.find_opt idClassNow e with
  | None -> e
  | Some cd ->
    match cd.construct with
    | None -> add_const cd idConstruct (* n'existe pas, donc : créer constructeur *)
    | Some c -> e   

(*=======================================================================*)

(* arg1 isChildOf arg2 in arg3[environment] *)
let rec is_child_of (idClassChild : idClass) (idClassParent : idClass) (e : env)  : bool =
  match IdClassMap.find_opt idClassChild e with
    | None -> false
    | Some c ->
        match c.parent with
            | None -> false
            | Some p -> if idClassParent = p
            then true
            else is_child_of p idClassParent e


(*verif si la method est dans la classe et si ses params sont correct*)
let is_methode (methode : methodeData) (idMethode : id)  (lparam : champData list) (typeRetour : valueType ) : bool =
  (*verification du nom du param*)  
  let verif_name_param (p1 : string) (p2 : string) : bool =
      if p1 = p2 then true else false
  in
  (*verification du type du param*)
  let verif_type_param (p1 : valueType) (p2 : valueType) : bool =
          match (p1,p2) with
              | (INT, INT)  -> true
              | (STR, STR)  -> true
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
            if (verif_param p1 p2)
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


    
    

(* isMethode dans les supers classes de arg2 *)        
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


let extend_is_looping (e: env) (idClass : idClass) : bool =
    match IdClassMap.find_opt idClass e with
        | None -> false
        | Some classe -> is_child_of idClass idClass e

let extend_exist (e: env) (idExtend : idClass) : bool = 
    match IdClassMap.find_opt idExtend e with
        | None -> false
        | Some classe -> true 

let extend_is_correct (e: env) (idClass : idClass) (idExtend : idClass) : bool =
    (*empeche l'heritage de integer et string*)
    if ( idExtend = "Integer" || idExtend = "String" )
    then false
    else
    match IdClassMap.find_opt idClass e with
        | None -> false
        | Some classe -> not (extend_is_looping e idClass) && (extend_exist e idExtend)


(*=======================================================================*)

let rec getIdent e l0 vlt =
  (match l0 with
    ((o1,n)::rest) ->
      let e = e @ [{name = n; value = CLASS vlt}]
      in getIdent e rest vlt
    | [] -> e)

let rec getChamps e lchamp0 = 
  (match lchamp0 with
    ((o1, l0, n)::rest) -> 
      let e = getIdent e l0 n
      in getChamps e rest
    | [] -> e
  )

let rec getSubParam e lO =
  (match lO with
    (par::rest, n) -> 
      let e = e @ [{name = par; value = CLASS n}]
      in getSubParam e (rest, n)
    |([], n) -> e
  )

let rec getParam e lO =
  (match lO with
    li::rest ->
      let e = getSubParam e li
      in getParam e rest
    | [] -> e
  )
  
let rec getMeth e construct0 lmeth0 class_name = 
  (match lmeth0 with
    ((o1, o2, n,lO ,optN, su, b)::rest) ->
      let lO = getParam [] lO in
      let optNC = (match optN with
        None -> VOID
        | Some n -> CLASS n
      ) in
      let construct0 =
        if n=class_name then
          (if construct0 = None then
            Some {name = n; param = lO; returnType = optNC; static = o2}
          else
            raise (VC_error "Constructeur défini plusieurs fois."))
        else
            construct0
      in
      let e = IdMap.add n {name = n; param = lO; returnType = optNC; static = o2} e
      in getMeth e construct0 rest class_name
    | [] -> e, construct0
  )

let rec check_method_validity e idClassNow idMethode lparam typeRetour =
  if is_methode_of e idClassNow idMethode lparam typeRetour then
    true
  else if can_override e idClassNow idMethode lparam typeRetour then
    true
  else
    false

let class_decl_is_correct e class_decl =
  (match class_decl with
    (class_name, params, parent, lchamp, lmeth) ->
      let champ0 : champData list = getChamps [] lchamp
      in
      let (methode0, construct0) = getMeth IdMap.empty None lmeth class_name
      in
      let class_decl : classData = {
        champ = champ0;
        methode = methode0;
        construct = construct0;
        parent = parent;
      } in
      let e = IdClassMap.add class_name class_decl e in
      
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

      if check_methods_validity e class_decl then
        e
      else
        raise (VC_error "Method declarations in class are not valid.")
    | _ -> e
  )

let runVC ast =
  let instruc_is_correct e instruc = e
  in
  let rec runVCRec e ast =
    (match ast with
      (class_decl::rest, instruc) -> 
        let e = class_decl_is_correct e class_decl
        in runVCRec e (rest,instruc)
      | ([],instruc) -> instruc_is_correct e ([],instruc)
      | _ -> e)
  in
  runVCRec (init_env ()) ast