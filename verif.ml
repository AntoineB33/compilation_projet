open Ast


(**========================TYPE & STRUCT=====================================**)
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
  static : bool;
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
  param : champData list;
  construct : methodeData option;
  parent : idClass option;
}

type env = classData IdClassMap.t

(*environement par defaut*)
let init_env () : env = 
    let e = IdClassMap.empty in

    (*declarer par defaut Integer avec la methode toSrting() *)
    let toString : methodeData = {name= "toString"; param= []; returnType= STR; static= false} in
    let integerMethodes = IdMap.empty in
    let integer : classData = {
        champ= [];
        methode= (IdMap.add "toString" toString integerMethodes);
		param= [];
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
		param= [];
        construct= None; (* pas besoin car on ne peut appeler String *)
        parent= None
    } in

    IdClassMap.add "Integer" integer (IdClassMap.add "String" str e)

(**=======================METHODE ENV==========================================**)

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

(**======================METHODE VERIF====================================**)

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
  let is_not_static =
    (* chercher la class dans map*)
    match IdClassMap.find_opt idClassNow e with
      | None   -> false
      | Some dn -> (*recherche de la methode*)
          match IdMap.find_opt idMethode dn.methode with
          | None -> false
          | Some m -> if is_methode m idMethode lparam typeRetour
                      then (m.static = false)
                      else false 
  in
  is_not_static && is_methode_of_parent e idClassNow idMethode lparam typeRetour





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

let is_type_class_exist (class_name : idClass) (e : env) : bool =
	match (IdClassMap.find_opt class_name e) with
		| None -> false
		| Some class_type -> true

(**=======================================================================**)
(**===========================ETATS=======================================**)
(**=======================================================================**)

(*===========================CHAMP=======================================*)

let rec cherche_doublon_champs (args : id list) : bool = 
	match args with
		| [] -> false
		| a::al -> if List.exists (fun x -> a = x) al then true else cherche_doublon_champs al

let rec id_extraction (id_boolean_list : (bool * id) list ) (res : id list) : id list =
	match id_boolean_list with
		| [] -> res
		| (o1,id)::ibs -> id_extraction ibs (id::res)

let champ_extraction (champ : bool * (bool * id) list * id) (e : env) : id list =
    match champ with
        | (_,[],_) -> [] (*internal error*)
        | (o1,id_boolean_list,type_name) -> if is_type_class_exist type_name e then (id_extraction id_boolean_list []) else [] (*VC error type de class non trouve*)

let rec champs_list_extraction (champs_list : champ list) (res : id list) (e : env) : id list =
    match champs_list with
        | [] -> res
        | c::cs -> champs_list_extraction cs ((champ_extraction c e)@res) e	

let rec champ_list_is_correct (champs_list : champ list) (e : env) : bool =
	(*verif que les parametres non pas les meme noms et que le type existe*)

    (*1. recuperer et concatener toutes les listes d'arguments en meme temps verif leur type*)
    let list_id = champs_list_extraction champs_list [] e
	in
	(*2. chercher un doublon dans la liste final*)
    not (cherche_doublon_champs list_id)	
		
(*===========================METHOD======================================*)

let rec method_list_is_correct methode_list (e : env) (c : idClass) : bool = 
  match methode_list with
    | [] -> true
    | m::ms -> true

      (* is_methode_of e c m.name m.param m.returnType;
      method_list_is_correct ms e c *)

  

(*===========================PARAMETER===================================*)

let rec cherche_doublon (args : id list) : bool = 
	match args with
		| [] -> false
		| a::al -> if List.exists (fun x -> a = x) al then true else cherche_doublon al

let parameter_extraction (param : (id list) * id) (e : env) : id list =
    match param with
        | ([],_) -> [] (*internal error*)
        | (args_list,type_name) -> if is_type_class_exist type_name e then args_list else [] (*VC error type de class non trouve*)

let rec parameter_list_extraction (parameter_list : param list) (res : id list) (e : env) : id list =
    match parameter_list with
        | [] -> res
        | p::ps -> parameter_list_extraction ps ((parameter_extraction p e)@res) e

let parameter_list_is_correct (parameter_list : param list) (e : env) : bool =
    (*verif que les parametres non pas les meme noms et que le type existe*)

    (*1. recuperer et concatener toutes les listes d'arguments en meme temps verif leur type*)
    let list_arg = parameter_list_extraction parameter_list [] e
	in
	(*2. chercher un doublon dans la liste final*)
    not (cherche_doublon list_arg)
   
(*===========================EXTEND======================================*)

let extend_is_looping (e: env) (idClass : idClass) : bool =
    match IdClassMap.find_opt idClass e with
        | None -> false (*internal error*)
        | Some classe -> is_child_of idClass idClass e

let extend_exist (e: env) (idExtend : idClass) : bool = 
    match IdClassMap.find_opt idExtend e with
        | None -> false
        | Some classe -> true 

let extend_is_correct (idExtend : id option) (idClass : idClass) (e: env) : bool =
    match idExtend with 
        | None -> true
        | Some extend ->
            if ( extend = "Integer" || extend = "String" )
            then false (*empeche l'heritage de integer et string*)
            else
                match IdClassMap.find_opt idClass e with
                    | None -> false (*internal error*)
                    | Some classe -> not (extend_is_looping e idClass) && (extend_exist e extend)


(*===========================EXPRESSION====================================*)

let expression_is_correct (exp : expType) (e : env) : bool = true

(*===========================ASSIGN=======================================*)

let assign_is_correct (exp1 : expType) (exp2 : expType) (e : env) : bool = true (*on peut pas verifier le type des deux expression avec la structure*)

(*===========================DECLARATION==================================*)

let declar_list_is_correct (declar_list : declaV list) (e : env) : bool = parameter_list_is_correct declar_list e

(*===========================INSTRUCTION==================================*)

let rec instruction_is_correct (inst : instruc) (e : env) : bool = 
	match inst with 
  | _ -> true
		(* | RetrunSemi -> true *)
		(* | Epr of exp -> expression_is_correct exp e
		| Assign of (exp1,exp2) -> assign_is_correct exp1 exp2 e
		| ITE of (exp,inst1,inst2) -> expression_is_correct exp e && instruction_is_correct inst1 e && instruction_is_correct inst2 e
		| Bloc of (declar_list,instru_list) -> declar_list_is_correct declar_list e && 
			match instru_list with 
				| [] -> true
				| i::is -> if instruction_is_correct i e then else false *)



(*===========================CLASS========================================*)

let class_is_correct (c : id * (param list) * (id option) * lchamp * lmeth) (e : env) : bool =
    match c with
        | (name,param_list,extend_option,champ_list,method_list) ->  
                parameter_list_is_correct param_list e &&
                extend_is_correct extend_option name e &&
                champ_list_is_correct champ_list e &&
                method_list_is_correct method_list e name

let rec class_list_is_correct (cl : classe list) (e : env) : bool =
    match cl with 
        | [] -> true
        | c::cs ->  if (class_is_correct c e) 
                    then (class_list_is_correct cs e) 
                    else false

(*===========================INSTRUCTION==================================*)

let instruction_is_correct (inst : instruc) (e : env) : bool = false

(**==========================LECTURE DE L'ENV=============================**)

let rec getIdent e methode0 l0 vlt static =
  (match l0 with
    ((auto,n)::rest) ->
      let e = e @ [{name = n; value = CLASS vlt; static = static}]
      in
      let methode0 =
        if auto then
          IdMap.add n {name = n; param = []; returnType = CLASS vlt; static = false} methode0
        else
          methode0
      in
      getIdent e methode0 rest vlt static
    | [] -> e, methode0)

let rec getChamps e methode0 lchamp0 = 
  (match lchamp0 with
    ((static, l0, n)::rest) -> 
      let e, methode0 = getIdent e methode0 l0 n static
      in
      getChamps e methode0 rest
    | [] -> (e, methode0)
  )

let rec getSubParam e lO =
  (match lO with
    (par::rest, n) -> 
      let e = e @ [{name = par; value = CLASS n; static = false}]
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
    ((o1, o2, n, lO ,optN, su, b)::rest) ->
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

let addClassDecl e class_decl =
  (match class_decl with
    (class_name, lO, parent0, lchamp, lmeth) ->
      let (champ0, methode0) = getChamps [] IdMap.empty lchamp
      in
      let (methode0, construct0) = getMeth methode0 None lmeth class_name
      in
      let param0 = getParam [] lO
      in
      let class_decl : classData = 
        {
          champ = champ0;
          methode = methode0;
          param = param0;
          construct = construct0;
          parent = parent0;
        }
      in
      IdClassMap.add class_name class_decl e
  )
  
let rec recup_env e ast =
  match ast with
    |(class_decl::rest, instruc) -> 
      let e = addClassDecl e class_decl
      in
      recup_env e (rest,instruc)
    | _ -> e

(**==========================RUNVC========================================**)

let runVC (ast : classe list*instruc) : bool =
  let e = recup_env (init_env ()) ast
  in
  match ast with
      | ([],i) -> instruction_is_correct i e
      | (cl,i) -> (class_list_is_correct cl e) && (instruction_is_correct i e)
      | _ -> raise (VC_error "Le programme est vide.")
