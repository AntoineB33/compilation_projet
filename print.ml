open Ast

let string_of_relop (op: Ast.opComp) : string =
  match op with
    Eq -> " = "
  | Neq -> " <> "
  | Lt -> " < "
  | Le -> " <= "
  | Gt -> " > "
  | Ge -> " >= "

(* imprime une expression sous forme entierement parenthésée, de façon à
 * ce qu'on puisse facilement verifier si les précédences et associativités
 * demandées sont bien respectées.
 *)

let rec print_expr (e: expType) : unit =
  match e with
    | This -> print_string "<THIS> "
    | Super -> print_string "<SUPER> "
    | Result -> print_string "<RESULT> "    
    | Id s -> print_string s; print_string " "
    | IdClass s -> print_string s; print_string " "
    | Cstes s -> print_string s; print_string " "
    | Cste i -> print_int i; print_string " "
    | Plus(g, d) ->
       print_string "["; print_expr g; print_string " + ";
       print_expr d; print_string "] "
    | Minus (g, d) ->
       print_string "["; print_expr g; print_string " - ";
       print_expr d; print_string "] "
    | Times (g, d) ->
       print_string "["; print_expr g; print_string " * ";
       print_expr d; print_string "] "
    | Div (g, d) ->
       print_string "["; print_expr g; print_string " / ";
       print_expr d; print_string "] "
    | Comp(op, g, d) ->
       print_string "["; print_expr g;
       print_string (string_of_relop op); print_expr d; print_string "] "
    | ExpString(e, s) ->
       print_string "["; print_expr e; print_string "] . [";
       print_string s; print_string "] "
    | ExpClass(s, li) ->
       print_string "( "; print_string s; print_string " <CAST> ";
       List.iter print_expr li; print_string ") "
    | ExpMethode(e, s, el) -> 
       print_string s; print_string "<"; print_expr e ; print_string ">(";
       List.iter print_expr el; print_string ")"
    | Relop(e1, e2) -> print_expr e1; print_string " <RELOP?> "; print_expr e2
    | Ampersand(e1, e2) -> print_expr e1; print_string " & "; print_expr e2
    | CallSuper(s, el) ->
       print_string " : "; print_string s; print_string "(";
       List.iter print_expr el; print_string ")"

let rec print_tab (n: int) : unit =
    if n > 0
    then (print_string "\t"; print_tab (n - 1))

let rec print_decl (tab: int) (d: declaV) : unit =
    print_tab tab; match d with (sl, s) -> ()

let rec print_class (c: classe) : unit =
      match c with
         | (class_name, params, parent0, lchamp, lmeth) -> 
            print_string "<CLASS> "; print_string class_name; print_string " <EXTENDS> ";
            if parent0 <> None then
            print_string parent0; print_string " <BEGIN>\n";
            List.iter (print_decl 1) lchamp;
            List.iter (print_decl 1) lmeth;
            print_string "<END>\n"



let rec print_instruct (tab: int) (i: instruc) : unit = match i with
    | Epr e -> print_tab tab; print_expr e; print_string ";\n"
    | Bloc(dl, il) -> 
        print_tab tab; print_string "{\n";
        if dl <> [] then (List.iter (print_decl (tab + 1)) dl; print_string " <IS>\n");
        List.iter (print_instruct (tab + 1)) il; print_tab tab; print_string "}\n"
    | ReturnSemi -> print_tab tab; print_string "<RETURN>;\n"
    | Assign(e1, e2) -> print_tab tab; print_expr e1; print_string " := "; print_expr e2
    | ITE(cond, theni, elsei) ->
        print_tab tab; print_string "<IF>("; print_expr cond; print_string ")\n";
        print_tab tab; print_string "<THEN> {\n"; print_instruct (tab + 1) theni;
        print_tab tab; print_string "}\n<ELSE> {\n"; print_instruct (tab + 1) elsei; 
        print_tab tab; print_string "}\n"

let print_all ast =
    match ast with
        |(lc,i) ->  List.iter print_class lc;
                    print_string "<BEGIN> "; print_instruct 0 i; print_string " <END>\n"




