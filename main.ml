open Ast
open Lexing

let parse_with_error lexbuf file_in chan =
  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "%s:%d:%d" file_in
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
  try
   let _ = TpParse.prog TpLex.token lexbuf in
   let _ = Verif.runVC () in
   let _ = Code.genCode () chan in
   print_endline "\nCompilation Done !"
  with
    Decl_error msg ->
     print_string "Declaration error. "; print_endline msg;
     print_endline "Compilation aborted";
   | VC_error msg ->
      print_string "Contextual error. "; print_endline msg;
      print_endline "Compilation aborted";
   | Internal_error msg ->
      print_string "Internal error. "; print_endline msg;
      print_endline "Compilation crashed";
   | TpParse.Error ->
      Printf.fprintf stderr "Syntax error at position %a\n" print_position lexbuf

let _ =
  let argc = Array.length Sys.argv in
  if argc = 1 then
    print_endline "usage: ./tp programme-source [fichier-pour-le-code]"
  else
    try
      (* par défaut on ecrit le code dans le fichier "out.txt" *)
      let file_out = if argc = 3 then Sys.argv.(2) else "out.txt"
      and file_in = Sys.argv.(1) in
      let chan_in = open_in file_in
      and chan_out = open_out file_out in
      let lexbuf = Lexing.from_channel chan_in in
      parse_with_error lexbuf file_in chan_out;
      close_in chan_in; close_out chan_out
    with Sys_error msg-> (* Probleme avec les fichiers ? *)
      Printf.fprintf stderr "Compilation aborted. %s\n " msg
