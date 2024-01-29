
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WALRUS
    | TIMES
    | THIS
    | THEN
    | SUPER
    | STATIC
    | SEMICOLON
    | RPAREN
    | RETURN
    | RESULT
    | RELOP of (
# 25 "tpParse.mly"
       (Ast.opComp)
# 24 "tpParse.ml"
  )
    | RCURL
    | PLUS
    | OVERRIDE
    | NEW
    | MINUS
    | LPAREN
    | LCURL
    | IS
    | IF
    | IDCLASS of (
# 7 "tpParse.mly"
       (string)
# 38 "tpParse.ml"
  )
    | ID of (
# 7 "tpParse.mly"
       (string)
# 43 "tpParse.ml"
  )
    | EXTENDS
    | EOF
    | ELSE
    | DOT
    | DIV
    | DEF
    | CSTES of (
# 7 "tpParse.mly"
       (string)
# 54 "tpParse.ml"
  )
    | CSTE of (
# 8 "tpParse.mly"
       (int)
# 59 "tpParse.ml"
  )
    | COMMA
    | COLON
    | CLASS
    | AUTO
    | AMPERSAND
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState158
  | MenhirState155
  | MenhirState149
  | MenhirState142
  | MenhirState140
  | MenhirState134
  | MenhirState131
  | MenhirState127
  | MenhirState122
  | MenhirState119
  | MenhirState115
  | MenhirState113
  | MenhirState105
  | MenhirState102
  | MenhirState94
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState84
  | MenhirState83
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState53
  | MenhirState49
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState37
  | MenhirState33
  | MenhirState28
  | MenhirState24
  | MenhirState21
  | MenhirState12
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 1 "tpParse.mly"
  
open Ast

# 125 "tpParse.ml"

let rec _menhir_goto_separated_nonempty_list_COMMA_ident_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((bool * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDCLASS _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, (o1 : (bool))), _, (lO : ((bool * string) list))), (n : (
# 7 "tpParse.mly"
       (string)
# 154 "tpParse.ml"
                    ))) = _menhir_stack in
                    let _v : (Ast.champ) = 
# 73 "tpParse.mly"
                                                                                                   ( o1,lO,n  )
# 159 "tpParse.ml"
                     in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | STATIC ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                    | DEF | RCURL ->
                        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                    | AUTO | ID _ ->
                        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (bool * string))), _, (xs : ((bool * string) list))) = _menhir_stack in
        let _v : ((bool * string) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 201 "tpParse.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_methode_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.lmeth) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.lmeth)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.methode))) = _menhir_stack in
        let _v : (Ast.lmeth) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 218 "tpParse.ml"
         in
        _menhir_goto_list_methode_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lO : (Ast.lmeth)) = _v in
        let _v : (Ast.lmeth) = 
# 67 "tpParse.mly"
                           ( lO )
# 228 "tpParse.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lI : (Ast.lmeth)) = _v in
        let (_menhir_stack, _menhir_s, (lO : (Ast.lchamp))) = _menhir_stack in
        let _v : (Ast.lchamp * Ast.lmeth) = 
# 65 "tpParse.mly"
                             ( lO, lI )
# 237 "tpParse.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (n : (
# 7 "tpParse.mly"
       (string)
# 251 "tpParse.ml"
            ))), _, (lO : (Ast.param list))), (h : (string option))), _, (c : (Ast.lchamp * Ast.lmeth))) = _menhir_stack in
            let _v : (Ast.classe) = 
# 49 "tpParse.mly"
                                                                                                  ( 
let (lc, lm) = c in(n, lO, h, lc, lm))
# 257 "tpParse.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | LCURL ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_OVERRIDE_ : _menhir_env -> 'ttv_tail -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STATIC ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | ID _ ->
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119

and _menhir_goto_boption_AUTO_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (n : (
# 7 "tpParse.mly"
       (string)
# 311 "tpParse.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s, (o1 : (bool))) = _menhir_stack in
        let _v : (bool * string) = 
# 75 "tpParse.mly"
                                  ( o1,n )
# 317 "tpParse.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AUTO ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | ID _ ->
                _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (bool * string))) = _menhir_stack in
            let _v : ((bool * string) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 343 "tpParse.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_methode : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.methode) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | RCURL ->
        _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134

and _menhir_reduce51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.lmeth) = 
# 211 "<standard.mly>"
    ( [] )
# 380 "tpParse.ml"
     in
    _menhir_goto_list_methode_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDCLASS _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | RPAREN ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | OVERRIDE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 422 "tpParse.ml"
         in
        _menhir_goto_boption_OVERRIDE_ _menhir_env _menhir_stack _v
    | ID _ | STATIC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 430 "tpParse.ml"
         in
        _menhir_goto_boption_OVERRIDE_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 445 "tpParse.ml"
     in
    _menhir_goto_boption_AUTO_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 456 "tpParse.ml"
     in
    _menhir_goto_boption_AUTO_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_bloc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instruc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 | MenhirState113 | MenhirState102 | MenhirState105 | MenhirState89 | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (b : (Ast.instruc))) = _menhir_stack in
        let _v : (Ast.instruc) = 
# 145 "tpParse.mly"
               ( b )
# 471 "tpParse.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), (n : (
# 7 "tpParse.mly"
       (string)
# 480 "tpParse.ml"
        ))), _, (lO : (Ast.param list))), (su : (Ast.expType option))), _, (b : (Ast.instruc))) = _menhir_stack in
        let _v : (Ast.methode) = 
# 85 "tpParse.mly"
                                                                                   ( 
(*
let (supN, supL) = (Some su) in(None, None, n, lO, (supN, supL), B(b) )

let (supN, supL) = su in(None, None, n, lO, supN, supL, B(b) )
*)
      (false, false, n, lO, None, su, b)
)
# 492 "tpParse.ml"
         in
        _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (c : (string option))), _, (b : (Ast.instruc))) = _menhir_stack in
        let _v : (string option * Ast.instruc) = 
# 102 "tpParse.mly"
                                       ( c, b )
# 502 "tpParse.ml"
         in
        _menhir_goto_suiteMethode _menhir_env _menhir_stack _v
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lO : (Ast.classe list))), _, (b : (Ast.instruc))) = _menhir_stack in
            let _v : (Ast.progType) = 
# 42 "tpParse.mly"
                                     ( lO,b )
# 517 "tpParse.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.progType)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instruc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (li1 : (Ast.declaV list))), _, (li2 : (Ast.instruc list))) = _menhir_stack in
            let _v : (Ast.instruc) = 
# 154 "tpParse.mly"
                                                                                    ( Bloc(li1,li2) )
# 549 "tpParse.ml"
             in
            _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.instruc))), _, (xs : (Ast.instruc list))) = _menhir_stack in
        let _v : (Ast.instruc list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 565 "tpParse.ml"
         in
        _menhir_goto_nonempty_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_suiteMethode : _menhir_env -> 'ttv_tail -> (string option * Ast.instruc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (suite : (string option * Ast.instruc)) = _v in
    let (((((_menhir_stack, _menhir_s), (o1 : (bool))), _, (o2 : (bool))), (n : (
# 7 "tpParse.mly"
       (string)
# 579 "tpParse.ml"
    ))), _, (lO : (Ast.param list))) = _menhir_stack in
    let _v : (Ast.methode) = 
# 81 "tpParse.mly"
                                                                                                               ( 
let ( optN, instr) = suite in ( o1, o2, n,lO ,optN, None, instr))
# 585 "tpParse.ml"
     in
    _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expType list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState33 | MenhirState40 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expType list)) = _v in
        let _v : (Ast.expType list) = 
# 144 "<standard.mly>"
    ( x )
# 599 "tpParse.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expType list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.expType))) = _menhir_stack in
        let _v : (Ast.expType list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 610 "tpParse.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> (
# 25 "tpParse.mly"
       (Ast.opComp)
# 651 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | CSTES _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | IDCLASS _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | LPAREN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | MINUS ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | NEW ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | PLUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | RESULT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | SUPER ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | THIS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | RPAREN ->
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | AMPERSAND | COMMA | DEF | DIV | DOT | MINUS | PLUS | RCURL | RELOP _ | RPAREN | SEMICOLON | THEN | TIMES | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Ast.expType))), (nomChamp : (
# 7 "tpParse.mly"
       (string)
# 798 "tpParse.ml"
            ))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 125 "tpParse.mly"
                                        ( ExpString(e, nomChamp) )
# 803 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_goto_list_champ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.lchamp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (lO : (Ast.lchamp)) = _v in
        let _v : (Ast.lchamp) = 
# 69 "tpParse.mly"
                          ( lO )
# 893 "tpParse.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | RCURL ->
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.lchamp)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.champ))) = _menhir_stack in
        let _v : (Ast.lchamp) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 916 "tpParse.ml"
         in
        _menhir_goto_list_champ_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_STATIC_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | RPAREN ->
                    _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AUTO ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | ID _ ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_declaVar_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.declaV list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | CSTES _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IDCLASS _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IF ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LCURL ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | LPAREN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | MINUS ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | NEW ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | PLUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | RESULT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | RETURN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | SUPER ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | THIS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.declaV))), _, (xs : (Ast.declaV list))) = _menhir_stack in
        let _v : (Ast.declaV list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 1037 "tpParse.ml"
         in
        _menhir_goto_nonempty_list_declaVar_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instruc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (li : (Ast.instruc list))) = _menhir_stack in
            let _v : (Ast.instruc) = 
# 153 "tpParse.mly"
                                          ( Bloc([],li) )
# 1060 "tpParse.ml"
             in
            _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.instruc))), _, (xs : (Ast.instruc list))) = _menhir_stack in
        let _v : (Ast.instruc list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1076 "tpParse.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instruc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | CSTES _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | IDCLASS _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | IF ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LCURL ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LPAREN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | MINUS ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NEW ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | PLUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | RESULT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | RETURN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | SUPER ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | THIS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (i : (Ast.expType))), _, (t : (Ast.instruc))), _, (e : (Ast.instruc))) = _menhir_stack in
        let _v : (Ast.instruc) = 
# 148 "tpParse.mly"
                                                                  ( ITE(i,t,e) )
# 1141 "tpParse.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState105 | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | CSTES _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | ID _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | IDCLASS _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | IF ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LCURL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LPAREN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | NEW ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | RESULT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | RETURN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | SUPER ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | THIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.instruc))) = _menhir_stack in
            let _v : (Ast.instruc list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 1183 "tpParse.ml"
             in
            _menhir_goto_nonempty_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | MenhirState113 | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | CSTES _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | ID _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | IDCLASS _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | IF ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LCURL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LPAREN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEW ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | RESULT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | RETURN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SUPER ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | THIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | RCURL ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expType list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs : (Ast.expType list)) = _v in
    let _v : (Ast.expType list) = let s = 
# 232 "<standard.mly>"
    ( xs )
# 1240 "tpParse.ml"
     in
    
# 116 "tpParse.mly"
                                            ( s )
# 1245 "tpParse.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expType))), (nomMethode : (
# 7 "tpParse.mly"
       (string)
# 1261 "tpParse.ml"
            ))), _, (lO : (Ast.expType list))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 128 "tpParse.mly"
                                                                    ( ExpMethode(e, nomMethode, lO) )
# 1266 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (nomClasse : (
# 7 "tpParse.mly"
       (string)
# 1287 "tpParse.ml"
            ))), _, (x : (Ast.expType list))) = _menhir_stack in
            let _v : (Ast.expType) = let del = 
# 200 "<standard.mly>"
    ( x )
# 1292 "tpParse.ml"
             in
            
# 127 "tpParse.mly"
                                                                       ( ExpClass(nomClasse, del) )
# 1297 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (n : (
# 7 "tpParse.mly"
       (string)
# 1318 "tpParse.ml"
            ))), _, (lO : (Ast.expType list))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 97 "tpParse.mly"
                                                     ( CallSuper(n, lO) )
# 1323 "tpParse.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (Ast.expType)) = _v in
            let _v : (Ast.expType option) = 
# 116 "<standard.mly>"
    ( Some x )
# 1331 "tpParse.ml"
             in
            _menhir_goto_option_super_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_acces : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (a : (Ast.expType)) = _v in
    let _v : (Ast.expType) = 
# 126 "tpParse.mly"
                ( a )
# 1351 "tpParse.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce18 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "tpParse.mly"
       (string)
# 1358 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (nom : (
# 7 "tpParse.mly"
       (string)
# 1364 "tpParse.ml"
    ))) = _menhir_stack in
    let _v : (Ast.expType) = 
# 120 "tpParse.mly"
                    ( IdClass(nom))
# 1369 "tpParse.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPERSAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RELOP _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (nomClasse : (
# 7 "tpParse.mly"
       (string)
# 1401 "tpParse.ml"
            ))), _, (e : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 124 "tpParse.mly"
                                                       ( ExpClass(nomClasse, [e]) )
# 1406 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | COMMA | DEF | DIV | MINUS | PLUS | RCURL | RELOP _ | RPAREN | SEMICOLON | THEN | TIMES | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (g : (Ast.expType))), _, (d : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 131 "tpParse.mly"
                                          ( Times(g,d) )
# 1430 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 | MenhirState40 | MenhirState68 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPERSAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | CSTES _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | IDCLASS _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LPAREN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | MINUS ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NEW ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | PLUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | RESULT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | SUPER ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | THIS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RELOP _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1495 "tpParse.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | COMMA | DEF | RCURL | RPAREN | SEMICOLON | THEN | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expType))), _), _, (e2 : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 133 "tpParse.mly"
                                            ( Relop(e1 ,e2) )
# 1525 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | COMMA | DEF | MINUS | PLUS | RCURL | RELOP _ | RPAREN | SEMICOLON | THEN | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (g : (Ast.expType))), _, (d : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 129 "tpParse.mly"
                                         ( Plus(g,d) )
# 1551 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | COMMA | DEF | DIV | MINUS | PLUS | RCURL | RELOP _ | RPAREN | SEMICOLON | THEN | TIMES | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (g : (Ast.expType))), _, (d : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 132 "tpParse.mly"
                                        ( Div(g,d) )
# 1573 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | COMMA | DEF | MINUS | PLUS | RCURL | RELOP _ | RPAREN | SEMICOLON | THEN | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (g : (Ast.expType))), _, (d : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 130 "tpParse.mly"
                                          ( Minus(g,d) )
# 1599 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RELOP _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | COMMA | DEF | RCURL | RPAREN | SEMICOLON | THEN | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expType))), _, (e2 : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 134 "tpParse.mly"
                                                ( Ampersand(e1 ,e2))
# 1631 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPERSAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RELOP _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 123 "tpParse.mly"
                                   ( e )
# 1665 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | COMMA | DEF | MINUS | PLUS | RCURL | RELOP _ | RPAREN | SEMICOLON | THEN | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 136 "tpParse.mly"
                           ( Minus(Cste(0), e) )
# 1693 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | COMMA | DEF | MINUS | PLUS | RCURL | RELOP _ | RPAREN | SEMICOLON | THEN | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.expType) = 
# 137 "tpParse.mly"
                          ( e )
# 1719 "tpParse.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPERSAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RELOP _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _v
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | CSTES _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | IDCLASS _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | IF ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LCURL ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LPAREN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | MINUS ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | NEW ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | PLUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | RESULT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | RETURN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | SUPER ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | THIS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 | MenhirState113 | MenhirState102 | MenhirState105 | MenhirState89 | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPERSAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RELOP _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _v
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.instruc) = 
# 144 "tpParse.mly"
                               ( Epr(e) )
# 1815 "tpParse.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | WALRUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | CSTES _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IDCLASS _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | LPAREN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | MINUS ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NEW ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | PLUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | RESULT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SUPER ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | THIS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPERSAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RELOP _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _v
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (nomVar : (Ast.expType))), _, (e : (Ast.expType))) = _menhir_stack in
            let _v : (Ast.instruc) = 
# 147 "tpParse.mly"
                                                          ( Assign(nomVar, e) )
# 1882 "tpParse.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPERSAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RELOP _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DEF | RCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (n : (
# 7 "tpParse.mly"
       (string)
# 1917 "tpParse.ml"
            ))), _, (e : (Ast.expType))) = _menhir_stack in
            let _v : (string option * Ast.instruc) = 
# 101 "tpParse.mly"
                                              ( (Some n), Epr(e) )
# 1922 "tpParse.ml"
             in
            _menhir_goto_suiteMethode _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 1939 "tpParse.ml"
     in
    _menhir_goto_boption_STATIC_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.lchamp) = 
# 211 "<standard.mly>"
    ( [] )
# 1948 "tpParse.ml"
     in
    _menhir_goto_list_champ_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 1959 "tpParse.ml"
     in
    _menhir_goto_boption_STATIC_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDCLASS _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (xs : (string list))), (n : (
# 7 "tpParse.mly"
       (string)
# 1988 "tpParse.ml"
                ))) = _menhir_stack in
                let _v : (Ast.declaV) = let l = 
# 232 "<standard.mly>"
    ( xs )
# 1993 "tpParse.ml"
                 in
                
# 157 "tpParse.mly"
                                                                ( (l,n) )
# 1998 "tpParse.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                | IS ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (Ast.declaV))) = _menhir_stack in
                    let _v : (Ast.declaV list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 2013 "tpParse.ml"
                     in
                    _menhir_goto_nonempty_list_declaVar_ _menhir_env _menhir_stack _menhir_s _v
                | COLON ->
                    _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState122 | MenhirState28 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.param list)) = _v in
        let _v : (Ast.param list) = 
# 144 "<standard.mly>"
    ( x )
# 2051 "tpParse.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.param list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.param))) = _menhir_stack in
        let _v : (Ast.param list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2062 "tpParse.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.instruc list) = 
# 211 "<standard.mly>"
    ( [] )
# 2073 "tpParse.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string list) = 
# 142 "<standard.mly>"
    ( [] )
# 2082 "tpParse.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.instruc) = 
# 146 "tpParse.mly"
                       ( ReturnSemi )
# 2100 "tpParse.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "tpParse.mly"
       (string)
# 2146 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (nom : (
# 7 "tpParse.mly"
       (string)
# 2152 "tpParse.ml"
    ))) = _menhir_stack in
    let _v : (Ast.expType) = 
# 119 "tpParse.mly"
               ( Id(nom) )
# 2157 "tpParse.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_estClass_ : _menhir_env -> 'ttv_tail -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCURL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_option_super_ : _menhir_env -> 'ttv_tail -> (Ast.expType option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCURL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expType list) = 
# 142 "<standard.mly>"
    ( [] )
# 2215 "tpParse.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expType) = 
# 111 "tpParse.mly"
           ( This )
# 2226 "tpParse.ml"
     in
    _menhir_goto_acces _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expType) = 
# 112 "tpParse.mly"
            ( Super )
# 2237 "tpParse.ml"
     in
    _menhir_goto_acces _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expType) = 
# 113 "tpParse.mly"
             ( Result )
# 2248 "tpParse.ml"
     in
    _menhir_goto_acces _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDCLASS _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | CSTES _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | ID _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | IDCLASS _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | LPAREN ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | MINUS ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | NEW ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | PLUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | RESULT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | SUPER ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | THIS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | RPAREN ->
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | ID _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IDCLASS _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState42 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | CSTES _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | ID _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | IDCLASS _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | LPAREN ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NEW ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | RESULT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SUPER ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | THIS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | AMPERSAND | DIV | DOT | RELOP _ | RPAREN | TIMES ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "tpParse.mly"
       (string)
# 2445 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "tpParse.mly"
       (string)
# 2455 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "tpParse.mly"
       (string)
# 2465 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 7 "tpParse.mly"
       (string)
# 2473 "tpParse.ml"
    )) = _v in
    let _v : (Ast.expType) = 
# 122 "tpParse.mly"
                ( Cstes(x) )
# 2478 "tpParse.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "tpParse.mly"
       (int)
# 2485 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 8 "tpParse.mly"
       (int)
# 2493 "tpParse.ml"
    )) = _v in
    let _v : (Ast.expType) = 
# 121 "tpParse.mly"
               ( Cste(x) )
# 2498 "tpParse.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_extClasse_ : _menhir_env -> 'ttv_tail -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (o1 : (string option)) = _v in
    let _v : (string option) = 
# 60 "tpParse.mly"
                             ( o1 )
# 2510 "tpParse.ml"
     in
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCURL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | STATIC ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | DEF | RCURL ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | AUTO | ID _ ->
                _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 7 "tpParse.mly"
       (string)
# 2560 "tpParse.ml"
        ))), _, (xs : (string list))) = _menhir_stack in
        let _v : (string list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2565 "tpParse.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState122 | MenhirState28 | MenhirState12 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDCLASS _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (n : (
# 7 "tpParse.mly"
       (string)
# 2585 "tpParse.ml"
                )) = _v in
                let (_menhir_stack, _menhir_s, (li : (string list))) = _menhir_stack in
                let _v : (Ast.param) = 
# 56 "tpParse.mly"
                                                                ( (li,n) )
# 2591 "tpParse.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COMMA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
                | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (Ast.param))) = _menhir_stack in
                    let _v : (Ast.param list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2615 "tpParse.ml"
                     in
                    _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState115 | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string list))) = _menhir_stack in
        let _v : (string list) = 
# 144 "<standard.mly>"
    ( x )
# 2643 "tpParse.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | CSTES _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState84 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack)
        | AMPERSAND | DIV | DOT | MINUS | PLUS | RELOP _ | SEMICOLON | TIMES | WALRUS ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | IDCLASS _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LCURL ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LPAREN ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MINUS ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NEW ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | PLUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | RESULT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | RETURN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | SUPER ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | THIS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | COLON ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | RCURL ->
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_goto_loption_separated_nonempty_list_COMMA_param__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs : (Ast.param list)) = _v in
    let _v : (Ast.param list) = let li = 
# 232 "<standard.mly>"
    ( xs )
# 2722 "tpParse.ml"
     in
    
# 53 "tpParse.mly"
                                        ( li )
# 2727 "tpParse.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXTENDS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDCLASS _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (n : (
# 7 "tpParse.mly"
       (string)
# 2753 "tpParse.ml"
                    )) = _v in
                    let _v : (string) = 
# 62 "tpParse.mly"
                                ( n )
# 2758 "tpParse.ml"
                     in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (x : (string)) = _v in
                    let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 2766 "tpParse.ml"
                     in
                    _menhir_goto_option_extClasse_ _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | IS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (string option) = 
# 114 "<standard.mly>"
    ( None )
# 2779 "tpParse.ml"
                 in
                _menhir_goto_option_extClasse_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDCLASS _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LPAREN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | CSTE _v ->
                            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                        | CSTES _v ->
                            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                        | ID _v ->
                            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                        | IDCLASS _v ->
                            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                        | LPAREN ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                        | MINUS ->
                            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                        | NEW ->
                            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                        | PLUS ->
                            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                        | RESULT ->
                            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                        | SUPER ->
                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                        | THIS ->
                            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                        | RPAREN ->
                            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | IS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (Ast.expType option) = 
# 114 "<standard.mly>"
    ( None )
# 2863 "tpParse.ml"
                 in
                _menhir_goto_option_super_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDCLASS _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | WALRUS ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | CSTE _v ->
                            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                        | CSTES _v ->
                            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                        | ID _v ->
                            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                        | IDCLASS _v ->
                            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                        | LPAREN ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | MINUS ->
                            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | NEW ->
                            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | PLUS ->
                            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | RESULT ->
                            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | SUPER ->
                            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | THIS ->
                            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
                    | IS ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, (n : (
# 7 "tpParse.mly"
       (string)
# 2935 "tpParse.ml"
                        ))) = _menhir_stack in
                        let _v : (string) = 
# 104 "tpParse.mly"
                             (n)
# 2940 "tpParse.ml"
                         in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (x : (string)) = _v in
                        let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 2948 "tpParse.ml"
                         in
                        _menhir_goto_option_estClass_ _menhir_env _menhir_stack _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | IS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (string option) = 
# 114 "<standard.mly>"
    ( None )
# 2966 "tpParse.ml"
                 in
                _menhir_goto_option_estClass_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce75 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "tpParse.mly"
       (string)
# 2987 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (x : (
# 7 "tpParse.mly"
       (string)
# 2993 "tpParse.ml"
    ))) = _menhir_stack in
    let _v : (string list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2998 "tpParse.ml"
     in
    _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "tpParse.mly"
       (string)
# 3005 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_goto_list_classe_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.classe list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCURL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.classe))), _, (xs : (Ast.classe list))) = _menhir_stack in
        let _v : (Ast.classe list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3040 "tpParse.ml"
         in
        _menhir_goto_list_classe_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.param list) = 
# 142 "<standard.mly>"
    ( [] )
# 3051 "tpParse.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "tpParse.mly"
       (string)
# 3058 "tpParse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
    | COLON ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.classe list) = 
# 211 "<standard.mly>"
    ( [] )
# 3245 "tpParse.ml"
     in
    _menhir_goto_list_classe_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDCLASS _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | RPAREN ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.progType) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LCURL ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3323 "tpParse.ml"
