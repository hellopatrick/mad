open Mad

let read = Reader.read

let eval env ast =
  let rec eval_ast env ast =
    match ast with
    | Sexp.Atom a ->
        Env.find a env
    | Sexp.List l ->
        let f ast =
          let _, res = eval' env ast in
          res
        in
        Sexp.List (List.map f l)
    | _ ->
        ast
  and eval_define env = function
    | [Sexp.Atom a; sexp] ->
        let env, value = eval' env sexp in
        let env' = Env.add a value env in
        (env', value)
    | _ ->
        failwith "invalid define"
  and eval_let env = function
    | [Sexp.List bindings; sexp] ->
        let rec bind res = function
          | [] ->
              res
          | [_] ->
              failwith "imbalanced let bindings definition"
          | Sexp.Atom a :: sexp :: tl ->
              let _, value = eval' env sexp in
              bind ((a, value) :: res) tl
          | _ ->
              failwith "invalid let"
        in
        let bindings = bind [] bindings |> List.to_seq in
        let env' = Env.add_seq bindings env in
        let _, value = eval' env' sexp in
        (env, value)
    | _ ->
        failwith "invalid let"
  and eval_if env tl =
    let cond, consq, alt =
      match tl with
      | [cond; consq; alt] ->
          (cond, consq, alt)
      | [cond; consq] ->
          (cond, consq, Sexp.Nil)
      | _ ->
          failwith "invalid if statement"
    in
    let _, value = eval' env cond in
    if Sexp.is_truthy value then eval' env consq else eval' env alt
  and eval_fn env sexps =
    let fn =
      match sexps with
      | [Sexp.List arg_names; body] ->
          Sexp.Function
            ( "anon"
            , fun args ->
                let rec bind env names values =
                  match (names, values) with
                  | Sexp.Atom name :: names, value :: values ->
                      let _, value = eval' env value in
                      let env = Env.add name value env in
                      bind env names values
                  | [], [] ->
                      env
                  | _, _ ->
                      failwith "binding failure"
                in
                let env = bind env arg_names args in
                let _, value = eval' env body in
                value )
      | _ ->
          failwith "invalid function definition"
    in
    (env, fn)
  and eval' env ast =
    let aux = eval_ast env in
    match ast with
    | Sexp.List [] ->
        (env, ast)
    | Sexp.List (Sexp.Define :: tl) ->
        eval_define env tl
    | Sexp.List (Sexp.Let :: tl) ->
        eval_let env tl
    | Sexp.List (Sexp.If :: tl) ->
        eval_if env tl
    | Sexp.List (Sexp.Defn :: tl) ->
        eval_fn env tl
    | Sexp.List _ -> (
        ( env
        , match aux ast with
          | Sexp.List (Sexp.Function (_, fn) :: tl) ->
              fn tl
          | l ->
              l ) )
    | _ ->
        (env, aux ast)
  in
  eval' env ast

let print = Sexp.to_string

let rep str env =
  let ast = read str in
  let env, res = eval env ast in
  (env, print res)

let prompt _ = Printf.printf "m> " ; flush stdout

let main =
  let rec aux env =
    prompt () ;
    try
      let input = input_line stdin in
      let env, output = rep input env in
      let _ = print_endline output in
      aux env
    with End_of_file -> ()
  in
  aux Env.prelude
