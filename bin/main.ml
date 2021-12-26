open Mad

let read = Reader.read

let eval env ast =
  let rec eval_ast env ast =
    match ast with
    | Sexp.Atom a ->
        Env.find a env
    | Sexp.List l ->
        Sexp.List (List.map (eval' env) l)
    | _ ->
        ast
  and eval' env ast =
    let aux = eval_ast env in
    match ast with
    | Sexp.List [] ->
        ast
    | Sexp.List _ -> (
      match aux ast with
      | Sexp.List (Sexp.Function (_, fn) :: tl) ->
          fn tl
      | l ->
          l )
    | _ ->
        aux ast
  in
  (eval' env ast, env)

let print = Sexp.to_string

let rep str env =
  let ast = read str in
  let res, env = eval env ast in
  (print res, env)

let prompt _ = Printf.printf "m> " ; flush stdout

let main =
  let rec aux env =
    prompt () ;
    try
      let input = input_line stdin in
      let output, env = rep input env in
      let _ = print_endline output in
      aux env
    with End_of_file -> Printf.printf "<e"
  in
  aux Env.prelude
