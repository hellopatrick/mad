include Map.Make (String)

let create_numeric_function name fn =
  let rec aux = function
    | Sexp.Integer i :: Sexp.Integer j :: tl ->
        aux (Sexp.Integer (fn i j) :: tl)
    | [Sexp.Integer i] ->
        Sexp.Integer i
    | _ ->
        failwith (Printf.sprintf "`%s` requires numeric arguments" name)
  in
  Sexp.Function (name, aux)

let prelude =
  List.to_seq
    [ ("+", create_numeric_function "+" Int.add)
    ; ("-", create_numeric_function "+" Int.sub)
    ; ("*", create_numeric_function "+" Int.mul)
    ; ("/", create_numeric_function "+" Int.div)
    ; ("%", create_numeric_function "+" Int.rem) ]
  |> of_seq
