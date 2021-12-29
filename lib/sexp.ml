type t =
  | List of t list
  | Atom of string
  | Integer of int
  | Float of float
  | Function of string * (t list -> t)
  | Nil
  | True
  | False
  | Define
  | Let
  | If
  | Defn

let rec pp = function
  | List tt ->
      Printf.sprintf "(%s)" (List.map pp tt |> String.concat " ")
  | Atom a ->
      a
  | Integer i ->
      Printf.sprintf "%d" i
  | Float f ->
      Printf.sprintf "%f" f
  | Function (fn, _) ->
      Printf.sprintf "#<fn:%s>" fn
  | Define ->
      "def!"
  | Let ->
      "let*"
  | False ->
      "false"
  | True ->
      "true"
  | Nil ->
      "nil"
  | If ->
      "if"
  | Defn ->
      "fn*"

let to_string t = pp t

let atom_of_string = function
  | "def!" ->
      Define
  | "let*" ->
      Let
  | "true" ->
      True
  | "false" ->
      False
  | "nil" ->
      Nil
  | "if" ->
      If
  | "fn*" ->
      Defn
  | str -> (
    match int_of_string_opt str with
    | Some i ->
        Integer i
    | None -> (
      match float_of_string_opt str with Some f -> Float f | None -> Atom str )
    )

let is_truthy = function False | Nil -> false | _ -> true

let is_falsey = function False | Nil -> true | _ -> false
