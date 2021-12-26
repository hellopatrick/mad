type t =
  | List of t list
  | Atom of string
  | Integer of int
  | Function of string * (t list -> t)

let rec pp = function
  | List tt ->
      Printf.sprintf "(%s)" (List.map pp tt |> String.concat " ")
  | Atom a ->
      a
  | Integer i ->
      Printf.sprintf "%d" i
  | Function (fn, _) ->
      fn

let to_string t = pp t
