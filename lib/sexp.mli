type t =
  | List of t list
  | Atom of string
  | Integer of int
  | Function of string * (t list -> t)

val to_string : t -> string
