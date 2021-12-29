type t = {str: string; pos: int}

let init str = {str; pos= 0}

let peek t =
  if t.pos >= String.length t.str then failwith "incomplete sexp"
  else t.str.[t.pos]

let advance t = {t with pos= t.pos + 1}

let read str =
  let t = init str in
  let rec parse t =
    match peek t with
    | ')' ->
        failwith (Printf.sprintf "unexpected right parens @ pos=%d" t.pos)
    | '(' ->
        read_list (advance t)
    | ' ' ->
        parse (advance t)
    | _ ->
        read_atom t
  and read_list t =
    let sexps, t = read_collection t ')' in
    (Sexp.List sexps, t)
  and read_collection t terminal =
    let rec aux t sexps =
      match peek t with
      | c when c = terminal ->
          (List.rev sexps, advance t)
      | ' ' ->
          aux (advance t) sexps
      | _ ->
          let sexp, t = parse t in
          aux t (sexp :: sexps)
    in
    aux t []
  and read_atom t =
    let buffer = Buffer.create 5 in
    let rec aux t =
      match peek t with
      | (exception _) | ' ' | ')' ->
          (Buffer.contents buffer, t)
      | c ->
          Buffer.add_char buffer c ;
          aux (advance t)
    in
    let str, t = aux t in
    let atom = Sexp.atom_of_string str in
    (atom, t)
  in
  let sexp, _ = parse t in
  sexp
