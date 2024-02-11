type t =
  | Var of string
  | Int of int
  | Add of t * t
  | App of t * t
  | Fun of string * t
  | Fix of string * t

let rec string_of_t =
  function
  | Var var -> var
  | Int int -> string_of_int int
  | Add (d1, d2) -> Printf.sprintf "(%s + %s)" (string_of_t d1) (string_of_t d2)
  | App (d1, d2) -> Printf.sprintf "(%s %s)" (string_of_t d1) (string_of_t d2)
  | Fun (x, d) -> Printf.sprintf "(fun %s -> %s)" x (string_of_t d)
  | Fix (x, d) -> Printf.sprintf "(fix %s -> %s)" x (string_of_t d)
