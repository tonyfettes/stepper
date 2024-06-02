type entry = { mutable parent : int; mutable length : int }
type t = entry Vector.t

let create ~(capacity : int) : t = Vector.create ~capacity

let add (forest : t) : unit =
  let parent = Vector.length forest in
  Vector.add forest { parent; length = 1 }

let rec find (forest : t) (child : int) : entry =
  let ({ parent; _ } as entry) = Vector.get forest child in
  if Int.equal parent child then entry else find forest parent

let union (forest : t) (a : int) (b : int) : unit =
  let t_a = find forest a in
  let t_b = find forest b in
  if t_a.length > t_b.length then (
    t_b.parent <- t_a.parent;
    t_a.length <- t_a.length + t_b.length)
  else (
    t_a.parent <- t_b.parent;
    t_b.length <- t_b.length + t_a.length)

let find (forest : t) (child : int) : int =
  let { parent; _ } = find forest child in
  parent
