type 'a t = { mutable array : 'a array; mutable length : int }

let create ~(capacity : int) : 'a t =
  let array = Array.make capacity (Obj.magic 0) in
  { array; length = 0 }

let length (vector : 'a t) : int = vector.length

let reserve (vector : 'a t) (capacity : int) : unit =
  let array_length = Array.length vector.array |> ref in
  while !array_length < capacity do
    array_length := !array_length * 2
  done;
  let array =
    Array.init !array_length @@ fun i ->
    if i < Array.length vector.array then vector.array.(i) else Obj.magic 0
  in
  vector.array <- array

let add (vector : 'a t) (element : 'a) : unit =
  reserve vector (vector.length + 1);
  vector.array.(vector.length) <- element;
  vector.length <- vector.length + 1

exception Invalid_index of { expect : (int * int); actual : int }

let get (vector : 'a t) (index : int) : 'a =
  if index < 0 || index >= vector.length then
    raise (Invalid_index { expect = (0, vector.length); actual = index });
  Array.unsafe_get vector.array index

let set (vector : 'a t) (index : int) (element : 'a) : unit =
  if index < 0 || index >= vector.length then
    raise (Invalid_index { expect = (0, vector.length); actual = index });
  Array.unsafe_set vector.array index element
