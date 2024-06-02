type 'a t

val create : capacity:int -> 'a t
(** [create ~capacity] creates a vector with capacity as [~capacity]. *)

val length : 'a t -> int
val reserve : 'a t -> int -> unit
val add : 'a t -> 'a -> unit
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
