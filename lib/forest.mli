type t

val create : capacity:int -> t
val add : t -> unit
val find : t -> int -> int
val union : t -> int -> int -> unit
