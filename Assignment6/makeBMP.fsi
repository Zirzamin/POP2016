module makeBMP

val makeBMP : string -> int -> int -> (int*int -> int*int*int) -> unit

val makeBMParray : string -> int -> int -> (int*int*int) [,] -> unit

val readBMP : string -> int*int*(int*int -> (int*int*int))

val readBMParray : string -> int*int*((int*int*int) [,])