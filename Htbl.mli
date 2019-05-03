module Htbl :
  sig
    val size : int
    val getKey : int -> int -> int
    val getTbl :
      (Base.int, Base.int * Base.int * Base.int) Core_kernel.Avltree.t
      Core_kernel.Array.t
    val insertHelper :
      int ->
      'a ->
      ('a, 'b * 'c * 'd) Core_kernel.Avltree.t Core_kernel.Array.t ->
      'b -> 'c -> 'd -> unit
    val insert :
      int * int ->
      int ->
      (int, int * int * int) Core_kernel.Avltree.t Core_kernel.Array.t ->
      unit
    val findBucket :
      int ->
      'a ->
      ('a, 'b * 'c * 'd) Core_kernel.Avltree.t Core_kernel.Array.t ->
      'b -> 'c -> 'd option
    val findHelper :
      int * int ->
      (int, int * int * int) Core_kernel.Avltree.t Core_kernel.Array.t -> int
    val find :
      int * int ->
      (int, int * int * int) Core_kernel.Avltree.t Core_kernel.Array.t -> int
  end