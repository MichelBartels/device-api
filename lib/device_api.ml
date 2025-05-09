module type S = sig
  type program

  type buffer

  val compile : ?path:string -> string -> program

  val tensor_to_buffer : ('a, 'b) Tensor.t -> buffer

  val execute : program -> num_outputs:int -> buffer list -> buffer list

  val buffer_to_tensor :
    shape:int list -> ('a, 'b) Tensor.kind -> buffer -> ('a, 'b) Tensor.t

  val identifier : string

  val collect_buffer : buffer -> unit
end

module Tensor = Tensor
