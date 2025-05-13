open Ctypes

type f32 = F32

type i1 = I1

type i64 = I64

type u32 = U32

type u64 = U64

type f64 = F64

type (_, _) kind =
  | F32 : (f32, float) kind
  | F64 : (f64, float) kind
  | I1 : (i1, bool) kind
  | I64 : (i64, Signed.Int64.t) kind
  | U32 : (u32, Unsigned.uint32) kind
  | U64 : (u64, Unsigned.uint64) kind

let ctype_of_kind : type a b. (a, b) kind -> b typ = function
  | F32 ->
      float
  | F64 ->
      double
  | I1 ->
      bool
  | I64 ->
      int64_t
  | U32 ->
      uint32_t
  | U64 ->
      uint64_t

type ('a, 'b) t = {kind: ('a, 'b) kind; data: 'b carray; shape: int list}

let make kind shape data = {kind; data; shape}

let of_list kind shape l =
  let n = List.length l in
  let expected_size = List.fold_left ( * ) 1 shape in
  if n <> expected_size then failwith "Tensor.of_list: size mismatch" ;
  let data = CArray.make (ctype_of_kind kind) n in
  List.iteri (fun i x -> CArray.set data i x) l ;
  {kind; data; shape}

let to_list t = CArray.to_list t.data

let shape t = t.shape

let size t = List.fold_left ( * ) 1 t.shape

let kind t = t.kind

let data t = CArray.start t.data

let calc_index t = List.fold_left2 (fun acc i j -> (acc * i) + j) 0 t.shape

let get t idx = CArray.get t.data @@ calc_index t idx

let zero : type a b. (a, b) kind -> b = function
  | F32 ->
      0.0
  | F64 ->
      0.0
  | I1 ->
      false
  | I64 ->
      0L
  | U32 ->
      Unsigned.UInt32.zero
  | U64 ->
      Unsigned.UInt64.zero

let one : type a b. (a, b) kind -> b = function
  | F32 ->
      1.0
  | F64 ->
      1.0
  | I1 ->
      true
  | I64 ->
      1L
  | U32 ->
      Unsigned.UInt32.one
  | U64 ->
      Unsigned.UInt64.one

let full kind shape value =
  let size = List.fold_left ( * ) 1 shape in
  let data = CArray.make ?initial:(Some value) (ctype_of_kind kind) size in
  {kind; data; shape}

let ones kind shape = full kind shape @@ one kind

let zeros kind shape = full kind shape @@ zero kind

let value_to_string : type a b. (a, b) kind -> b -> string =
 fun kind v ->
  match (kind, v) with
  | F32, v ->
      Printf.sprintf "%e" v
  | F64, v ->
      Printf.sprintf "%e" v
  | I1, b ->
      string_of_bool b
  | I64, i ->
      Signed.Int64.to_string i
  | U32, i ->
      Unsigned.UInt32.to_string i
  | U64, i ->
      Unsigned.UInt64.to_string i

type 'a values = Tensor of 'a values Seq.t | Value of 'a

let values t =
  let shape = shape t in
  let rec values' shape acc =
    match shape with
    | [] ->
        Value (get t (List.rev acc))
    | x :: xs ->
        Tensor (Seq.init x (fun i -> values' xs (i :: acc)))
  in
  values' shape []

let to_string t =
  let rec values_to_string = function
    | Tensor s ->
        "["
        ^ (Seq.map values_to_string s |> List.of_seq |> String.concat ", ")
        ^ "]"
    | Value v ->
        value_to_string t.kind v
  in
  values_to_string (values t)

let scalar kind v = of_list kind [] [v]

let scalar_f32 v = scalar F32 v

let concatenate tensors =
  let shape = shape @@ List.hd tensors in
  let kind = kind @@ List.hd tensors in
  let flat_size = List.fold_left ( * ) 1 shape in
  let n = List.length tensors in
  let data = CArray.make (ctype_of_kind @@ kind) (n * flat_size) in
  List.iteri
    (fun i t ->
      let flat = t.data in
      let flat_size = size t in
      for j = 0 to flat_size - 1 do
        CArray.set data ((i * flat_size) + j) (CArray.get flat j)
      done )
    tensors ;
  {kind; data; shape= n :: shape}


let normal mean std shape =
  let normal () =
    let u1 = Random.float 1.0 in
    let u2 = Random.float 1.0 in
    sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2) *. std +. mean in
  let data = List.init (List.fold_left ( * ) 1 shape) (fun _ -> normal ()) in
  of_list F32 shape data
