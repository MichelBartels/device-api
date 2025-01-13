open Device_api

let a = Tensor.ones F32 [2; 3; 4]

let b = Tensor.zeros F32 [2; 3; 4]

let c = Tensor.concatenate [a; b]

let () = Printf.printf "%s\n" (Tensor.to_string c)
