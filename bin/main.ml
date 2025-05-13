open Device_api

let t1 = Tensor.of_list F32 [2; 2] [1.; 2.; 3.; 4.]

let t2 = Tensor.of_list F32 [2; 2] [5.; 6.; 7.; 8.]

let t3 = Tensor.concatenate [t1; t2]

let () = print_endline @@ Tensor.to_string t3
