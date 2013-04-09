let (key, values) = Program.get_input() in
let reduced_list = List.fold_left 
  (fun acc v -> if List.mem values v then acc else v::acc) 0 values in
Program.set_output [reduced_list]