let (key, value_list) = Program.get_input() in
let reduced_list = List.fold_left 
  (fun acc v -> if List.mem v acc then acc else v :: acc) [] value_list in
Program.set_output reduced_list