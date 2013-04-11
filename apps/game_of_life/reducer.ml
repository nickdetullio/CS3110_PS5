let (k, v) = Program.get_input () in

let (x, y) = Util.unmarshal k in
let v_list : ((int * int) * int) list = List.map Util.unmarshal v in

let cur_bool = List.assoc (x, y) v_list in

let bool_sum = List.fold_left (fun acc (ind, bool) -> acc + bool) 0 
  (List.remove_assoc (x, y) v_list) in
  
let out = 
  if cur_bool = 1 && (bool_sum = 2 || bool_sum = 3) then 1
  else if cur_bool = 1 && (bool_sum < 2 || bool_sum > 3) then 0
  else if cur_bool = 0 && bool_sum = 3 then 1
  else 0 in
  
Program.set_output [Util.marshal out]
   