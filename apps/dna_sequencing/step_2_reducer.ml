(* output = (short_id, (ref_id, short_index, ref_index)) *)
(* (short_index, seed_length, ref_id, ref_index) *)
let (k, v) = Program.get_input() in
let key = Util.unmarshal k in
let values = List.map Util.unmarshal v in

let (ref_id, _, _) = List.hd values in
let int_values = 
  List.map (fun (_, y, z) -> (int_of_string y, int_of_string z)) values in

let sorted_list = List.sort (fun (f, _) (s, _) -> f - s) values in

let rec grow_seed ((seed_index, ref_index, length) as seed) lst =
  match lst with
  | (short_index, _) :: tl -> 
    begin
      if short_index < (seed_index + length) then
        combine_seed 
          (seed_index, ref_index, (short_index + 10) - seed_index) tl
      else
        (seed, lst)
    end
  | [] -> (seed, []) in

let rec gather_seeds seeds lst =
  match lst with
  | (seed_index, ref_index) :: tl -> 
      let (new_tl, new_seed) = grow_seed (seed_index, ref_index, 10) tl in
      gather_seeds (new_seed :: seeds) new_tl
  | [] -> seeds in

let out = gather_seeds [] sorted_list in

let form_out = List.map (fun (x, y, z) -> (x, z, ref_id, y)) out in

Program.set_output (List.map Util.marshal form_out)



				   
	
	
	
