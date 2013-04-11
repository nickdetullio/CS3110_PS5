let (key, values) = Program.get_input() in
	let contains_list = List.fold_left (fun k acc -> let (a, b, c, d) = k in a::acc) [] values in
	if (List.mem "REF" contains_list && List.mem "READ" contains_list) then
		let rec find_ref value_list =
			match value_list with
			| (a, b, c, d)::t -> if a = "REF" then (c,d)
			| [] -> raise Not_Found in
		let ref_id_index = find_ref values
		Program.set_output (List.fold_left (fun ele acc -> let (a, b, short_id, short_index) = ele in 
		if a <> "REF" then (ref_id_index, short_id, short_index)::acc else acc) [] values)
	 
	
	