(* output = (short_id, (ref_id, short_index, ref_index)) *)
(* (short_index, seed_length, ref_id, ref_index) *)
let (key, values) = Program.get_input() in
	let helper ele1 ele2 = 
		let (a, (b,first_index,d)) = ele1 in
		let (x, (y,second_index,z)) = ele2 in
		if first_index = second_index then 0
		else (if first_index > second_index then 1 else -1) in
	let sorted_list = List.sort helper values in
	let rec helper lst acc curr_length =
		match lst with 
		| h1::h2::t -> let (t1, (t2,first_index,t3)) = h1 in
				   let (t4, (t5,second_index,t6)) = h2 in
				   if (second_index - first_index) < 10 then
					 helper (t1, (t2,first_index,t3))::t acc (second_index + 10)
				   else
					 helper (t4, (t5, second_index, t6))::t (first_index, curr_length, t2, t3)::acc 0
		| [] -> acc
				   
					
				   
	
	
	
