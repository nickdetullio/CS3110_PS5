let (key, v) = Program.get_input () in
  let values = List.map Util.unmarshal v in
	let contains_list = List.map (fun (k, _, _, _) -> k) values in
  
	if (List.mem "REF" contains_list && List.mem "READ" contains_list) then
    begin
      let (ref, read) = List.partition (fun (a, _, _, _) -> a = "REF") values in
  		let (_, _, c, d) = List.hd ref in
  		let out = List.map (fun (_, _, x, y) -> ((c, d), x, y)) values in   
      Program.set_output (List.map Util.marshal out)
    end
	else Program.set_output []
	
	