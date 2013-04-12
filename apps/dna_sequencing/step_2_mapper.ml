(* value = (ref_id_index, short_id, short_index)
   output = (short_id, (ref_id, short_index, ref_index)) *)
let (key, value) = Program.get_input() in
let (ref_id_index, short_id, short_index) = Util.unmarshal value in
let (ref_id, ref_index) = ref_id_index in
Program.set_output [Util.marshal (short_id, (ref_id, short_index, ref_index))]
	