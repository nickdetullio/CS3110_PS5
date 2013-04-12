let marshal_kvs = 
  List.map (fun (k, v) -> (Util.marshal k, Util.marshal v)) in

let (key, value) = Program.get_input() in
let v : (string * int) = Util.unmarshal key in
let (title, id) = v in
let short_id = string_of_int id in
let kmer_list = ref [] in 
for i = 0 to ((String.length value) - 10) do
	let kmer_id = short_id ^ (string_of_int i) in 
	kmer_list := (String.sub value i 10, (title, kmer_id, short_id, string_of_int i)) :: !kmer_list;
done;
Program.set_output (marshal_kvs !kmer_list); 