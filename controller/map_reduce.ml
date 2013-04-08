open Util
open Worker_manager

(* TODO implement these *)
let map kv_pairs map_filename : (string * string) list = 
  let work_man = Worker_manager.intialize_mappers map_filename
  let helper ele = 
	let mapper = pop_worker work_man in
	let (k,v) = ele in 
	match Worker_manager.map mapper k v with 
	| [Some l] -> l
	| [None] -> failwith("Experienced an error")
  List.flatten (List.map helper kv_pairs)
  
let combine kv_pairs : (string * string list) list = 
  failwith "You have been doomed ever since you lost the ability to love."
let reduce kvs_pairs reduce_filename : (string * string list) list =
  failwith "The only thing necessary for evil to triumph is for good men to do nothing"

let map_reduce app_name mapper_name reducer_name kv_pairs =
  let mapper = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name
  and reducer = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped = map kv_pairs mapper in
  let combined = combine mapped in
  let reduced = reduce combined reducer in
  reduced
