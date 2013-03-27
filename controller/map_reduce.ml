open Util
open Worker_manager

(* TODO implement these *)
let map kv_pairs map_filename : (string * string) list = 
  failwith "Go back whence you came! Trouble the soul of my Mother no more!"
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
