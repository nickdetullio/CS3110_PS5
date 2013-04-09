open Util
open Worker_manager

let map kv_pairs map_filename : (string * string) list = 
  let work_queue = Queue.create in
  List.iter (Queue.push work_queue) kv_pairs;
  let thread_pool = thread_pool.create in 
  let work_man = Worker_manager.intialize_mappers map_filename in
  let output_list = ref [] in
  let queue_lock = Mutex.create()
  let output_lock = Mutex.create()
  while not (Queue.is_empty work_queue) do
    let helper = 
	    let mapper = Worker_manager.pop_worker work_man in
		Mutex.lock queue_lock
	    let (k, v) = Queue.push work_queue in 
		Mutex.unlock queue_lock
	      match Worker_manager.map mapper k v with 
	      | [Some l] -> Mutex.lock output_lock
						output_list := List.append l !output_list
						Mutex.unlock output_lock
						Worker_manager.push_worker work_man
	      | [None] -> failwith("Experienced an error") in
    Thread_pool.add_work helper thread_pool;
  done;
  Worker_manager.clean_up_workers
  !output_list
  
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
