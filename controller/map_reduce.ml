open Util
open Worker_manager

let map kv_pairs map_filename : (string * string) list = 
  let work_queue = Queue.create in
  List.iter (Queue.push work_queue) kv_pairs;
  let thread_pool = Thread_pool.create in 
  let work_man = Worker_manager.intialize_mappers map_filename in
  let output_list = ref [] in
  let queue_lock = Mutex.create() in
  let output_lock = Mutex.create() in
  while not (Queue.is_empty work_queue) do
    let helper = 
	    let mapper = Worker_manager.pop_worker work_man in
		Mutex.lock queue_lock;
	    let (k, v) = Queue.pop work_queue in 
		Mutex.unlock queue_lock;
	      match Worker_manager.map mapper k v with 
	      | [Some l] -> Mutex.lock output_lock
						output_list := List.append l !output_list;
						Mutex.unlock output_lock;
						Worker_manager.push_worker work_man mapper
	      | [None] -> Queue.push (k, v) in
    Thread_pool.add_work helper thread_pool;
  done;
  Worker_manager.clean_up_workers
  !output_list
  
let combine kv_pairs : (string * string list) list = 
  let hash_table = Hashtbl.create 10 in
  let work_queue = Queue.create () in
  while not (Queue.is_empty work_queue) do
    let (k, v) = Queue.pop work_queue in
    if Hashtbl.mem hash_table k 
      then let (k, v_list) = Hashtbl.find hash_table k in 
      Hashtbl.replace hash_table k (k, v :: v_list)
    else Hashtbl.add hash_table k (k, [v]) 
  done;
  Hashtbl.fold (fun k (k, v_list) acc -> (k, v_list) :: acc) hash_table []
  
let reduce kvs_pairs reduce_filename : (string * string list) list =
  let work_queue = Queue.create in
  List.iter (Queue.push work_queue) reduce_filename;
  let thread_pool = Thread_pool.create in 
  let work_man = Worker_manager.intialize_reducers map_filename in
  let output_list = ref [] in
  let queue_lock = Mutex.create() in
  let output_lock = Mutex.create() in
  while not (Queue.is_empty work_queue) do
    let helper = 
	    let reducer = Worker_manager.pop_worker work_man in
		Mutex.lock queue_lock;
    let string_list = ref [] in
	    let (k, v_list) = Queue.pop work_queue in
		Mutex.unlock queue_lock;
	      match Worker_manager.reduce reducer k v_list with 
	      | [Some l] -> Mutex.lock output_lock
						output_list := List.append l !output_list;
						Mutex.unlock output_lock;
						Worker_manager.push_worker work_man reducer
	      | [None] -> Queue.push (k, v_list) in
    Thread_pool.add_work helper thread_pool;
  done;
  Worker_manager.clean_up_workers
  !output_list

let map_reduce app_name mapper_name reducer_name kv_pairs =
  let mapper = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name
  and reducer = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped = map kv_pairs mapper in
  let combined = combine mapped in
  let reduced = reduce combined reducer in
  reduced
