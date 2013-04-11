open Util
open Worker_manager

let ts_queue_push (queue, lock) elt = 
  Mutex.lock lock;
  Queue.push elt queue;
  Mutex.unlock lock
                                
let ts_queue_pop (queue, lock) = 
  Mutex.lock lock;
  let out = Queue.pop queue in
  Mutex.unlock lock;
  out
                                        
let ts_queue_isempty (queue, lock) = 
  Mutex.lock lock;
  let out = Queue.is_empty queue in
  Mutex.unlock lock;
  out
                                        
let ts_cons (list, lock) elt = 
  Mutex.lock lock;
  list := elt :: !list;
  Mutex.unlock lock 
  
let ts_append (list, lock) lst =
  Mutex.lock lock;
  list := List.append lst !list;
  Mutex.unlock lock
                                
let ts_pop (list, lock) = 
  Mutex.lock lock;
  match !list with
  | hd :: tl -> hd
  | [] -> failwith "Won't happen"
                              
let ts_list_isempty (list, lock) = 
  Mutex.lock lock;
  let out = !list == [] in
  Mutex.unlock lock;
  out

let ts_remove (list, lock) elt =
  Mutex.lock lock;
  list := List.remove_assoc (fst elt) !list;      
  Mutex.unlock lock                  
                                                                                     
let ts_mem_remove (list, lock) elt = 
  Mutex.lock lock;
  let out = List.mem elt !list in
  let helper = if out then
    list := List.remove_assoc (fst elt) !list
  else () in
  helper;
  Mutex.unlock lock;
  out

let map kv_pairs map_filename : (string * string) list = 
  let thread_pool = Thread_pool.create 100 in 
  let work_man = Worker_manager.initialize_mappers map_filename in
  
  let work_queue = (Queue.create (), Mutex.create ()) in
  let output_list = (ref [], Mutex.create ()) in
  let pending_list = (ref [], Mutex.create ()) in
  
  List.iter (fun e -> Queue.push e (fst work_queue)) kv_pairs;
  
  while not (ts_queue_isempty work_queue) do
    let mapper = Worker_manager.pop_worker work_man in
    let (k, v) = ts_queue_pop work_queue in
    let helper () =   
      ts_cons pending_list (k, v);
	      match Worker_manager.map mapper k v with 
	      | Some l -> (if (ts_mem_remove pending_list (k, v)) then
                       ts_append output_list l
                     else ());
						Worker_manager.push_worker work_man mapper;
	      | None -> ts_remove pending_list (k, v);
          ts_queue_push work_queue (k, v); in
    Thread_pool.add_work helper thread_pool;
  done;
  
  while not (ts_list_isempty pending_list) do
    let mapper = Worker_manager.pop_worker work_man in
    let helper () =
      let (k, v) = ts_pop pending_list in
      match Worker_manager.map mapper k v with
      | Some l -> ts_remove pending_list (k, v);
        ts_append output_list l;
      | None -> (); in
     Thread_pool.add_work helper thread_pool;
  done;
  
  Worker_manager.clean_up_workers work_man;
  Thread_pool.destroy thread_pool;
  !(fst output_list)
  
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
  let thread_pool = Thread_pool.create 100 in 
  let work_man = Worker_manager.initialize_reducers reduce_filename in
  
  let work_queue = (Queue.create (), Mutex.create ()) in
  let output_list = (ref [], Mutex.create ()) in
  let pending_list = (ref [], Mutex.create ()) in
  
  List.iter (fun e -> Queue.push e (fst work_queue)) kvs_pairs;
  
  while not (ts_queue_isempty work_queue) do
    let reducer = Worker_manager.pop_worker work_man in
    let (k, v_list) = ts_queue_pop work_queue in
    let helper () =   
      ts_cons pending_list (k, v_list);
	      match Worker_manager.reduce reducer k v_list with 
	      | Some l -> (if (ts_mem_remove pending_list (k, v_list)) then
                       ts_cons output_list (k, l)
                     else ());
						Worker_manager.push_worker work_man reducer;
	      | None -> ts_remove pending_list (k, v_list);
          ts_queue_push work_queue (k, v_list); in
    Thread_pool.add_work helper thread_pool;
  done;
  
  while not (ts_list_isempty pending_list) do
    let reducer = Worker_manager.pop_worker work_man in
    let helper () =
      let (k, v_list) = ts_pop pending_list in
      match Worker_manager.reduce reducer k v_list with
      | Some l -> ts_remove pending_list (k, v_list);
        ts_cons output_list (k, l);
      | None -> (); in
     Thread_pool.add_work helper thread_pool;
  done;
  
  Worker_manager.clean_up_workers work_man;
  Thread_pool.destroy thread_pool;
  !(fst output_list)

let map_reduce app_name mapper_name reducer_name kv_pairs =
  let mapper = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name
  and reducer = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped = map kv_pairs mapper in
  let combined = combine mapped in
  let reduced = reduce combined reducer in
  reduced
