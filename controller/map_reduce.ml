open Util
open Worker_manager
                      
let ts_cons (list, lock) elt = 
  Mutex.lock lock;
  list := elt :: !list;
  Mutex.unlock lock 
  
let ts_append (list, lock) lst =
  Mutex.lock lock;
  list := List.append lst !list;
  Mutex.unlock lock

let ts_list_peek (list, lock) =
  Mutex.lock lock;
  let out = match !list with
  | [] -> None
  | hd::tl -> Some hd in
  Mutex.unlock lock;
  out
                                                                                     
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
  
  let work_queue = Queue.create () in
  let output_list = (ref [], Mutex.create ()) in
  let pending_list = (ref [], Mutex.create ()) in
  
  List.iter (fun e -> Queue.push e work_queue) kv_pairs;
  
  while not (Queue.is_empty work_queue) do
    let mapper = Worker_manager.pop_worker work_man in
    let (k, v) = Queue.pop work_queue in
    ts_cons pending_list (k, v);
    let helper () =   
	      match Worker_manager.map mapper k v with 
	      | Some l -> (if (ts_mem_remove pending_list (k, v)) then
                       ts_append output_list l
                     else ());
						Worker_manager.push_worker work_man mapper;
	      | None -> (); in
    Thread_pool.add_work helper thread_pool;
  done;
  
  let rec repeat_pending () =
    match ts_list_peek pending_list with
    | None -> ()
    | Some (k, v) -> 
        begin
          let mapper = Worker_manager.pop_worker work_man in
          let helper () =
            match Worker_manager.map mapper k v with
            | Some l -> (if (ts_mem_remove pending_list (k, v)) then
                           ts_append output_list l
                         else ())
            | None -> () in
          Thread_pool.add_work helper thread_pool;
          repeat_pending ()
        end in
  repeat_pending ();
  
  Thread_pool.destroy thread_pool;            
  Worker_manager.clean_up_workers work_man;
  
  !(fst output_list)
  
let combine kv_pairs : (string * string list) list = 
  let hash_table = Hashtbl.create 10 in
  
  let helper (k, v) = 
    if Hashtbl.mem hash_table k then
      let v_list = Hashtbl.find hash_table k in
      Hashtbl.replace hash_table k (v :: v_list)
    else
      Hashtbl.add hash_table k [v] in
  
  List.iter helper kv_pairs;
  Hashtbl.fold (fun k v_list acc -> (k, v_list) :: acc) hash_table []
  
let reduce kvs_pairs reduce_filename : (string * string list) list = 
  let thread_pool = Thread_pool.create 100 in 
  let work_man = Worker_manager.initialize_reducers reduce_filename in
  
  let work_queue = Queue.create () in
  let output_list = (ref [], Mutex.create ()) in
  let pending_list = (ref [], Mutex.create ()) in
  
  List.iter (fun e -> Queue.push e work_queue) kvs_pairs;
  
  while not (Queue.is_empty work_queue) do
    let mapper = Worker_manager.pop_worker work_man in
    let (k, v) = Queue.pop work_queue in
    ts_cons pending_list (k, v);
    let helper () =   
	      match Worker_manager.reduce mapper k v with 
	      | Some l -> (if (ts_mem_remove pending_list (k, v)) then
                       ts_cons output_list (k, l)
                     else ());
						Worker_manager.push_worker work_man mapper;
	      | None -> (); in
    Thread_pool.add_work helper thread_pool;
  done;
  
  let rec repeat_pending () =
    match ts_list_peek pending_list with
    | None -> ()
    | Some (k, v) -> 
        begin
          let mapper = Worker_manager.pop_worker work_man in
          let helper () =
            match Worker_manager.reduce mapper k v with
            | Some l -> (if (ts_mem_remove pending_list (k, v)) then
                           ts_cons output_list (k, l)
                         else ())
            | None -> () in
          Thread_pool.add_work helper thread_pool;
          repeat_pending ()
        end in
  repeat_pending ();
  
  Thread_pool.destroy thread_pool;
  Worker_manager.clean_up_workers work_man;
  !(fst output_list)


let map_reduce app_name mapper_name reducer_name kv_pairs =
  let mapper = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name
  and reducer = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped = map kv_pairs mapper in
  let combined = combine mapped in
  let reduced = reduce combined reducer in
  reduced
