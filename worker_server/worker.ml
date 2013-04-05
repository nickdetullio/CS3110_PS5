open Protocol

let send_response client response =
  let success = Connection.output client response in
    (if not success then
      (Connection.close client;
       print_endline "Connection lost before response could be sent.")
    else ());
    success

let mappers = ref []

let reducers = ref []

let rec handle_request client =
  match Connection.input client with
    Some v -> 
      begin
        match v with
        | InitMapper source -> 
            let program = Program.build source in
            match program with
            | (None, error) -> if send_response client (Mapper (None, error))
                                 then handle_request client
                               else failwith "InitMapper failed"
            | (Some id, str) -> mappers := id :: !mappers;
                               if send_response client (Mapper (Some id, str)) 
                                 then handle_request client
                               else failwith "InitMapper failed"
        | InitReducer source -> 
            let program = Program.build source in
              match program with
              | (None, error) -> if send_response client (Reducer (None, error))
                                   then handle_request client
                                 else failwith "InitMapper failed"
              | (Some id, str) -> reducers := id :: !reducers;
                                 if send_response client (Reducer (Some id, str)) 
                                   then handle_request client
                                 else failwith "InitMapper failed"
        | MapRequest (id, k, v) -> 
            if not (List.mem !mappers id) then 
                               begin
                                 if send_response client (InvalidWorker id)
                                   then handle_request client
                                 else failwith "MapRequest failed"
                               end
            else match Program.run id v with
                 | None -> if send_response client (RuntimeError (id, v)) 
                             then handle_request client
                           else failwith "MapRequest failed"
                 | Some result -> if send_response client (MapResults id, results)
                                    then handle_request client
                                  else failwith "MapRequest failed"    
        | ReduceRequest (id, k, v) -> 
            if not (List.mem !reducers id) then 
                       begin
                         if send_response client (InvalidWorker id)
                           then handle_request client
                         else failwith "ReduceRequest failed"
                       end
            else match Program.run id v with
                 | None -> if send_response client (RuntimeError (id, v)) 
                             then handle_request client
                           else failwith "ReduceRequest failed"
                 | Some result -> if send_response client (ReduceResults id, results)
                                    then handle_request client
                                  else failwith "ReduceRequest failed" 
      end
  | None ->
      Connection.close client;
      print_endline "Connection lost while waiting for request."
