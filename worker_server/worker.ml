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
            (match program with
             | (None, error) -> if send_response client (Mapper (None, error))
                                  then handle_request client
                                else ()
             | (Some id, str) -> mappers := id :: !mappers;
                     begin
                       if send_response client (Mapper (Some id, str)) 
                         then handle_request client
                       else ()
                     end)
        | InitReducer source -> 
            let program = Program.build source in
              (match program with
               | (None, error) -> 
                          begin
                            if send_response client (Reducer (None, error))
                              then handle_request client
                            else ()
                          end
               | (Some id, str) -> reducers := id :: !reducers;
                            begin
                              if send_response client (Reducer (Some id, str)) 
                                then handle_request client
                              else ()
                            end)
        | MapRequest (id, k, v) -> 
            (if not (List.mem id !mappers) then 
                                 begin
                                   if send_response client (InvalidWorker id)
                                     then handle_request client
                                   else ()
                                 end
             else match Program.run id v with
                  | None -> 
                      if send_response client 
                        (RuntimeError (id, "Failed MapRequest")) 
                        then handle_request client
                      else ()
                  | Some result -> 
                       begin
                         if send_response client (MapResults (id, result))
                           then handle_request client
                         else ()
                       end)
        | ReduceRequest (id, k, v) -> 
            (if not (List.mem id !reducers) then 
                        begin
                          if send_response client (InvalidWorker id)
                            then handle_request client
                          else ()
                        end
             else match Program.run id v with
                  | None -> if send_response client 
                              (RuntimeError (id, "Failed ReduceRequest")) 
                              then handle_request client
                            else ()
                  | Some result -> if send_response client 
                                     (ReduceResults (id, result))
                                     then handle_request client
                                   else ()) 
      end
  | None ->
      Connection.close client;
      print_endline "Connection lost while waiting for request."
