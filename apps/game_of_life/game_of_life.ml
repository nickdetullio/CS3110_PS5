open Util;;

(** 
 * Read in the board from filename. 
 * Each line corresponds to one horizontal row of the board 
 *)
let board_from_file (filename : string) : int array array = 
  (* read in the board from filename
   * each line corresponds to one horizontal 
   * row of the board *)
  let in_channel = try open_in filename with
    |Sys_error e -> failwith "404040404040404040404" in
  let rows : string list = 
    let lines = ref [] in
    (try 
      while true; do
        lines := (input_line in_channel) :: (!lines);
      done;
    with End_of_file -> close_in in_channel);
    List.rev !lines in
    if not (List.for_all (fun row ->
      (String.length row) = (String.length (List.hd rows))) rows) then
      failwith "not all rows are the same length"
    else 
  (* assuming all input is geometrically well-formed *)
  let lst =  List.rev (List.fold_left (fun acc row -> 
      let chars = ref [] in
      String.iter (fun c -> match c with
        | '1' -> chars := 1 :: !chars
        | '0' -> chars := 0 :: !chars
        | _ -> failwith "character other than 0 or 1 in board") row;
      (List.rev !chars)::acc) [] rows) in
  Array.of_list (List.map Array.of_list lst) 
in

(**
 * [write_board board out_chan]: 
 * Print a string representation of the int array [board]
 * to output channel [out_chan]
 *)
let write_board board out_chan = 
  Array.iter (fun outer -> Array.iter (fun x -> output_string out_chan
  (string_of_int x)) outer; output_string out_chan "\n") board;
  output_string out_chan "\n"
in

let main (args : string array) : unit = 
  if Array.length args < 3 then
    print_endline "Usage: game_of_life <board> <# of iterations> [<outfile>]
  <board> is the path to an input file
  <# of iterations> is an int 
  <outfile> is where the result will be written, otherwise to stdout"
  else begin
    let shared = board_from_file args.(2)
    and num_iterations = try int_of_string args.(3) 
      with Failure "int_of_string" -> failwith "game_of_life: invalid number of iterations"
    and out_channel = 
      if Array.length args > 4 then open_out args.(4) 
      else stdout 
    in
    let w = Array.length shared.(0) and h = Array.length shared in
    output_string out_channel (Printf.sprintf "%d,%d\n" w h);
    (* Board loaded, output file primed. Ready to generate epochs and
     * write them to the output file. TODO implement! *)
    (* These lines swallow the otherwise unused variables. You'll probably
     * want to use these variables. If not, make sure to delete them above *)
    let _ = num_iterations
    and _ = write_board in
    failwith "Maria?"
  end
in

main Sys.argv    
