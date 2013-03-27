open Util;;

(* Print the final output from the dna_sequencing app *)
let print_short_reads short_reads = 
    let print_exact_short_read (read_id, infos) =
      let print_one info_str =
        let (read_idx, len, ref_id, ref_idx) = unmarshal info_str in
        Printf.printf "(ref:%i  start idx:%i  length:%i  read start idx:%i); "
          ref_id ref_idx len read_idx
      in
      Printf.printf "Short read id: %s  " read_id;
      List.iter print_one infos;
      Printf.printf "\n"
    in
    let compare_keys (k1,_) (k2,_) = compare k1 k2 in
    List.iter print_exact_short_read (List.sort compare_keys short_reads)
in

let main (args : string array) : unit =
  let map_reduce = Map_reduce.map_reduce "dna_sequencing" in
  if Array.length args < 3 then
    print_endline "Usage: dna_sequencing <filename>"
  else
    (* Read input file *)
    let filename = args.(2) in
    let docs = load_documents filename in
    let kv_pairs =
      List.rev_map (fun d -> (marshal(d.title, d.id), d.body)) docs in
    (* map-reduce 1. TODO implement the mapper and reducer! *)
    let shared_k_mers = map_reduce "step_1_mapper" "step_1_reducer" kv_pairs in
    (* fix up the reduce results to have the right shape for map inputs.
     * For example:
     * [(k1,[v1;v2;v3]); (k2,[v4;v5])]
     * => [(k1,v1);(k1,v2);(k1,v3);(k2,v4);(k2,v5)] *)
    let shared_k_mers_split =
      List.fold_left
        (fun ss (k, vs) -> List.fold_left (fun ss' v -> (k, v)::ss') ss vs )
        []
        shared_k_mers
    in
    (* map-reduce 2. TODO implement the mapper and reducer *)
    let exact_short_reads = 
      map_reduce "step_2_mapper" "step_2_reducer" shared_k_mers_split in
    print_short_reads exact_short_reads
in

main Sys.argv    
