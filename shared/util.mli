(* Shared functionality between apps. 
 * 2013-03-26: Besides [marshal] and [unmarshal], these functions are only useful
 * for the [word_count] and [inverted_index] apps.
 * The map reduce print functions might help in controller.exe, though *)
type document = {id : int; title : string; body : string}

val load_documents : string -> document list
(**
 * [load_documents filepath]
 * Computes word vectors for the set of documents located by [filepath]. 
 * The file at [filepath] must contain a list of documents, one per line. 
 * Each document is of the format: "id @ title @ body", 
 * such that '@' does not appear in the title
 * or body.
 *)

val marshal : 'a -> string
(** [marshal x] Wrapper for OCaml's [Marshal.to_string] *)

val print_kvs : (string * string list) list -> unit
val print_map_results : (string * string) list -> unit
val print_combine_results : (string * string list) list -> unit
val print_reduced_documents : (string * string list) list -> unit
(** Debugging helpers for controller.exe *)
 
val read_whole_file : string -> string
val split_words : string -> string list
(** File I/O helpers *)

val unmarshal : string -> 'a
(** [unmarshal x] Wrapper for OCaml's [Marshal.from_string] *)
