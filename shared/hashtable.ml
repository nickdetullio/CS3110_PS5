type ('a, 'b) t = ('a * 'b) list array ref * int ref * ('a -> int)

let create capacity hash : ('a, 'b) t = 
  (ref (Array.make capacity []), ref 0, fun x -> (hash x) mod capacity)
  
let iter f ((a, i, _) : ('a, 'b) t) = 
  Array.iter (List.iter (fun (key, value) -> f key value)) !a

let rec table_double (a, i, f)  = 
  let size = Array.length !a in
  if !i >= ((3 * size) / 4) then
    let (a2, i, f) = create (2 * size) f in
      iter (fun key value -> add (a2, i, f) key value) (a, i, f);
      a := !a2
  else ()

and add ((a, i, f) : ('a, 'b) t) key value =  
  table_double (a, i, f);
  let lst = Array.get !a (f key) in 
  if List.mem_assoc key lst then
    Array.set !a (f key) ((key, value) :: (List.remove_assoc key lst))  
  else 
    (Array.set !a (f key) ((key, value) :: lst); 
     i := !i + 1)
 
let find ((a, i, f) : ('a, 'b) t) key = 
  let lst = Array.get !a (f key) in
  List.assoc key lst

let mem ((a, i, f) : ('a, 'b) t) key = 
  let lst = Array.get !a (f key) in
  List.mem_assoc key lst

let remove ((a, i, f) : ('a, 'b) t) key = 
  let lst = Array.get !a (f key) in
  if List.mem_assoc key lst then
    (Array.set !a (f key) (List.remove_assoc key lst); i := !i - 1)
  else ()
  
let fold f (table : ('a, 'b) t) init = 
  let acc_ref = ref init in
  let helper key value = 
    acc_ref := f key value !acc_ref in
  iter helper table;
  !acc_ref 
  
let length ((a, i, f) : ('a, 'b) t) = !i 
