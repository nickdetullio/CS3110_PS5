let marshal_kvs = 
  List.map (fun (k, v) -> (Util.marshal k, Util.marshal v)) in
  
let (k, v) = Program.get_input () in
let (x, y) = Util.unmarshal k in
let (x_length, y_length, value) = Util.unmarshal v in


let out = [((x, y), ((x, y), value)); 
           ((x + 1, y), ((x, y), value)); 
           ((x - 1, y), ((x, y), value)); 
           ((x, y + 1), ((x, y), value)); 
           ((x, y - 1), ((x, y), value)); 
           ((x + 1, y + 1), ((x, y), value));
           ((x + 1, y - 1), ((x, y), value));
           ((x - 1, y + 1), ((x, y), value));
           ((x - 1, y - 1), ((x, y), value))] in      


let modded_out = 
  List.map (fun ((x, y), z) -> (((x + x_length) mod x_length, (y + y_length) mod y_length), z)) out in


Program.set_output (marshal_kvs modded_out)