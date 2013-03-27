
let map (f : func) (t: tree) : (t :tree) -> 
	let tsize = BinaryTree.size t in
		let rec tree_cycle (func : func) (tree_index : int) (tr : tree) -> 
			match tree_index with
			| ((2*tree_index)+1) < tsize ->  tree_cycle func (2*tree_index+1) "final argument"
			| ((2*tree_index)+2) < tsize ->  tree_cycle func (2*tree_index+2) "final argument"
			| tree_index < tsize -> tree_cycle func tree_index "final argument"
			| _ -> failwith "" in
	tree_cycle f 0 t
