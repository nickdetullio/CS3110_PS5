let map (f : func) (t: tree) : (t :tree) -> 
	let tsize = BinaryTree.size t in
		let rec tree_cycle (func : func) (tree_index : int) (tr : tree) -> 
			if ((2*tree_index) + 1) < tsize then tree_cycle func ((2*tree_index) + 1) "final argument"
			if ((2*tree_index) + 2) < tsize then tree_cycle func ((2*tree_index) + 2) "final argument"
			if tree_index < tsize then tree_cycle func tree_index "final argument"
	tree_cycle f 0 t
