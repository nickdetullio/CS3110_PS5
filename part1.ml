let map f t =
	let tsize = BinaryTree.size t in
		let rec tree_cycle func tree_index tr = 
			if (tree_index < tsize) && (BinaryTree.member tr tree_index) 
        then let new_tree = BinaryTree.remove tr tree_index 
               && BinaryTree.add tr (f (*Value at tree_index*)) in
      if ((2*tree_index + 1) < tsize) && (BinaryTree.member tr tree_index) 
        then if ((2*tree_index + 2) < tsize) && (BinaryTree.member tr tree_index)
               then tree_cycle func (2*tree_index + 2) (tree_cycle func (2*tree_index + 1) new_tree)
             else tree_cycle func (2*tree_index + 1) new_tree
      else new_tree in
	tree_cycle f 0 t
