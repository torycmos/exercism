module BinarySearchTree

type Tree = 
    {   value : int
        left : Tree option
        right : Tree option   }

let singleton n = { value = n; left = None; right = None}

let (|Lbranch|Rbranch|Dbranch|Leaf|) tree =
    match tree.left, tree.right with
    | Some _, Some _ -> Dbranch
    | Some _, None -> Lbranch
    | None, Some _ -> Rbranch
    | _ -> Leaf

let rec insert n tree = 
    let leaf n = function
        | Some x -> Some (insert n x)
        | None -> Some (singleton n)
    match n <= tree.value with
    | true -> {tree with left = leaf n tree.left}
    | false -> {tree with right = leaf n tree.right}

let fromList (xs:int list) = List.fold (fun acc x -> insert x acc) 
                                       (singleton xs.Head) 
                                       (xs.Tail)

let foldtree fdual fleft fright leaf tree =
    let rec loop tree k =
        match tree with
        | Dbranch as t -> loop t.left.Value (fun lacc ->
                          loop t.right.Value (fun racc ->
                            k (fdual t.value lacc racc)))
        | Lbranch as t -> loop t.left.Value (fun acc -> 
                            k (fleft t.value acc))
        | Rbranch as t -> loop t.right.Value (fun acc -> 
                            k (fright t.value acc))
        | Leaf -> k (leaf tree.value)
    loop tree id

let toList (tree : Tree) =
    foldtree (fun x l r acc -> l (x :: (r acc)))
             (fun x l acc -> l (x :: acc)) 
             (fun x r acc -> x :: (r acc))
             (fun x acc -> x :: acc) 
             tree
             []