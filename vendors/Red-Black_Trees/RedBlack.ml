(* Red-black trees according to the following classic paper:

   Chris Okasaki, Red-Black Trees in a Functional
   Setting. J. Funct. Program. 9(4): 471-477 (1999)
*)

type colour = Red | Black

type 'a t =
  Ext
| Int of colour * 'a t * 'a * 'a t

let empty = Ext

let is_empty m = (m = empty)

let blacken = function
  Ext -> Ext
| Int (_, left, root, right) -> Int (Black, left, root, right)

let balance colour left root right =
  match colour, left, root, right with
    Black, Int (Red, Int (Red, a, x, b), y, c), z, d
  | Black, Int (Red, a, x, Int (Red, b, y, c)), z, d
  | Black, a, x, Int (Red, Int (Red, b, y, c), z, d)
  | Black, a, x, Int (Red, b, y, Int (Red, c, z, d)) ->
      Int (Red, Int (Black, a, x, b), y, Int (Black, c, z, d))
  | _ ->
      Int (colour, left, root, right)

type choice = Old | New

let choose ~old ~new' = function
  Old -> old
| New -> new'

exception Physical_equality

let add ~cmp choice elt tree =
  let rec insert = function
    Ext -> Int (Red, Ext, elt, Ext)  (* A leaf *)
  | Int (colour, left, root, right) ->
      let diff = cmp elt root in
      if diff = 0 then
        let root' = choose ~new':elt ~old:root choice
        in if root == root' then raise Physical_equality
           else Int (colour, left, root', right)
      else if diff < 0 then 
        balance colour (insert left) root right
      else 
        balance colour left root (insert right)
  in try blacken (insert tree) with
       Physical_equality -> tree

let remove : type a b . cmp:(a -> b -> int) -> a -> b t -> b t = fun ~cmp elt tree ->
  (* TODO: this leaves the tree not properly balanced. *)
  let rec bst_remove : b t -> b t = function
    | Ext -> failwith "unknown error"
    | Int (colour,left, root, right) -> (
         ignore root; (* Deletion *)
         match left, right with 
         (* Get the next value, then put it at the place of the element you are removing and remove this element *)
         | _, Int _ ->
            let new_root, new_right = bst_remove_leftmost right in
            Int (colour,left,new_root,new_right)
         (* If this is the highr value, there is only one child, so move the tree up*)
         | Int (_lcolor, lleft, lroot, lright), Ext ->
            Int (colour, lleft, lroot, lright)
          (* No child, just delete *)
         | Ext, Ext -> Ext
    )
  and bst_remove_leftmost : b t -> b * b t = function
    | Ext -> failwith "unknow error"
    | Int  (colour, left, root, right) ->
      match left,right with
      | Int _, _ -> 
        (* Continue searching*) 
        bst_remove_leftmost left
      | Ext,Ext ->
        (* found with no children*)
        root,Ext  
      | Ext, Int (_rcolour, rleft,rroot,rright) ->
        root, Int (colour, rleft,rroot, rright)
  in
  let rec bst_delete : a -> b t -> b t = fun elt -> function
    | Ext -> raise Not_found
    | Int (colour, left, root, right) as current ->
       let c = cmp elt root in
       if      c = 0 then bst_remove current
       else if c < 0 then Int (colour, bst_delete elt left, root, right)
       else               Int (colour, left, root, bst_delete elt right)
  in
  try bst_delete elt tree
  with Not_found -> tree

let rec find ~cmp elt = function
  Ext -> (
    raise Not_found)
| Int (_, left, root, right) ->
    let diff = cmp elt root in
    if diff = 0 then (root)
    else if diff < 0 
      then (find ~cmp elt left)
      else (find ~cmp elt right)

let find_opt ~cmp elt tree =
  try Some (find ~cmp elt tree) with Not_found -> None

(* Inorder iterators *)

let rec iter f = function
                         Ext -> ()
| Int (_, left, root, right) -> iter f left; f root; iter f right

let rec inorder acc = function
                         Ext -> acc
| Int (_, left, root, right) -> inorder (root :: inorder acc right) left

let elements t = inorder [] t


let union ~cmp choice (tree_a : 'a t) tree_b =
  List.fold_left
    (fun acc elt -> add ~cmp choice elt acc)
    tree_b
    (elements tree_a)
    
let rec fold_inc f ~init = function
                         Ext -> init
| Int (_, left, root, right) ->
    fold_inc f ~init:(f ~elt:root ~acc:(fold_inc f ~init left)) right

let rec fold_dec f ~init = function
                         Ext -> init
| Int (_, left, root, right) ->
    fold_dec f ~init:(f ~elt:root ~acc:(fold_dec f ~init right)) left

let rec pp f ppf = function
  Ext -> Format.fprintf ppf "Ext"
| Int (c, l, root, r) ->
    Format.fprintf ppf "Int (%s,%a,%a,%a)"
    (match c with Red -> "Red" | Black -> "Black")
    (pp f) l
    f root
    (pp f) r