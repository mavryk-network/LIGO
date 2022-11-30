(* Private module to stop collision with publically exposed store module below *)
module Private = struct
  module Store = struct
    type 'a t =
      { next_addr : int (* Next available address *)
      ; heap : 'a Int.Map.t (* Store heap *)
      }

    let empty = { next_addr = 0; heap = Int.Map.empty }

    module Ref = struct
      type 'a t = { addr : int } [@@unboxed]

      let create { next_addr; heap } content =
        let heap = Int.Map.set heap ~key:next_addr ~data:content in
        { next_addr = next_addr + 1; heap }, { addr = next_addr }


      let invariant { next_addr; _ } { addr } = assert (addr >= next_addr)

      let get store t =
        invariant store t;
        Map.find_exn store.heap t.addr


      let set store t content =
        invariant store t;
        { store with heap = Map.set store.heap ~key:t.addr ~data:content }


      let phys_equal store t1 t2 =
        invariant store t1;
        invariant store t2;
        t1.addr = t2.addr
    end
  end
end

open Private.Store

(* This module implements an imperative data structure for disjoint sets
   (commonly known as `union-find'), based on Tarjan and Huet. 
   Union find implements a family of disjoint sets on values, where
   the disjoint sets can dynamically be combined using [union]. 
   A disjoint set [D] is a family of disjoint sets [D = {t1, ..., tn}],
   with the following operations:
    - [create v]: creates a new set [t] containing [v] in [D].
    - [find v] returns the (unique) set [t] in [D] that contains [v]. 
    - [union t1 t2] performs the union of [t1] and [t2] in [D]. 
    
   A disjoint set [D] is represeted using a forest, a collection of trees, 
   each node in the tree storing it's value, with pointers to parents. 
   Operations:
    - [find v]: 
      This traverses the element [v] back to the root [r] of the set, 
      creating a path [p] (the `find' path). 
      
      Path compression is performed on this operation, which updates the 
      parent pointer to point directly to the root [r]. 
      
    - [union t1 t2]: 
      We use union by rank. Each set stores the `rank', an upper bound for the 
      height of the tree. The set with the smallest rank becomes the child,
      with the edge case of equal ranks. 
   This implementation is optimized for the representation of equivalent 
   classes. Each equivalence class containing a "value". 
*)

(* Trees representing equivalence classes are of the form:
   {v
            Root
             |
           Inner
        / .. | .. \
     Inner Inner Inner
      /|\   /|\   /|\
      ...   ...   ...
   v}
   With directed edges towards the parents. 
   The root of the class contains the [rank] and value of type ['a]. 
   Internal nodes contain a pointer to their parent. 
*)

type 'a root =
  { rank : int
  ; content : 'a
  }

and 'a node =
  | Root of 'a root
  | Inner of 'a t

and 'a t = 'a node Ref.t

(* let invariant store t =
  let rec loop t height =
    match Ref.get store t with
    | Root r -> assert (height <= r.rank)
    | Inner t -> loop t (height + 1)
  in
  loop t 0 *)


let create store content = Ref.create store (Root { rank = 0; content })

(* [compress t ~imm_desc ~imm_desc_node ~prop_descs] compresses the path
   from [t] upwards to the root of [t]'s tree, where:
    - [imm_desc] is the immediate descendent of [t], and 
    - [prop_descs] are proper descendents of [imm_desc]
   The use of [imm_desc_node] is to avoid additional heap allocation
   when performing path compression. 
*)
let rec compress store t ~imm_desc ~imm_desc_node ~prop_descs =
  match Ref.get store t with
  | Root r ->
    (* Perform path compression *)
    let store =
      List.fold prop_descs ~init:store ~f:(fun store t -> Ref.set store t imm_desc_node)
    in
    (* Return pointer to root and it's contents *)
    store, t, r
  | Inner t' as imm_desc_node ->
    compress store t' ~imm_desc:t ~imm_desc_node ~prop_descs:(imm_desc :: prop_descs)


let repr store t =
  match Ref.get store t with
  | Root r -> store, t, r
  | Inner t' as imm_desc_node ->
    compress store t' ~imm_desc:t ~imm_desc_node ~prop_descs:[]


let root store t =
  match Ref.get store t with
  | Root _ -> store, t
  | _ ->
    let store, t, _ = repr store t in
    store, t


let rec get store t =
  match Ref.get store t with
  | Root { content; _ } -> store, content
  | Inner t' ->
    (match Ref.get store t' with
    | Root { content; _ } -> store, content
    | Inner _ ->
      let store, t = root store t in
      get store t)


let rec set store t content =
  match Ref.get store t with
  | Root { rank; _ } -> Ref.set store t (Root { rank; content })
  | Inner t' ->
    (match Ref.get store t' with
    | Root { rank; _ } -> Ref.set store t (Root { rank; content })
    | Inner _ ->
      let store, t = root store t in
      set store t content)


let link store ~src ~dst = Ref.set store dst (Inner src)

let union store t1 t2 ~f =
  let store, t1, { rank = r1; content = c1 } = repr store t1 in
  let store, t2, { rank = r2; content = c2 } = repr store t2 in
  if Ref.phys_equal store t1 t2
  then store
  else if r2 < r1
  then (
    let store = link store ~src:t1 ~dst:t2 in
    Ref.set store t1 (Root { rank = r1; content = f c1 c2 }))
  else (
    let r = if r1 < r2 then r1 else r1 + 1 in
    let store = link store ~src:t2 ~dst:t1 in
    Ref.set store t1 (Root { rank = r; content = f c1 c2 }))


let is_equiv store t1 t2 =
  let store, t1 = root store t1 in
  let store, t2 = root store t2 in
  store, Ref.phys_equal store t1 t2


module Store = struct
  module Store = Private.Store
  type 'a t = 'a node Store.t

  let empty = Store.empty
end
