(** This module implements a persistent (immutable) implementation
    of data structure for disjoint sets (commonly known as `union-find'), 
    based on Tarjan and Huet. 
    
    Union find implements a family of disjoint sets on values, where
    the disjoint sets can dynamically be combined using [union]. 
    
    This implementation is optimized for the representation of equivalent 
    classes. Each equivalence class containing a "value".  
*)

(** The implementation uses an underlying immutable "store" to provide 
    immutablility. *)
module Store : sig
  (** ['a t] is the type of a store containing equivalence classes with
      a value of type ['a]. *)
  type 'a t

  (** [empty] is the empty store *)
  val empty : 'a t
end

(** The type ['a t] denotes a node in an equivalence class associated with a 
    unique value of type ['a]. *)
type 'a t

(** [create store v] creates a new node representing a singleton class with value [v]. *)
val create : 'a Store.t -> 'a -> 'a Store.t * 'a t

(** [get store t] returns the value of the equivalence class of [t]. *)
val get : 'a Store.t -> 'a t -> 'a Store.t * 'a

(** [set store t v] sets the value of the equivalence class of [t] to [v]. *)
val set : 'a Store.t -> 'a t -> 'a -> 'a Store.t

(** [union store t1 t2 ~f] merges the equivalence classes of [t1] and [t2].
    The value of the combined class is given by [f (get t1) (get t2)]. 
        
    After [union store t1 t2 ~f], [is_equiv store t1 t2] always holds true. *)
val union : 'a Store.t -> 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a Store.t

(** [is_equiv store t1 t2] returns true iff [t1] and [t2] belong to the same
    equivalence class. *)
val is_equiv : 'a Store.t -> 'a t -> 'a t -> 'a Store.t * bool
