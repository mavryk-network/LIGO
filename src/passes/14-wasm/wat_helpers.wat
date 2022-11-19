(module 
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (param i32) ))
    (type  (func (param i32 i32) (result i32)))
    (import "env" "malloc" (func $malloc (type 0)))
    (import "host" "print" (func $print (type 1)))
    (import "env" "__indirect_function_table" (table $table 0 funcref))
    ;; (import "env" "")

    (func $right_rotate (param $child i32) (result i32) 
        (local $grand_parent i32)
        (local $parent i32)
        local.get $child
        i32.const 4
        i32.add
        i32.load
        local.set $parent

        local.get $parent 
        i32.const 4 
        i32.add 
        i32.load
        local.set $grand_parent

        local.get $parent 
        i32.const 12
        i32.add 
        local.get $grand_parent
        i32.store

        local.get $grand_parent 
        i32.const 4
        i32.add
        local.get $parent 
        i32.store
        local.get $parent
    )

    (func $c_set_add_color (param $item i32) (result i32)
        local.get $item
        i32.const 16
        i32.add
        i32.load
    )

    (func $c_set_left_child (param $parent i32) (result i32)
        local.get $parent
        i32.const 8
        i32.add
        i32.load
        
    )

    (func $c_set_add_insert_value (param $value i32) (param $parent i32) (param $position i32) (param $size i32)
        (local $child i32)
        
        local.get $size
        call $malloc
        local.tee $child 
        local.get $value 
        i32.store


        local.get $child 
        i32.const 4
        i32.add
        local.get $parent
        i32.store

        local.get $parent
        local.get $position 
        i32.add
        local.get $child 
        i32.store

        local.get $child 
        i32.const 16
        i32.add
        i32.const 1
        i32.store

    )

    (func $__ligo_internal__set_size (param $set i32) (result i32)
        (local $count i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $left_value i32)
        (local $right_value i32)
        (local $temp i32)

        local.get $set 
        i32.const 8
        i32.add
        i32.load
        local.set $left_child

        local.get $left_child 
        i32.const 0
        i32.eq
        if 
            i32.const 0
            local.set $left_value
        else
            local.get $left_child
            call $__ligo_internal__set_size
            local.set $left_value
        end
        local.get $set 
        i32.const 12
        i32.add
        i32.load
        local.set $right_child

        local.get $right_child
        i32.const 0
        i32.eq
        if 
            i32.const 0
            local.set $right_value
        else 
            local.get $right_child
            call $__ligo_internal__set_size
            local.set $right_value
        end

        i32.const 1
        local.get $left_value 
        local.get $right_value 
        i32.add
        i32.add
)

    (func $__ligo_internal__set_remove (param $set i32) (param $key i32) (param $compare i32) (param $SIZE i32) (param $C_SET_EMPTY i32) (result i32)
        (local $compare_result i32)
        (local $result i32)
        (local $new_item i32)
        (local $current_item i32)
        (local $parent i32)
        (local $new_item_parent i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $new_child i32)
        (local $temp i32)
        (local $most_left i32)
        (local $check_left i32)
        

        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq 
        if (result i32)
            ;; no collection, so nothing to delete
            local.get $C_SET_EMPTY
        else 
            

            local.get $SIZE
            call $malloc
            local.set $result 

            local.get $result
            local.set $new_item 

            local.get $set 
            local.set $current_item

            ;; loop to find the right node to delete
            loop (result i32)

                local.get $new_item 
                local.get $current_item 
                local.get $SIZE
                memory.copy 

                local.get $new_item 
                i32.const 4
                i32.add
                local.get $parent 
                i32.store

                local.get $parent
                local.set $new_item_parent

                local.get $new_item 
                local.set $parent
    
                local.get $current_item 
                i32.const 8
                i32.add 
                i32.load 
                local.set $left_child

                local.get $current_item 
                i32.const 4
                i32.add 
                local.get $new_item
                i32.store

                local.get $new_item 
                i32.const 12
                i32.add 
                i32.load 
                local.set $right_child  

                local.get $right_child 
                i32.const 4
                i32.add 
                local.get $new_item
                i32.store
    
                ;; do comparison of the nodes
                local.get $new_item
                local.get $key                
                local.get $compare            
                call_indirect 0 (type 2)
                local.tee $compare_result
                if (result i32) 

                    local.get $compare_result
                    i32.const -1
                    i32.eq
                    if (result i32)
                        local.get $left_child 
                        if (result i32)
                            
                            ;; proceed with the left child 
                            local.get $SIZE
                            call $malloc
                            local.set $new_child

                            local.get $new_item 
                            i32.const 8
                            i32.add 
                            local.get $new_child
                            i32.store

                            local.get $new_child
                            local.set $new_item

                            local.get $left_child
                            local.set $current_item 
                            br 3
                        else 
                            local.get $result
                        end
                    else 
                        local.get $right_child 
                        if (result i32)   

                            ;; proceed with the right child
                            local.get $SIZE
                            call $malloc
                            local.set $new_child

                            local.get $new_item 
                            i32.const 12
                            i32.add 
                            local.get $new_child
                            i32.store

                            local.get $new_child
                            local.set $new_item

                            local.get $right_child
                            local.set $current_item 
                            br 3
                        else 
                            local.get $result
                        end
                    end                                        
                else 
                    ;; found the node to replace
                    local.get $left_child
                    i32.const 0
                    i32.eq
                    local.get $right_child 
                    i32.const 0 
                    i32.ne
                    i32.and
                    if (result i32)
                        local.get $SIZE
                        call $malloc 
                        local.set $new_child

                        local.get $new_child 
                        local.get $right_child 
                        local.get $SIZE
                        memory.copy 

                        local.get $new_child
                        i32.const 4
                        i32.add 
                        local.get $new_item_parent
                        i32.store

                        ;; can replace with right_child
                        local.get $new_item_parent 
                        i32.const 8
                        i32.add 
                        i32.load 
                        local.get $new_item 
                        i32.eq
                        if (result i32)
                            local.get $new_item_parent 
                            i32.const 8 
                            i32.add 
                            local.get $new_child 
                            i32.store
                            local.get $result
                        else 
                            local.get $new_item_parent 
                            i32.const 12 
                            i32.add 
                            local.get $new_child 
                            i32.store
                            local.get $result
                        end
                    else 
                        local.get $left_child
                        i32.const 0
                        i32.ne
                        local.get $right_child 
                        i32.const 0 
                        i32.eq
                        i32.and
                        if (result i32)
                            local.get $SIZE
                            call $malloc 
                            local.set $new_child

                            local.get $new_child 
                            local.get $left_child 
                            local.get $SIZE
                            memory.copy 

                            local.get $new_child
                            i32.const 4
                            i32.add 
                            local.get $new_item_parent
                            i32.store

                            ;; can replace with left_child
                            local.get $new_item_parent 
                            i32.const 8
                            i32.add 
                            i32.load 
                            local.get $new_item 
                            i32.eq
                            if (result i32)
                                local.get $new_item_parent 
                                i32.const 8 
                                i32.add 
                                local.get $new_child 
                                i32.store
                                local.get $result
                            else 
                                local.get $new_item_parent 
                                i32.const 12 
                                i32.add 
                                local.get $new_child 
                                i32.store
                                local.get $result
                            end
                        else 
                            local.get $left_child
                            i32.const 0
                            i32.eq
                            local.get $right_child 
                            i32.const 0 
                            i32.eq
                            i32.and
                            if (result i32)
                                ;; there is no child
                                local.get $new_item_parent 
                                i32.const 8
                                i32.add 
                                i32.load
                                local.get $new_item
                                i32.eq
                                if 
                                    local.get $new_item_parent
                                    i32.const 8 
                                    i32.add 
                                    i32.const 0
                                    i32.store
                                else 
                                    local.get $new_item_parent 
                                    i32.const 12
                                    i32.add 
                                    i32.const 0
                                    i32.store
                                end 
                                local.get $result
                            else
                                local.get $SIZE
                                call $malloc 
                                local.set $most_left

                                local.get $most_left
                                local.get $right_child 
                                local.get $SIZE
                                memory.copy

                                local.get $most_left 
                                i32.const 4
                                i32.add 
                                local.get $new_item
                                i32.store

                                local.get $new_item
                                i32.const 12
                                i32.add 
                                local.get $most_left
                                i32.store

                                loop                                   
                                    local.get $most_left
                                    i32.const 8
                                    i32.add
                                    i32.load 
                                    local.tee $temp
                                    if 
                                        local.get $SIZE
                                        call $malloc 
                                        local.tee $new_child

                                        local.get $new_child 
                                        local.get $temp 
                                        local.get $SIZE
                                        memory.copy 

                                        local.get $most_left 
                                        local.set $parent

                                        local.get $new_child 
                                        i32.const 4
                                        i32.add 
                                        local.get $most_left 
                                        i32.store

                                        local.get $most_left
                                        i32.const 8 
                                        i32.add 
                                        local.get $new_child
                                        i32.store

                                        local.get $temp
                                        local.set $most_left 

                                        i32.const 1
                                        local.set $check_left
                                        br 1
                                    else
                                        local.get $most_left
                                        br 0
                                    end

                                end 

                                local.get $new_item
                                local.get $most_left 
                                i32.load
                                i32.store
                                
                                

                                local.get $check_left 
                                if 
                                    local.get $parent 
                                    i32.const 8 
                                    i32.add 
                                    local.get $most_left 
                                    i32.const 12
                                    i32.add
                                    i32.load 
                                    i32.store
                                else 
                                    local.get $parent 
                                    i32.const 12
                                    i32.add 
                                    local.get $most_left 
                                    i32.const 12
                                    i32.add
                                    i32.load 
                                    i32.store
                                end
                                local.get $result
                                br 6
                            end
                        end
                    end 
                end
            end
        end  
    )

    (func $__ligo_internal__set_fold (param $set i32) (result i32)
        i32.const 2
    )

    (func $c_set_left_balancing 
        (param $child i32)
;; local_get_s new_item; 
;;                     call_s "c_set_color";
                    
;;                     if_ (ValBlockType None) 
;;                       [
;;                         (* grand parent *)
;;                         local_get_s new_item;
;;                         const 4l;
;;                         i32_add;
;;                         load;
;;                         local_set_s grand_parent;
                        
               
;;                         const 987654l;
;;                         call_s "print";

;;                         (* only right left case *)

;;                         local_get_s grand_parent;
;;                         const 8l;
;;                         i32_add;
;;                         load;
;;                         const 0l;
;;                         i32_eq;
                        
;;                         local_get_s grand_parent;
;;                         const 12l;
;;                         i32_add;
;;                         load;
;;                         const 0l;
;;                         i32_ne;
;;                         i32_and;


;;                         local_get_s new_item;
;;                         const 8l;
;;                         i32_add;
;;                         load;
;;                         const 0l;
;;                         i32_ne;
                        
;;                         local_get_s new_item;
;;                         const 12l;
;;                         i32_add;
;;                         load;
;;                         const 0l;
;;                         i32_eq;
;;                         i32_and;
;;                         i32_and;

;;                         if_
;;                           (ValBlockType None) 
;;                           [
;;                             (*
;;                               50  
;;                                 60
;;                               55
;;                             *)
;;                             local_get_s grand_parent;
;;                             const 8l;
;;                             i32_add;
;;                             const 0l;
;;                             store;

;;                             local_get_s grand_parent;
;;                             const 12l;
;;                             i32_add;
;;                             local_get_s new_child;
;;                             store;

;;                             local_get_s new_child;
;;                             const 12l;
;;                             i32_add;
;;                             local_get_s new_item;
;;                             store;

;;                             local_get_s new_item;
;;                             const 4l;
;;                             i32_add;
;;                             local_get_s new_child;
;;                             store;

;;                             local_get_s new_item;
;;                             const 8l;
;;                             i32_add;
;;                             const 0l;
;;                             store;

                          

;;                             local_get_s new_child;
;;                             const 4l;
;;                             i32_add;
;;                             local_get_s grand_parent;
;;                             store;

                            
;;                             local_get_s new_item;
;;                             local_set_s temp;

;;                             local_get_s new_child;
;;                             local_set_s new_item;

;;                             local_get_s temp;
;;                             local_set_s new_child;
                     
;;                             (* todo: left rotate... *)
;;                             (* todo: coloring??? *)
;;                           ]
;;                           [                        
;;                         local_get_s grand_parent;
;;                         if_  (ValBlockType None) 
;;                           [
;;                             const 191919l;
;;                             call_s "print";
                        
;;                             (* there is a grand parent *) 
;;                             (* get the uncle *)
;;                             local_get_s grand_parent;
;;                             const 8l; 
;;                             i32_add;
;;                             local_tee_s uncle;   
;;                             load;
;;                             local_get_s new_item;
                            
;;                             i32_eq;
;;                             if_ (ValBlockType None) 
;;                               [
;;                                 local_get_s grand_parent;
;;                                 const 12l; 
;;                                 i32_add;
;;                                 local_set_s uncle;
;;                               ]
;;                               [ ];
;;                               local_get_s new_item;
;;                               load;
;;                               load;
;;                               call_s "print";

;;                               local_get_s uncle;
;;                               load;
;;                             load;
;;                             load;
;;                             call_s "print";

;;                             (* check if there is no uncle *)
;;                             local_get_s uncle;
;;                             load;
;;                             const 0l;
;;                             i32_eq;
;;                             if_ (ValBlockType None) 
;;                               [
;;                                 const 1919192l;
;;                                 call_s "print";
                        
;;                                 (* right rotate *)
;;                                 (* local_get_s new_child;                 
;;                                 call_s "right_rotate";
;;                                 drop at; *)

;;                                 (* make 3 the right child of 2 *)
;;                                 local_get_s new_item;
;;                                 const 12l;
;;                                 i32_add;
;;                                 local_get_s grand_parent;
;;                                 store;

;;                                 (* grand parent parent *)
;;                                 local_get_s grand_parent;
;;                                 const 4l;
;;                                 i32_add;
;;                                 load;
;;                                 local_set_s grand_parent_parent;

;;                                 (* set parent of new right to x *)
;;                                 local_get_s grand_parent;
;;                                 const 4l;
;;                                 i32_add;
;;                                 local_get_s new_item;
;;                                 store;

;;                                 local_get_s grand_parent_parent;
;;                                 const 0l;
;;                                 i32_eq;
;;                                 if_ 
;;                                 (ValBlockType None)
;;                                 [
;;                                   (* there is no grand parent parent - so we can return the new_item as new root *)

;;                                   local_get_s new_item;
;;                                   const 4l;
;;                                   i32_add;
;;                                   const 0l;
;;                                   store;
;; const 12121313l;
;; call_s "print";

;; local_get_s new_item;
;; load;
;; load;
;; (* load; *)
;; call_s "print";
;;                                   local_get_s new_item;
;;                                   br 7l
;;                                 ]
;;                                 [
                                  
;;                                   (* set grand parent child *)
;;                                   local_get_s grand_parent_parent;
;;                                   const 8l;
;;                                   i32_add;
;;                                   local_get_s new_item;
;;                                   store;

;;                                   (* set item parent parent *)
;;                                   local_get_s new_item;
;;                                   const 4l;
;;                                   i32_add;
;;                                   local_get_s grand_parent_parent;
;;                                   store;

;;                                   local_get_s result;
;;                                   br 8l
;;                                 ];

;;                               ]
;;                               [
;;                                 (* there is an uncle *)
;;                                 (* is the uncle red ? *)
;;                                 const 22224l;
;;                                 call_s "print";
;;                               ]
;;                           ]
;;                           [
;;                             (* no grand parent *)
;;                           ]
;;                         ]   
;;                       ]
;;                       []; 
    )

    ;; (func $c_set_add (param $value i32) (param $rbt i32) (param $compare i32) (param $C_SET_EMPTY i32)
    ;;     (local $result i32)
    ;;     (local $new_item i32)
    ;;     (local $current_item i32)
    ;;     (local $comparison_value i32)

    ;;     local.get $rbt 
    ;;     local.get $C_SET_EMPTY
    ;;     i32.eq
    ;;     if 
    ;;         i32.const 20
    ;;         call $malloc
    ;;         local.set $result
    ;;         local.get $value
    ;;         i32.store
    ;;         local.get $result
    ;;     else
    ;;         i32.const 20
    ;;         call $malloc
    ;;         local.set $result 

    ;;         local.get $result
    ;;         local.set $new_item

    ;;         local.get $rbt
    ;;         local.set $current_item

    ;;         loop
    ;;             ;; check where the value should go
    ;;             local.get $compare
    ;;             call_indirect $compare_fun_type
    ;;             local.set $comparison_value
    ;;             if 
    ;;                 local.get comparison_value
    ;;                 i32.const -1;
    ;;                 if
    ;;                     ;; go left
    ;;                     ;; is there a left child already?
    ;;                     if 
    ;;                         ;; no left child 
    ;;                         call $insert_child
    ;;                     else 
    ;;                         ;; there is a left child so continue
    ;;                     end
    ;;                 else 
    ;;                     ;; go right
    ;;                     if 
    ;;                         ;; no right child
    ;;                     else 
    ;;                         ;; continue on the right
    ;;                     end
    ;;                 end
    ;;             else 
    ;;                 ;; equal value
    ;;                 local.get $new_item
    ;;             end
    ;;         end
    ;;     end

    ;; )

    ;; (func $insert_right_child (param $value i32) (param parent $i32) (result i32)

    ;; )

    ;; (func $needs_reordering_rl (param $child i32) (result i32)
    
    ;; ) 

    (func $to_int (param $value i32) (result i32)
        local.get $value
    )

    ;; (table (;0;) 2 2 funcref)
    ;; (elem (;0;) (i32.const 1) func $compare_fn)
    (memory (;0;) 129)
    (export "right_rotate" (func $right_rotate))
)