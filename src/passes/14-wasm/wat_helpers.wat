(module 
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (param i32) ))
    (type  (func (param i32 i32) (result i32)))
    (type  (func (param i32 i32 i32 i32) (result i32)))
    
    (import "env" "malloc" (func $malloc (type 0)))
    (import "host" "print" (func $print (type 1)))
    (import "wasi_unstable" "fd_write" (func $fd_write (type 3)))
    (import "env" "__indirect_function_table" (table $table 0 funcref))

    (global $__false__tag  i32 (i32.const 0))
    (global $__true__tag   i32 (i32.const 1))
    (global $__int__tag    i32 (i32.const 2))
    (global $__string__tag i32 (i32.const 4))
    (global $__pair__tag   i32 (i32.const 5)) 
    (global $__option__tag  i32 (i32.const 6)) 
    (global $__left__tag    i32 (i32.const 7))  ;; can't we optimize this more? 
    (global $__right__tag   i32 (i32.const 8)) 

    
;; Comparison only works on a class of types that we call comparable. The COMPARE instruction is defined in an ad hoc way for each comparable type, but the result of COMPARE is always an int, which can in turn be checked in a generic manner using the EQ, NEQ, LT, GT, LE and GE combinators.

;; The result of COMPARE is 0 if the top two elements of the stack are equal, negative if the first element in the stack is less than the second, and positive otherwise.

;; address values are compared as follows:
;; addresses of implicit accounts are strictly less than addresses of originated accounts
;; addresses of the same type are compared lexicographically

    (func $compare (param $a i32) (param $b i32) (result i32)
        (local $tag i32)
        (local $a_value i32)
        (local $b_value i32)
        (local $a_size i32)
        (local $b_size i32)
        (local $counter i32)
        (local $x1 i32)
        (local $x12 i32)
        (local $a_char i32)
        (local $b_char i32)
        (local $temp i32)

        

        local.get $a
        i32.load8_u
        local.set $tag

        local.get $tag
        global.get $__int__tag
        i32.eq
        if (result i32)
            local.get $a 
            i32.const 1
            i32.add 
            i32.load 
            local.set $a_value

            local.get $b
            i32.const 1
            i32.add 
            i32.load 
            local.set $b_value

            local.get $a_value 
            local.get $b_value
            i32.eq
            if (result i32)
                i32.const 0
            else 
                local.get $a_value 
                local.get $b_value
                i32.lt_u
                if (result i32)
                    i32.const -1
                else 
                    i32.const 1
                end
            end
        else 
            local.get $tag 
            global.get $__string__tag ;; string tag
            i32.eq 
            if (result i32)
                ;; do a lexical comparison
                local.get $a 
                i32.const 1 
                i32.add 
                i32.load 
                local.set $a_size

                local.get $b
                i32.const 1 
                i32.add 
                i32.load 
                local.set $b_size
                
                local.get $a
                i32.const 5
                i32.add 
                local.set $a_value

                local.get $b
                i32.const 5
                i32.add 
                local.set $b_value

                i32.const 0
                local.set $counter
                loop (result i32)
                    local.get $a_value
                    i32.load 
                    local.set $a_char

                    local.get $b_value
                    i32.load 
                    local.set $b_char
                    
                    local.get $a_char
                    local.get $b_char 
                    i32.lt_u
                    if (result i32)
                        i32.const -1
                    else
                        local.get $a_char 
                        local.get $b_char 
                        i32.gt_u
                        if (result i32)
                            i32.const 1
                        else
                            local.get $counter 
                            local.get $a_size 
                            i32.lt_u
                            local.set $x1 

                            local.get $counter 
                            local.get $b_size 
                            i32.lt_u
                            local.set $x12
                            
                            local.get $x1 
                            i32.const 1
                            i32.eq
                            local.get $x12
                            i32.const 1 
                            i32.eq 
                            i32.and
                            if (result i32)
                                local.get $a_value
                                i32.const 1 
                                i32.add
                                local.set $a_value

                                local.get $b_value
                                i32.const 1 
                                i32.add
                                local.set $b_value

                                local.get $counter 
                                i32.const 1 
                                i32.add 
                                local.set $counter

                                br 3
                            else 
                                local.get $x1 
                                i32.const 1
                                i32.eq
                                local.get $x12
                                i32.const 0
                                i32.eq 
                                i32.and
                                if (result i32)
                                    i32.const -1
                                    ;; br 4
                                else 
                                    local.get $x1 
                                    i32.const 0
                                    i32.eq
                                    local.get $x12
                                    i32.const 1
                                    i32.eq 
                                    i32.and
                                    if (result i32)
                                        i32.const 1 
                                        ;; br 4
                                    else 
                                        i32.const 0
                                        ;; br 4
                                    end
                                end
                            end

                        end
                    end 
                    ;; 

                end
            else 
                local.get $tag
                global.get $__pair__tag 
                i32.eq
                if (result i32)
                    local.get $a
                    i32.const 1
                    i32.add 
                    i32.load

                    local.get $b
                    i32.const 1
                    i32.add 
                    i32.load
                    call $compare
                    local.tee $temp
                    i32.const -1
                    i32.eq
                    if (result i32)
                        i32.const -1
                    else 
                        local.get $temp
                        i32.const 1
                        i32.eq
                        if (result i32)
                            i32.const 1 
                        else 
                            local.get $a
                            i32.const 5
                            i32.add 
                            i32.load

                            local.get $b
                            i32.const 5
                            i32.add 
                            i32.load
                            call $compare
                        end
                    end
                else 
                    local.get $tag
                    global.get $__false__tag ;; or unit
                    i32.eq 
                    if (result i32)
                        local.get $a
                        i32.load 
                        local.set $a
                        local.get $b
                        i32.load
                        local.set $b 
                        local.get $a
                        local.get $b
                        i32.eq
                        if (result i32)
                            i32.const 0
                        else 
                            local.get $a
                            local.get $b 
                            i32.lt_u
                            if (result i32)
                                i32.const -1
                            else 
                                i32.const 1
                            end
                        end
                    else 
                        local.get $tag
                        global.get $__true__tag
                        i32.eq 
                        if (result i32)
                            local.get $a
                            local.get $b 
                            i32.eq
                            if (result i32)
                                i32.const 0
                            else 
                                local.get $a
                                local.get $b 
                                i32.lt_u
                                if (result i32)
                                    i32.const -1
                                else 
                                    i32.const 1
                                end
                            end
                            
                        else 
                            
                            local.get $tag
                            global.get $__option__tag
                            i32.eq 
                            if (result i32)
                                local.get $a
                                i32.const 1
                                i32.add 
                                i32.load
                                local.set $a_value

                                local.get $b
                                i32.const 1
                                i32.add 
                                i32.load
                                local.set $b_value

                                local.get $a_value 
                                local.get $b_value 
                                i32.eq 
                                local.get $a_value 
                                i32.const 0
                                i32.eq 
                                i32.and
                                if (result i32)
                                    i32.const 0 
                                else 
                                    local.get $a_value 
                                    i32.const 0 
                                    i32.eq
                                    if (result i32)
                                        i32.const -1
                                    else 
                                        local.get $a_value 
                                        i32.const 0 
                                        i32.eq
                                        if (result i32)
                                            i32.const 1
                                        else
                                            local.get $a
                                            i32.const 5
                                            i32.add 
                                            i32.load
                                            
                                            local.get $b
                                            i32.const 5
                                            i32.add 
                                            i32.load

                                            call $compare
                                        end
                                    end
                                end
                            else 
                                local.get $tag 
                                global.get $__left__tag
                                i32.eq
                                local.get $tag 
                                global.get $__right__tag
                                i32.eq
                                i32.or
                                if (result i32)
                                    

                                    local.get $a 
                                    i32.load
                                    local.set $a_value

                                    local.get $b 
                                    i32.load
                                    local.set $b_value

                                    local.get $a_value 
                                    local.get $b_value 

                                    i32.lt_u
                                    if (result i32)
                                        i32.const -1
                                    else 
                                        local.get $a_value 
                                        local.get $b_value 
                                        i32.gt_u 
                                        if (result i32)
                                            i32.const 1 
                                        else 
                                            local.get $a 
                                            i32.const 1
                                            i32.add
                                            i32.load
                                            local.set $a_value

                                            local.get $b 
                                            i32.const 1 
                                            i32.add
                                            i32.load
                                            local.set $b_value

                                            local.get $a_value
                                            local.get $b_value 
                                            call $compare
                                            
                                        end
                                    end
                                else 
                                    i32.const 0
                                    unreachable ;; for debugging only
                                end
                            end
                        end 
                    end
                end
            end
        end    
    )

    (func $__ligo_internal__log (param $str_info i32) (result i32)
        (local $str i32)
        (local $str_size i32)
        (local $iov i32)
        (local $nwritten i32)


        local.get $str_info
        local.set $str_size

        local.get $str_info
        i32.const 4
        i32.add
        local.set $str

        i32.const 8
        call $malloc
        
        local.tee $iov
        local.get $str
        i32.store

        local.get $iov
        i32.const 4
        i32.add
        local.get $str_size
        i32.load
        i32.store
        
        local.get $str_size
        call $malloc
        local.set $nwritten

        ;; local.get $nwritten
        ;; i32.const 1
        ;; local.get $iov
        ;; i32.const 1 
        ;; call $fd_write 
       
        ;; drop
        i32.const 0
    )

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

    (func $c_set_add_insert_value (param $value i32) (param $parent i32) (param $position i32) (param $size i32) (result i32)
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

        local.get $child

    )

    (func $__ligo_internal__set_size_in (param $set i32) (result i32)
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
            call $__ligo_internal__set_size_in
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
            call $__ligo_internal__set_size_in
            local.set $right_value
        end

        i32.const 1
        local.get $left_value 
        local.get $right_value 
        i32.add
        i32.add
    )

    (func $__ligo_internal__set_size (param $set i32) (result i32)
        (local $result i32)
        i32.const 8
        call $malloc
        local.tee $result
        global.get $__int__tag
        i32.store8

        local.get $result
        i32.const 1
        i32.add
        local.get $set
        call $__ligo_internal__set_size_in
        i32.store

        local.get $result
    )

    (func $__ligo_internal__set_mem (param $set i32) (param $m i32) (param $C_SET_EMPTY i32) (result i32)
        (local $count i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $left_value i32)
        (local $right_value i32)
        (local $temp i32)
        (local $comparison_result i32)
        
        local.get $set
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)        
            i32.const 0
            br 0
        else 
            loop (result i32)
                local.get $m 
                local.get $set
                i32.load                
                call $compare
                local.tee $comparison_result
                i32.const 0
                i32.eq
                if (result i32)
                
                    i32.const 1
                    br 3
                else 
                    local.get $comparison_result 
                    i32.const -1
                    i32.eq
                    if  (result i32)
                        local.get $set 
                        i32.const 8
                        i32.add
                        i32.load
                        local.tee $left_child
                        if (result i32)
                            local.get $left_child
                            local.set $set 
                            br 3
                        else 
                            i32.const 0
                            br 4
                        end
                    else 
                        local.get $set 
                        i32.const 12
                        i32.add
                        i32.load
                        local.tee $right_child
                        if (result i32)
                            local.get $right_child
                            local.set $set 
                            br 3
                        else 
                            i32.const 0
                            br 4
                        end
                    end
                end
                

            end
        end
    )

    (func $__ligo_internal__map_find_opt (param $set i32) (param $m i32) (param $C_SET_EMPTY i32) (result i32)
        (local $count i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $left_value i32)
        (local $right_value i32)
        (local $temp i32)
        (local $comparison_result i32)
        (local $result i32)

        
        local.get $set
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)  
                  
            i32.const 5
            call $malloc
            local.tee $result
            global.get $__option__tag
            i32.store8
            local.get $result 
            i32.const 1
            i32.add 
            i32.const 0
            i32.store      
            local.get $result
            br 0
        else 
            loop (result i32)
                local.get $m 
                local.get $set
                i32.load                
                call $compare
                local.tee $comparison_result
                i32.const 0
                i32.eq
                if (result i32)  
                    ;; unreachable
                    i32.const 9
                    call $malloc
                    local.tee $result
                    global.get $__option__tag
                    i32.store8

                    local.get $result 
                    i32.const 1
                    i32.add 
                    i32.const 1
                    i32.store

                    local.get $result 
                    i32.const 5
                    i32.add 
                    local.get $set
                    i32.const 20
                    i32.add
                    i32.load   
                    ;; i32.load
                    i32.store      
                    local.get $result
                    br 3
                else 
                    local.get $comparison_result 
                    i32.const -1
                    i32.eq
                    if  (result i32)
                        local.get $set 
                        i32.const 8
                        i32.add
                        i32.load
                        local.tee $left_child
                        if (result i32)
                            local.get $left_child
                            local.set $set 
                            br 3
                        else 
                            i32.const 5
                            call $malloc
                            local.tee $result
                            global.get $__option__tag
                            i32.store8
                            local.get $result 
                            i32.const 1
                            i32.add 
                            i32.const 0
                            i32.store      
                            local.get $result
                            br 4
                        end
                    else 
                        local.get $set 
                        i32.const 12
                        i32.add
                        i32.load
                        local.tee $right_child
                        if (result i32)
                            local.get $right_child
                            local.set $set 
                            br 3
                        else 
                            
                            i32.const 5 
                            call $malloc
                            local.tee $result
                            global.get $__option__tag
                            i32.store
                            local.get $result 
                            i32.const 1
                            i32.add 
                            i32.const 0
                            i32.store      
                            local.get $result
                            br 4
                        end
                    end
                end
            end
        end
    )

   (func $__ligo_internal__set_add (param $set i32) (param $key i32) (param $C_SET_EMPTY i32) (result i32)
        (local $result i32)
        (local $new_item i32)
        (local $current_item i32)
        (local $parent i32)
        (local $compare_result i32)
        (local $new_child i32)
        (local $left_child i32)
        (local $right_child i32)

        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)
            i32.const 20
            call $malloc 
            local.tee $result
            local.get $key 
            i32.store           
            local.get $result
        else
            i32.const 20
            call $malloc 
            local.set $result 

            local.get $result 
            local.set $new_item

            local.get $set 
            local.set $current_item

            loop (result i32)
                local.get $new_item
                local.get $current_item 
                i32.const 20
                memory.copy 

                local.get $new_item 
                i32.const 4
                i32.add 
                local.get $parent
                i32.store

                local.get $new_item 
                local.set $parent

                
                
                local.get $key
                local.get $current_item
                i32.load             
                call $compare
                local.tee $compare_result
                if (result i32)
                    local.get $compare_result 
                    i32.const -1
                    i32.eq 
                    if (result i32)
                        
                        local.get $current_item
                        i32.const 8
                        i32.add
                        i32.load
                        local.tee $left_child
                        i32.const 0 
                        i32.eq
                        if (result i32) 
                            local.get $key 
                            local.get $new_item 
                            i32.const 8
                            i32.const 20
                            call $c_set_add_insert_value
                            drop
                            local.get $result 
                            br 4
                        else
                            i32.const 20
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
                        end
                    else 
                        local.get $current_item
                        i32.const 12
                        i32.add
                        i32.load
                        local.tee $right_child
                        i32.const 0 
                        i32.eq
                        if (result i32)
                            local.get $key 
                            local.get $new_item 
                            i32.const 12 
                            i32.const 20
                            call $c_set_add_insert_value
                            drop
                            local.get $result 
                            br 4
                        else
                            i32.const 20 
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
                        end                    
                    end
                else
                    local.get $new_item
                end           
            end
        end
        drop
        local.get $result
        ;; i32.const 0
    )

    (func $__ligo_internal__map_add (param $set i32) (param $key i32) (param $value i32) (param $C_SET_EMPTY i32) (result i32)
        (local $result i32)
        (local $new_item i32)
        (local $current_item i32)
        (local $parent i32)
        (local $compare_result i32)
        (local $new_child i32)
        (local $left_child i32)
        (local $right_child i32)

        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)
            i32.const 24
            call $malloc 
            local.tee $result
            local.get $key 
            i32.store         

            local.get $result 
            i32.const 20
            i32.add
            local.get $value
            i32.store

            local.get $result
        else
            i32.const 24
            call $malloc 
            local.set $result 

            local.get $result 
            local.set $new_item

            local.get $set 
            local.set $current_item

            loop (result i32)
                local.get $new_item
                local.get $current_item 
                i32.const 24
                memory.copy 

                local.get $new_item 
                i32.const 4
                i32.add 
                local.get $parent
                i32.store

                local.get $new_item 
                local.set $parent

                
                
                local.get $key
                local.get $current_item
                i32.load             
                call $compare
                local.tee $compare_result
                if (result i32)
                    local.get $compare_result 
                    i32.const -1
                    i32.eq 
                    if (result i32)
                        
                        local.get $current_item
                        i32.const 8
                        i32.add
                        i32.load
                        local.tee $left_child
                        i32.const 0 
                        i32.eq
                        if (result i32) 
                            local.get $key 
                            local.get $new_item 
                            i32.const 8
                            i32.const 24
                            call $c_set_add_insert_value
                            i32.const 20
                            i32.add
                            local.get $value 
                            i32.store
                            local.get $result 
                            br 4
                        else
                            i32.const 24
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
                        end
                    else 
                        local.get $current_item
                        i32.const 12
                        i32.add
                        i32.load
                        local.tee $right_child
                        i32.const 0 
                        i32.eq
                        if (result i32)
                            local.get $key 
                            local.get $new_item 
                            i32.const 12 
                            i32.const 24
                            call $c_set_add_insert_value
                            i32.const 20
                            i32.add
                            local.get $value 
                            i32.store
                            local.get $result 
                            br 4
                        else
                            i32.const 24
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
                        end                    
                    end
                else
                    local.get $new_item
                end           
            end
        end
        drop
        local.get $result
    )

    (func $__ligo_internal__map_update_pair (param $previous i32) (param $map i32) (result i32)
        (local $result i32)

        i32.const 9
        call $malloc 
        local.tee $result 
        global.get $__pair__tag
        i32.store8

        local.get $result 
        i32.const 1
        i32.add
        
        local.get $previous
        i32.store

        local.get $result 
        i32.const 5
        i32.add
        local.get $map
        i32.store

        local.get $result
    )

    (func $__ligo_internal__map_update_pair_no_previous (param $map i32) (result i32)
        (local $no_previous i32)

        i32.const 5
        call $malloc
        local.tee $no_previous 
        global.get $__option__tag 
        i32.store8

        local.get $no_previous 
        i32.const 1
        i32.add
        i32.const 0 
        i32.store

        local.get $no_previous
        local.get $map

        call $__ligo_internal__map_update_pair

    )
    

    (func $__ligo_internal__set_remove_in (param $set i32) (param $key i32) (param $SIZE i32) (param $C_SET_EMPTY i32) (result i32)
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
        (local $previous i32)
        

        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq 
        if (result i32)
            local.get $C_SET_EMPTY
            call $__ligo_internal__map_update_pair_no_previous
            
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
                local.get $key                                
                local.get $new_item
                i32.load                
                call $compare
                
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
                        else ;; nothing to delete                            
                            local.get $result
                            call $__ligo_internal__map_update_pair_no_previous
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
                        else ;; nothing to delete
                            local.get $result
                            call $__ligo_internal__map_update_pair_no_previous
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
                            

                            i32.const 9
                                call $malloc 
                                local.tee $previous 
                                global.get $__option__tag
                                i32.store8

                                local.get $previous
                                i32.const 1
                                i32.add 
                                i32.const 1 
                                i32.store

                                local.get $previous
                                i32.const 5
                                i32.add 

                                local.get $current_item
                                i32.const 20 
                                i32.add
                                i32.load

                                i32.store

                                local.get $previous
                                local.get $result
                                call $__ligo_internal__map_update_pair
                        else 
                            local.get $new_item_parent 
                            i32.const 12 
                            i32.add 
                            local.get $new_child 
                            i32.store

                            
                            i32.const 9 
                                call $malloc 
                                local.tee $previous 
                                global.get $__option__tag
                                i32.store8

                                local.get $previous
                                i32.const 1
                                i32.add 
                                i32.const 1 
                                i32.store

                                local.get $previous
                                i32.const 5
                                i32.add 

                                local.get $current_item
                                i32.const 20 
                                i32.add
                                i32.load

                                i32.store

                                local.get $previous
                                local.get $result
                                call $__ligo_internal__map_update_pair
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
                                
                                i32.const 9
                                call $malloc 
                                local.tee $previous 
                                global.get $__option__tag
                                i32.store8

                                local.get $previous
                                i32.const 1
                                i32.add 
                                i32.const 1 
                                i32.store

                                local.get $previous
                                i32.const 5
                                i32.add 

                                local.get $current_item
                                i32.const 20 
                                i32.add
                                i32.load

                                i32.store

                                local.get $previous
                                local.get $result
                                call $__ligo_internal__map_update_pair
                            else 
                                local.get $new_item_parent 
                                i32.const 12 
                                i32.add 
                                local.get $new_child 
                                i32.store
                                

                                i32.const 9 
                                call $malloc 
                                local.tee $previous 
                                global.get $__option__tag
                                i32.store8

                                local.get $previous
                                i32.const 1
                                i32.add 
                                i32.const 1 
                                i32.store

                                local.get $previous
                                i32.const 5
                                i32.add 

                                local.get $current_item
                                i32.const 20 
                                i32.add
                                i32.load

                                i32.store

                                local.get $previous
                                local.get $result
                                call $__ligo_internal__map_update_pair

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

                                i32.const 9
                                call $malloc 
                                local.tee $previous 
                                global.get $__option__tag
                                i32.store8

                                local.get $previous
                                i32.const 1
                                i32.add 
                                i32.const 1 
                                i32.store

                                local.get $previous
                                i32.const 5
                                i32.add 

                                local.get $current_item
                                i32.const 20 
                                i32.add
                                i32.load

                                i32.store

                                local.get $previous
                                local.get $result
                                call $__ligo_internal__map_update_pair
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

                                i32.const 9 
                                call $malloc 
                                local.tee $previous 
                                global.get $__option__tag
                                i32.store8

                                local.get $previous
                                i32.const 1
                                i32.add 
                                i32.const 1 
                                i32.store

                                local.get $previous
                                i32.const 5
                                i32.add 

                                local.get $current_item
                                i32.const 20 
                                i32.add
                                i32.load

                                i32.store
                                local.get $previous
                                local.get $result
                                call $__ligo_internal__map_update_pair

                                br 6
                            end
                        end
                    end 
                end
            end
        end  
    )

    (func $__ligo_internal__set_remove (param $set i32) (param $key i32) (param $SIZE i32) (param $C_SET_EMPTY i32) (result i32)
        local.get $set 
        local.get $key
        local.get $SIZE
        local.get $C_SET_EMPTY
        call $__ligo_internal__set_remove_in
        i32.const 5
        i32.add
        i32.load
    )

    (func $__ligo_internal__set_update (param $set i32) (param $bool i32) (param $key i32) (param $SIZE i32) (param $C_SET_EMPTY i32) (result i32)
        local.get $bool
        if (result i32)
            local.get $set 
            local.get $key
            local.get $C_SET_EMPTY
            call $__ligo_internal__set_add
        else
            local.get $set
            local.get $key
            local.get $SIZE
            local.get $C_SET_EMPTY
            call $__ligo_internal__set_remove
        end
    )

(func $__ligo_internal__map_update_in (param $set i32) (param $key i32) (param $value i32) (param $C_SET_EMPTY i32) (result i32)
        (local $result i32)
        (local $new_item i32)
        (local $current_item i32)
        (local $parent i32)
        (local $compare_result i32)
        (local $new_child i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $tuple i32)
        (local $previous i32)

        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)            
            local.get $result
            call $__ligo_internal__map_update_pair_no_previous
        else
            i32.const 24
            call $malloc 
            local.set $result 

            local.get $result 
            local.set $new_item

            local.get $set 
            local.set $current_item

            loop (result i32)
                local.get $new_item
                local.get $current_item 
                i32.const 24
                memory.copy 

                local.get $new_item 
                i32.const 4
                i32.add 
                local.get $parent
                i32.store

                local.get $new_item 
                local.set $parent

                
                
                local.get $key
                local.get $current_item
                i32.load             
                call $compare
                local.tee $compare_result
                if (result i32)
                    local.get $compare_result 
                    i32.const -1
                    i32.eq 
                    if (result i32)
                        
                        local.get $current_item
                        i32.const 8
                        i32.add
                        i32.load
                        local.tee $left_child
                        i32.const 0 
                        i32.eq
                        if (result i32) 
                            
                            local.get $result
                            call $__ligo_internal__map_update_pair_no_previous
                            br 4
                        else
                            i32.const 24
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
                        end
                    else 
                        local.get $current_item
                        i32.const 12
                        i32.add
                        i32.load
                        local.tee $right_child
                        i32.const 0 
                        i32.eq
                        if (result i32)
                        
                            local.get $result
                            call $__ligo_internal__map_update_pair_no_previous
                            br 4
                        else
                            i32.const 24
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
                        end                    
                    end
                else                              
                    local.get $new_item
                    i32.const 20
                    i32.add
                    local.get $value 
                    i32.store


                    i32.const 9 
                    call $malloc 
                    local.tee $previous 
                    global.get $__option__tag
                    i32.store8 

                    local.get $previous
                    i32.const 1
                    i32.add 
                    i32.const 1 
                    i32.store

                    local.get $previous
                    i32.const 5
                    i32.add 

                    local.get $current_item
                    i32.const 20 
                    i32.add
                    i32.load

                    i32.store

                    local.get $previous

                    local.get $result
                    
                    call $__ligo_internal__map_update_pair

                end           
            end
        end
        ;; drop
        ;; local.get $result
    )

    (func $__ligo_internal__map_update (param $map i32) (param $key i32) (param $value i32) (param $SIZE i32) (param $C_SET_EMPTY i32) (result i32)
        local.get $value
        i32.const 1
        i32.add  
        i32.load
        i32.const 1
        i32.eq
        if (result i32)
            local.get $map 
            local.get $key
            local.get $value
            i32.const 5
            i32.add  
            i32.load
            
            local.get $C_SET_EMPTY
            call $__ligo_internal__map_update_in
            i32.const 5 
            i32.add
            i32.load
        else
            local.get $map
            local.get $key
            local.get $SIZE
            local.get $C_SET_EMPTY
            call $__ligo_internal__set_remove
        end
    )

    (func $__ligo_internal__map_get_update (param $map i32) (param $key i32) (param $value i32) (param $SIZE i32) (param $C_SET_EMPTY i32) (result i32)
        local.get $value
        i32.const 4 
        i32.add  
        i32.load
        i32.const 1
        i32.eq
        if (result i32)
            local.get $map 
            local.get $key
            local.get $value
            i32.const 5
            i32.add  
            i32.load            
            local.get $C_SET_EMPTY
            call $__ligo_internal__map_update_in
        else
            local.get $map
            local.get $key
            local.get $SIZE
            local.get $C_SET_EMPTY
            call $__ligo_internal__set_remove_in
        end
    )

    (func $__ligo_internal__set_iter (param $set i32) (param $fn i32) (param $C_SET_EMPTY i32) (result i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $temp i32)
        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)
            i32.const 0
        else 
            local.get $set 
            i32.const 8
            i32.add
            i32.load
            local.tee $left_child
            if (result i32)
                local.get $left_child
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__set_iter
            else
                i32.const 0
            end
            drop

            local.get $set
            i32.load
            local.get $fn
            call_indirect 0 (type 0) 
            drop

            local.get $set 
            i32.const 12
            i32.add
            i32.load
            local.tee $right_child
            if (result i32)
                local.get $right_child
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__set_iter
            else
                i32.const 0
            end
            drop
            i32.const 0
        end
    )

    (func $__ligo_internal__set_fold (param $set i32) (param $init i32) (param $fn i32) (param $C_SET_EMPTY i32) (result i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $temp i32)
        (local $tuple i32)
        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)
            i32.const 0
        else 
            local.get $set 
            i32.const 8
            i32.add
            i32.load
            local.tee $left_child
            if 
                local.get $left_child
                local.get $init
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__set_fold
                local.set $init
            end
            
            i32.const 9
            call $malloc 
            local.tee $tuple
            global.get $__pair__tag
            i32.store8
            local.get $tuple
            i32.const 1
            i32.add
            local.get $init            
            i32.store
            local.get $tuple
            i32.const 5
            i32.add 
            local.get $set
            i32.load
            i32.store

            local.get $tuple
            local.get $fn
            call_indirect 0 (type 0) 
            local.set $init

            local.get $set 
            i32.const 12
            i32.add
            i32.load
            local.tee $right_child
            if
                local.get $right_child
                local.get $init
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__set_fold
                local.set $init
            end
            local.get $init
        end
        
    )

    (func $__ligo_internal__map_fold (param $set i32) (param $init i32) (param $fn i32) (param $C_SET_EMPTY i32) (result i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $temp i32)
        (local $tuple i32)
        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)
            i32.const 0
        else 
            local.get $set 
            i32.const 8
            i32.add
            i32.load
            local.tee $left_child
            if 
                local.get $left_child
                local.get $init
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__map_fold
                local.set $init
            end
            
            i32.const 9
            call $malloc 
            local.tee $tuple
            global.get $__pair__tag
            i32.store8
            local.get $tuple
            i32.const 1
            i32.add
            local.get $init            
            i32.store
            local.get $tuple
            i32.const 5
            i32.add 
            
            i32.const 9 
            call $malloc 
            local.tee $temp
            global.get $__pair__tag
            i32.store8

            local.get $temp 
            i32.const 1 
            i32.add
            local.get $set
            i32.load
            i32.store

            local.get $temp 
            i32.const 5
            i32.add
            local.get $set
            i32.const 20 
            i32.add
            i32.load
            i32.store

            local.get $temp 

            i32.store

            local.get $tuple
            local.get $fn
            call_indirect 0 (type 0) 
            local.set $init

            local.get $set 
            i32.const 12
            i32.add
            i32.load
            local.tee $right_child
            if
                local.get $right_child
                local.get $init
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__map_fold
                local.set $init
            end
            local.get $init
        end
    )

    (func $__ligo_internal__set_fold_right (param $set i32) (param $init i32) (param $fn i32) (param $C_SET_EMPTY i32) (result i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $temp i32)
        (local $tuple i32)
        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)
            i32.const 0
        else 
            local.get $set 
            i32.const 12
            i32.add
            i32.load
            local.tee $right_child
            if
                local.get $right_child
                local.get $init
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__set_fold_right
                local.set $init
            end
            
            i32.const 9
            call $malloc 
            local.tee $tuple
            global.get $__pair__tag
            i32.store8

            local.get $tuple
            i32.const 1
            i32.add
            local.get $set
            i32.load          
            i32.store
            local.get $tuple
            i32.const 5
            i32.add 
            local.get $init  
            i32.store

            local.get $tuple
            local.get $fn
            call_indirect 0 (type 0) 
            local.set $init

            local.get $set 
            i32.const 8
            i32.add
            i32.load
            local.tee $left_child
            if 
                local.get $left_child
                local.get $init
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__set_fold_right
                local.set $init
            end
            

            
            local.get $init
        end
        
    )
    

    (func $__ligo_internal__map_iter (param $set i32) (param $fn i32) (param $C_SET_EMPTY i32) (result i32)
        (local $left_child i32)
        (local $right_child i32)
        (local $tuple i32)
        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)
            i32.const 0
        else 
            local.get $set 
            i32.const 8
            i32.add
            i32.load
            local.tee $left_child
            if (result i32)
                local.get $left_child
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__map_iter
            else
                i32.const 0
            end
            drop

            ;; TODO: move key and value next to each other to avoid doing this
            i32.const 9
            call $malloc
            local.set $tuple

            local.get $tuple
            global.get $__pair__tag
            i32.store8

            local.get $tuple
            i32.const 1
            i32.add
            local.get $set 
            i32.load
            i32.store

            local.get $tuple
            i32.const 5
            i32.add

            local.get $set            
            i32.const 20
            i32.add
            i32.load
            
            i32.store
            
            local.get $tuple
            local.get $fn
            call_indirect 0 (type 0) 
            drop

            local.get $set 
            i32.const 12
            i32.add
            i32.load
            local.tee $right_child
            if (result i32)
                local.get $right_child
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__map_iter
            else
                i32.const 0
            end
            
        end
    )
    
(func $__ligo_internal__map_map (param $set i32) (param $fn i32) (param $C_SET_EMPTY i32) (result i32)
    (local $left_child i32)
        (local $right_child i32)
        (local $tuple i32)
        (local $new_value i32)
        (local $item i32)
        local.get $set 
        local.get $C_SET_EMPTY
        i32.eq
        if (result i32)
            local.get $C_SET_EMPTY
        else 
            local.get $set 
            i32.const 8
            i32.add
            i32.load
            local.tee $left_child
            if (result i32)
                local.get $left_child
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__map_map
            else
                i32.const 0
            end
            local.set $left_child

            ;; TODO: move key and value next to each other to avoid doing this
            i32.const 9
            call $malloc
            local.set $tuple

            local.get $tuple
            global.get $__pair__tag
            i32.store8

            local.get $tuple
            i32.const 1
            i32.add
            local.get $set 
            i32.load
            i32.store

            local.get $tuple
            i32.const 5
            i32.add
            local.get $set            
            i32.const 20
            i32.add
            i32.load            
            
            i32.store
            
            local.get $tuple
            local.get $fn
            call_indirect 0 (type 0) 
            local.set $new_value

            local.get $set 
            i32.const 12
            i32.add
            i32.load
            local.tee $right_child
            if (result i32)
                local.get $right_child
                local.get $fn
                local.get $C_SET_EMPTY
                call $__ligo_internal__map_map
            else
                i32.const 0
            end
            local.set $right_child

            i32.const 24
            call $malloc
            local.tee $item             
            local.get $set 
            i32.load
            i32.store

            local.get $item 
            i32.const 4 
            i32.add 
            i32.const 0 
            i32.store

            local.get $item 
            i32.const 8
            i32.add
            local.get $left_child 
            i32.store


            local.get $item 
            i32.const 12
            i32.add
            local.get $right_child 
            i32.store
            
            local.get $item 
            i32.const 16
            i32.add
            local.get $set 
            i32.const 16
            i32.add
            i32.load
            i32.store

            local.get $item 
            i32.const 20 
            i32.add 
            local.get $new_value
            i32.store

            local.get $item
            
        end
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
        (local $new_int i32)

        i32.const 5
        call $malloc
        local.tee $new_int
        global.get $__int__tag
        i32.store8

        local.get $new_int
        i32.const 1
        i32.add
        local.get $value
        i32.const 1
        i32.add
        i32.load
        i32.store

        local.get $new_int
    )

    (func $__ligo_internal__string_concat (param $left i32) (param $right i32) (result i32)
      (local $left_size i32)
      (local $left_source i32)
      (local $right_size i32)
      (local $right_source i32)
      (local $new_size i32)
      (local $new_string i32)

      local.get $left
      i32.const 1
      i32.add
      i32.load
      
      local.set $left_size

      local.get $left
      i32.const 5
      i32.add
      local.set $left_source

      local.get $right
      i32.const 1
      i32.add
      i32.load
      
      local.set $right_size
      
      local.get $right
      i32.const 5
      i32.add
      local.set $right_source
      
      local.get $left_size
      local.get $right_size
      i32.add
      local.set $new_size
      
      local.get $new_size
      i32.const 9
      i32.add
      call $malloc
      local.set $new_string

      local.get $new_string
      i32.const 4 ;; string tag
      i32.store8

      local.get $new_string
      i32.const 1 
      i32.add
      local.get $new_size
      i32.store

      local.get $new_string
      i32.const 5
      i32.add
      local.get $left_source
      local.get $left_size
      memory.copy

      local.get $new_string
      i32.const 5
      i32.add
      local.get $left_size
      i32.add
      local.get $right_source
      local.get $right_size
      memory.copy

      local.get $new_string
    )

    (func $__ligo_internal__string_slice 
        (param $offset i32)
        (param $len i32)
        (param $str i32)
        (result i32)
        (local $new_string i32)
        (local $str_size i32)

        local.get $str
        
        i32.const 1
        
        i32.add
        i32.load
        local.set $str_size

        local.get $str_size
        local.get $offset
        i32.const 1
        i32.add
        i32.load
        local.tee $offset
        i32.lt_u
        if (result i32)
            i32.const 0
        else             
            local.get $len
            i32.const 1
            i32.add
            i32.load
            local.set $len

            local.get $str_size
            local.get $len
            local.get $offset 
            i32.add
            i32.lt_u
            if 
                local.get $str_size 
                local.get $offset 
                i32.sub
                local.set $len
            end

            local.get $len
            i32.const 5
            i32.add
            call $malloc
            local.set $new_string

            local.get $new_string
            i32.const 4
            i32.store8

            local.get $new_string
            i32.const 1
            i32.add
            local.get $len
            i32.store

            local.get $new_string
            i32.const 5
            i32.add

            local.get $str
            i32.const 5
            i32.add
            local.get $offset 
            i32.add
            
            local.get $len
            

            memory.copy

            local.get $new_string
        end
    )

    (func $__ligo_internal__list_iter (param $list i32) (param $fn i32) (param $C_LIST_EMPTY i32) (result i32)
        (local $next_item i32)
        local.get $list 
        local.get $C_LIST_EMPTY
        i32.eq
        if (result i32)
            i32.const 0
        else        
            loop 
                local.get $list
                i32.const 4
                i32.add
                i32.load
                local.set $next_item

                local.get $list 
                i32.load
                local.set $list
                
                local.get $list
                local.get $fn
                call_indirect 0 (type 0)
                drop

                local.get $next_item 
                local.set $list

                local.get $next_item 
                local.get $C_LIST_EMPTY
                i32.ne
                br_if 0
            end
            i32.const 0
        end
    )

    (func $__ligo_internal__list_size (param $list i32) (param $C_LIST_EMPTY i32) (result i32)
        (local $counter i32)
        (local $return i32)

        i32.const 8
        call $malloc 
        local.set $return 

        local.get $return 
        global.get $__int__tag
        i32.store8

        local.get $list 
        local.get $C_LIST_EMPTY
        i32.eq
        if 
            local.get $return 
            i32.const 1
            i32.add
            i32.const 0 
            i32.store
        else 
            i32.const 0 
            local.set $counter 
            loop 
                local.get $counter
                i32.const 1
                i32.add 
                local.set $counter 

                local.get $list 
                i32.const 4
                i32.add 
                i32.load 
                local.set $list 

                local.get $list 
                local.get $C_LIST_EMPTY
                i32.ne 
                br_if 0
            end
            local.get $return 
            i32.const 1
            i32.add
            local.get $counter
            i32.store
        end
        local.get $return
      
    )

    (func $__ligo_internal__list_map (param $list i32) (param $fn i32) (param $C_LIST_EMPTY i32) (result i32)
        (local $result_iter i32)
        (local $result i32)
        (local $next_item i32)
        (local $result_iter_next i32)


        local.get $list 
        if (result i32)
            i32.const 8
            call $malloc 
            local.tee $result_iter
            local.set $result
            loop (result i32)
                local.get $list
                i32.const 4
                i32.add
                i32.load
                local.set $next_item

                local.get $next_item 
                i32.load
                if 
                    i32.const 8 
                    call $malloc 
                    local.set $result_iter_next
                else 
                    local.get $C_LIST_EMPTY
                    local.set $result_iter_next
                end
                local.get $result_iter



                local.get $list
                i32.load
                local.get $fn
                call_indirect 0 (type 0)
                i32.store 

                local.get $result_iter
                i32.const 4 
                i32.add 
                local.get $result_iter_next
                i32.store 

                local.get $result_iter_next
                local.set $result_iter

                local.get $next_item 
                local.set $list

                local.get $result 
                local.get $next_item 
                i32.load
                i32.const 0 
                i32.ne 
                br_if 0
            end

        else 
            local.get $C_LIST_EMPTY
        end
    )

    (func $__ligo_internal__list_fold (param $list i32) (param $init i32)  (param $fn i32) (param $C_LIST_EMPTY i32) (result i32)
        (local $next_item i32)
        (local $result i32)
        (local $tuple i32)

        local.get $list 
        local.get $C_LIST_EMPTY
        i32.eq 
        if (result i32)
            local.get $init
        else
            loop (result i32)
                i32.const 9 
                call $malloc 
                local.tee $tuple 
                global.get $__pair__tag
                i32.store8
                local.get $tuple
                i32.const 1
                i32.add
                local.get $init 
                i32.store
                local.get $tuple
                i32.const 5
                i32.add
                local.get $list 
                i32.load
                i32.store

                local.get $tuple 
                local.get $fn
                call_indirect 0 (type 0)
                local.set $result

                local.get $result 
                local.set $init 

                local.get $list
                i32.const 4
                i32.add
                i32.load
                local.set $next_item

                local.get $next_item
                local.set $list

                local.get $result

                local.get $list 
                local.get $next_item
                i32.load
                i32.ne 
                br_if 0 
            end

        end
    )

    (func $__ligo_internal__list_fold_right (param $list i32) (param $init i32) (param $fn i32) (param $C_LIST_EMPTY i32) (result i32)
        (local $tuple i32)

        local.get $list 
        local.get $C_LIST_EMPTY 
        i32.eq
        if (result i32)
            local.get $init 
        else 
            local.get $list 
            i32.const 4
            i32.add
            i32.load
            local.get $init
            local.get $fn
            local.get $C_LIST_EMPTY
            call $__ligo_internal__list_fold_right 
            local.set $init

            i32.const 9
            call $malloc 
            local.tee $tuple 
            global.get $__pair__tag
            i32.store8

            local.get $tuple 
            i32.const 1
            i32.add
            local.get $list 
            i32.load
            i32.store

            local.get $tuple 
            i32.const 5
            i32.add
            local.get $init 
            i32.store

            local.get $tuple
            local.get $fn
            call_indirect 0 (type 0)
        end 
    )


    (memory (;0;) 1)
)