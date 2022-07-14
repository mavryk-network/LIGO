//                                   (module
//                                     (import "env" "syscall" (func $syscall (param i64) (result i32)))
//                                     (memory (export "memory") 1)
//                                     (func (export "main")  (param i32) (result i64 i64 i64)
//                                       i32.const 57
//                                       i32.const 8
//                                       i32.store
//                                       i32.const 62
//                                       i32.const 0
//                                       i32.store
//                                       i32.const 67
//                                       i32.const 8
//                                       i32.store
//                                       i32.const 72
//                                       i64.const 10
//                                       i64.store
//                                       i32.const 81
//                                       i32.const 12
//                                       i32.store
//                                       i64.const 57
//                                       call $syscall
//                                       i64.extend_i32_s
//                                       (i64.const 0)
//                                       i32.const 102
//                                       i32.const 1
//                                       i32.store
//                                       i32.const 107
//                                       i32.const 1
//                                       i32.store
//                                       (i64.const 102)
//                                       ))