//   (module
//         (import "env" "syscall" (func $syscall (param i64) (result i32)))
//         (memory (export "memory") 1)
//         (func (export "main")  (param i32) (result i64 i64 i64)
//           i32.const 41
//           i32.const 5
//           i32.store
//           i32.const 46
//           i32.const 0
//           i32.store
//           i64.const 41 
//           call $syscall
//           i64.extend_i32_s
//           (i64.const 40)
//           (i64.const 95)
//           ))
//     |}


// ???