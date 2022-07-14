(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32)))
  (type (;3;) (func (param i32 i32 i32) (result i32)))
  (type (;4;) (func))
  (type (;5;) (func (param i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
  (type (;6;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;7;) (func (param i32 i32)))
  (import "env" "__linear_memory" (memory (;0;) 1))
  (import "env" "__stack_pointer" (global (;0;) (mut i32)))
  (import "env" "malloc" (func (;0;) (type 1)))
  (import "env" "printf" (func (;1;) (type 0)))
  (import "env" "__wasi_path_open" (func (;2;) (type 5)))
  (import "env" "__wasi_proc_exit" (func (;3;) (type 2)))
  (import "env" "__wasi_fd_filestat_get" (func (;4;) (type 0)))
  (import "env" "__wasi_fd_close" (func (;5;) (type 1)))
  (import "env" "__wasi_fd_read" (func (;6;) (type 6)))
  (import "env" "__load" (func (;7;) (type 2)))
  (import "env" "entrypoint" (func (;8;) (type 0)))
  (import "env" "__save" (func (;9;) (type 7)))
  (import "env" "__wasi_fd_write" (func (;10;) (type 6)))
  (import "env" "free" (func (;11;) (type 2)))
  (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
  (func $newNode (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 2
    i32.const 16
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    local.get 4
    global.set 0
    local.get 4
    local.get 0
    i32.store offset=12
    local.get 4
    local.get 1
    i32.store offset=8
    i32.const 20
    local.set 5
    local.get 5
    call 0
    local.set 6
    local.get 4
    local.get 6
    i32.store offset=4
    local.get 4
    i32.load offset=4
    local.set 7
    i32.const 0
    local.set 8
    local.get 7
    local.get 8
    i32.store8
    local.get 4
    i32.load offset=4
    local.set 9
    i32.const 0
    local.set 10
    local.get 9
    local.get 10
    i32.store offset=4
    local.get 4
    i32.load offset=12
    local.set 11
    local.get 4
    i32.load offset=4
    local.set 12
    local.get 12
    local.get 11
    i32.store offset=8
    local.get 4
    i32.load offset=4
    local.set 13
    i32.const 0
    local.set 14
    local.get 13
    local.get 14
    i32.store offset=12
    local.get 4
    i32.load offset=4
    local.set 15
    i32.const 0
    local.set 16
    local.get 15
    local.get 16
    i32.store offset=16
    local.get 4
    i32.load offset=4
    local.set 17
    i32.const 16
    local.set 18
    local.get 4
    local.get 18
    i32.add
    local.set 19
    local.get 19
    global.set 0
    local.get 17
    return)
  (func $cloneNode (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=12
    i32.const 20
    local.set 4
    local.get 4
    call 0
    local.set 5
    local.get 3
    local.get 5
    i32.store offset=8
    local.get 3
    i32.load offset=12
    local.set 6
    local.get 6
    i32.load8_u
    local.set 7
    local.get 3
    i32.load offset=8
    local.set 8
    i32.const 1
    local.set 9
    local.get 7
    local.get 9
    i32.and
    local.set 10
    local.get 8
    local.get 10
    i32.store8
    local.get 3
    i32.load offset=12
    local.set 11
    local.get 11
    i32.load offset=4
    local.set 12
    local.get 3
    i32.load offset=8
    local.set 13
    local.get 13
    local.get 12
    i32.store offset=4
    local.get 3
    i32.load offset=12
    local.set 14
    local.get 14
    i32.load offset=8
    local.set 15
    local.get 3
    i32.load offset=8
    local.set 16
    local.get 16
    local.get 15
    i32.store offset=8
    local.get 3
    i32.load offset=12
    local.set 17
    local.get 17
    i32.load offset=12
    local.set 18
    local.get 3
    i32.load offset=8
    local.set 19
    local.get 19
    local.get 18
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 20
    local.get 20
    i32.load offset=16
    local.set 21
    local.get 3
    i32.load offset=8
    local.set 22
    local.get 22
    local.get 21
    i32.store offset=16
    local.get 3
    i32.load offset=8
    local.set 23
    i32.const 16
    local.set 24
    local.get 3
    local.get 24
    i32.add
    local.set 25
    local.get 25
    global.set 0
    local.get 23
    return)
  (func $top (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=8
    local.get 3
    i32.load offset=8
    local.set 4
    local.get 4
    i32.load offset=4
    local.set 5
    i32.const 0
    local.set 6
    local.get 5
    local.set 7
    local.get 6
    local.set 8
    local.get 7
    local.get 8
    i32.eq
    local.set 9
    i32.const 1
    local.set 10
    local.get 9
    local.get 10
    i32.and
    local.set 11
    block  ;; label = @1
      block  ;; label = @2
        local.get 11
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=8
        local.set 12
        local.get 3
        local.get 12
        i32.store offset=12
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=8
      local.set 13
      local.get 13
      i32.load offset=4
      local.set 14
      local.get 14
      call $top
      local.set 15
      local.get 3
      local.get 15
      i32.store offset=12
    end
    local.get 3
    i32.load offset=12
    local.set 16
    i32.const 16
    local.set 17
    local.get 3
    local.get 17
    i32.add
    local.set 18
    local.get 18
    global.set 0
    local.get 16
    return)
  (func $rotateLeft (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    local.get 4
    i32.load offset=16
    local.set 5
    local.get 3
    local.get 5
    i32.store offset=8
    local.get 3
    i32.load offset=12
    local.set 6
    local.get 6
    i32.load offset=12
    local.set 7
    local.get 3
    local.get 7
    i32.store offset=4
    local.get 3
    i32.load offset=12
    local.set 8
    local.get 3
    i32.load offset=8
    local.set 9
    local.get 9
    local.get 8
    i32.store offset=12
    local.get 3
    i32.load offset=4
    local.set 10
    local.get 3
    i32.load offset=8
    local.set 11
    local.get 11
    local.get 10
    i32.store offset=16
    local.get 3
    i32.load offset=8
    local.set 12
    local.get 3
    i32.load offset=12
    local.set 13
    local.get 13
    local.get 12
    i32.store offset=4
    local.get 3
    i32.load offset=4
    local.set 14
    i32.const 0
    local.set 15
    local.get 14
    local.set 16
    local.get 15
    local.set 17
    local.get 16
    local.get 17
    i32.ne
    local.set 18
    i32.const 1
    local.set 19
    local.get 18
    local.get 19
    i32.and
    local.set 20
    block  ;; label = @1
      local.get 20
      i32.eqz
      br_if 0 (;@1;)
      local.get 3
      i32.load offset=12
      local.set 21
      local.get 3
      i32.load offset=4
      local.set 22
      local.get 22
      local.get 21
      i32.store offset=4
    end
    local.get 3
    i32.load offset=8
    local.set 23
    local.get 23
    return)
  (func $rotateRight (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    local.get 4
    i32.load offset=12
    local.set 5
    local.get 3
    local.get 5
    i32.store offset=8
    local.get 3
    i32.load offset=12
    local.set 6
    local.get 6
    i32.load offset=16
    local.set 7
    local.get 3
    local.get 7
    i32.store offset=4
    local.get 3
    i32.load offset=12
    local.set 8
    local.get 3
    i32.load offset=8
    local.set 9
    local.get 9
    local.get 8
    i32.store offset=16
    local.get 3
    i32.load offset=4
    local.set 10
    local.get 3
    i32.load offset=8
    local.set 11
    local.get 11
    local.get 10
    i32.store offset=12
    local.get 3
    i32.load offset=8
    local.set 12
    local.get 3
    i32.load offset=12
    local.set 13
    local.get 13
    local.get 12
    i32.store offset=4
    local.get 3
    i32.load offset=4
    local.set 14
    i32.const 0
    local.set 15
    local.get 14
    local.set 16
    local.get 15
    local.set 17
    local.get 16
    local.get 17
    i32.ne
    local.set 18
    i32.const 1
    local.set 19
    local.get 18
    local.get 19
    i32.and
    local.set 20
    block  ;; label = @1
      local.get 20
      i32.eqz
      br_if 0 (;@1;)
      local.get 3
      i32.load offset=12
      local.set 21
      local.get 3
      i32.load offset=4
      local.set 22
      local.get 22
      local.get 21
      i32.store offset=4
    end
    local.get 3
    i32.load offset=8
    local.set 23
    local.get 23
    return)
  (func $uncle (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    local.get 0
    i32.store offset=8
    local.get 3
    i32.load offset=8
    local.set 4
    local.get 3
    i32.load offset=8
    local.set 5
    local.get 5
    i32.load offset=4
    local.set 6
    local.get 6
    i32.load offset=12
    local.set 7
    local.get 4
    local.set 8
    local.get 7
    local.set 9
    local.get 8
    local.get 9
    i32.eq
    local.set 10
    i32.const 1
    local.set 11
    local.get 10
    local.get 11
    i32.and
    local.set 12
    block  ;; label = @1
      block  ;; label = @2
        local.get 12
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=8
        local.set 13
        local.get 13
        i32.load offset=4
        local.set 14
        local.get 14
        i32.load offset=16
        local.set 15
        local.get 3
        local.get 15
        i32.store offset=12
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=8
      local.set 16
      local.get 16
      i32.load offset=4
      local.set 17
      local.get 17
      i32.load offset=12
      local.set 18
      local.get 3
      local.get 18
      i32.store offset=12
    end
    local.get 3
    i32.load offset=12
    local.set 19
    local.get 19
    return)
  (func $insertCase5 (type 2) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    local.get 4
    i32.load offset=4
    local.set 5
    i32.const 1
    local.set 6
    local.get 5
    local.get 6
    i32.store8
    local.get 3
    i32.load offset=12
    local.set 7
    local.get 7
    i32.load offset=4
    local.set 8
    local.get 8
    i32.load offset=4
    local.set 9
    i32.const 0
    local.set 10
    local.get 9
    local.get 10
    i32.store8
    local.get 3
    i32.load offset=12
    local.set 11
    local.get 3
    i32.load offset=12
    local.set 12
    local.get 12
    i32.load offset=4
    local.set 13
    local.get 13
    i32.load offset=12
    local.set 14
    local.get 11
    local.set 15
    local.get 14
    local.set 16
    local.get 15
    local.get 16
    i32.eq
    local.set 17
    i32.const 1
    local.set 18
    local.get 17
    local.get 18
    i32.and
    local.set 19
    block  ;; label = @1
      block  ;; label = @2
        local.get 19
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=12
        local.set 20
        local.get 20
        i32.load offset=4
        local.set 21
        local.get 3
        i32.load offset=12
        local.set 22
        local.get 22
        i32.load offset=4
        local.set 23
        local.get 23
        i32.load offset=4
        local.set 24
        local.get 24
        i32.load offset=12
        local.set 25
        local.get 21
        local.set 26
        local.get 25
        local.set 27
        local.get 26
        local.get 27
        i32.eq
        local.set 28
        i32.const 1
        local.set 29
        local.get 28
        local.get 29
        i32.and
        local.set 30
        local.get 30
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=12
        local.set 31
        local.get 31
        call $rotateRight
        drop
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 32
      local.get 32
      call $rotateLeft
      drop
    end
    i32.const 16
    local.set 33
    local.get 3
    local.get 33
    i32.add
    local.set 34
    local.get 34
    global.set 0
    return)
  (func $insertCase4 (type 2) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    local.get 3
    i32.load offset=12
    local.set 5
    local.get 5
    i32.load offset=4
    local.set 6
    local.get 6
    i32.load offset=16
    local.set 7
    local.get 4
    local.set 8
    local.get 7
    local.set 9
    local.get 8
    local.get 9
    i32.eq
    local.set 10
    i32.const 1
    local.set 11
    local.get 10
    local.get 11
    i32.and
    local.set 12
    block  ;; label = @1
      block  ;; label = @2
        local.get 12
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=12
        local.set 13
        local.get 13
        i32.load offset=4
        local.set 14
        local.get 3
        i32.load offset=12
        local.set 15
        local.get 15
        i32.load offset=4
        local.set 16
        local.get 16
        i32.load offset=4
        local.set 17
        local.get 17
        i32.load offset=12
        local.set 18
        local.get 14
        local.set 19
        local.get 18
        local.set 20
        local.get 19
        local.get 20
        i32.eq
        local.set 21
        i32.const 1
        local.set 22
        local.get 21
        local.get 22
        i32.and
        local.set 23
        local.get 23
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=12
        local.set 24
        local.get 24
        i32.load offset=4
        local.set 25
        local.get 25
        call $rotateLeft
        drop
        local.get 3
        i32.load offset=12
        local.set 26
        local.get 26
        i32.load offset=12
        local.set 27
        local.get 3
        local.get 27
        i32.store offset=12
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 28
      local.get 3
      i32.load offset=12
      local.set 29
      local.get 29
      i32.load offset=4
      local.set 30
      local.get 30
      i32.load offset=12
      local.set 31
      local.get 28
      local.set 32
      local.get 31
      local.set 33
      local.get 32
      local.get 33
      i32.eq
      local.set 34
      i32.const 1
      local.set 35
      local.get 34
      local.get 35
      i32.and
      local.set 36
      block  ;; label = @2
        local.get 36
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=12
        local.set 37
        local.get 37
        i32.load offset=4
        local.set 38
        local.get 3
        i32.load offset=12
        local.set 39
        local.get 39
        i32.load offset=4
        local.set 40
        local.get 40
        i32.load offset=4
        local.set 41
        local.get 41
        i32.load offset=16
        local.set 42
        local.get 38
        local.set 43
        local.get 42
        local.set 44
        local.get 43
        local.get 44
        i32.eq
        local.set 45
        i32.const 1
        local.set 46
        local.get 45
        local.get 46
        i32.and
        local.set 47
        local.get 47
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=12
        local.set 48
        local.get 48
        i32.load offset=4
        local.set 49
        local.get 49
        call $rotateRight
        drop
        local.get 3
        i32.load offset=12
        local.set 50
        local.get 50
        i32.load offset=16
        local.set 51
        local.get 3
        local.get 51
        i32.store offset=12
      end
    end
    local.get 3
    i32.load offset=12
    local.set 52
    local.get 52
    call $insertCase5
    i32.const 16
    local.set 53
    local.get 3
    local.get 53
    i32.add
    local.set 54
    local.get 54
    global.set 0
    return)
  (func $insertCase3 (type 2) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    local.get 4
    call $uncle
    local.set 5
    local.get 3
    local.get 5
    i32.store offset=8
    local.get 3
    i32.load offset=8
    local.set 6
    local.get 6
    i32.load8_u
    local.set 7
    i32.const 1
    local.set 8
    local.get 7
    local.get 8
    i32.and
    local.set 9
    block  ;; label = @1
      block  ;; label = @2
        local.get 9
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=12
        local.set 10
        local.get 10
        i32.load offset=4
        local.set 11
        i32.const 1
        local.set 12
        local.get 11
        local.get 12
        i32.store8
        local.get 3
        i32.load offset=8
        local.set 13
        i32.const 1
        local.set 14
        local.get 13
        local.get 14
        i32.store8
        local.get 3
        i32.load offset=12
        local.set 15
        local.get 15
        i32.load offset=4
        local.set 16
        local.get 16
        i32.load offset=4
        local.set 17
        i32.const 0
        local.set 18
        local.get 17
        local.get 18
        i32.store8
        local.get 3
        i32.load offset=12
        local.set 19
        local.get 19
        i32.load offset=4
        local.set 20
        local.get 20
        i32.load offset=4
        local.set 21
        local.get 21
        call $insertCase1
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 22
      local.get 22
      call $insertCase4
    end
    i32.const 16
    local.set 23
    local.get 3
    local.get 23
    i32.add
    local.set 24
    local.get 24
    global.set 0
    return)
  (func $insertCase1 (type 2) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    local.get 4
    i32.load offset=4
    local.set 5
    i32.const 0
    local.set 6
    local.get 5
    local.set 7
    local.get 6
    local.set 8
    local.get 7
    local.get 8
    i32.eq
    local.set 9
    i32.const 1
    local.set 10
    local.get 9
    local.get 10
    i32.and
    local.set 11
    block  ;; label = @1
      block  ;; label = @2
        local.get 11
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=12
        local.set 12
        i32.const 1
        local.set 13
        local.get 12
        local.get 13
        i32.store8
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 14
      local.get 14
      call $insertCase2
    end
    i32.const 16
    local.set 15
    local.get 3
    local.get 15
    i32.add
    local.set 16
    local.get 16
    global.set 0
    return)
  (func $insertCase2 (type 2) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    local.get 4
    i32.load offset=4
    local.set 5
    local.get 5
    i32.load8_u
    local.set 6
    i32.const 1
    local.set 7
    local.get 6
    local.get 7
    i32.and
    local.set 8
    i32.const 1
    local.set 9
    local.get 8
    local.set 10
    local.get 9
    local.set 11
    local.get 10
    local.get 11
    i32.eq
    local.set 12
    i32.const 1
    local.set 13
    local.get 12
    local.get 13
    i32.and
    local.set 14
    block  ;; label = @1
      block  ;; label = @2
        local.get 14
        i32.eqz
        br_if 0 (;@2;)
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 15
      local.get 15
      call $insertCase3
    end
    i32.const 16
    local.set 16
    local.get 3
    local.get 16
    i32.add
    local.set 17
    local.get 17
    global.set 0
    return)
  (func $insertNode (type 3) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 3
    i32.const 48
    local.set 4
    local.get 3
    local.get 4
    i32.sub
    local.set 5
    local.get 5
    global.set 0
    local.get 5
    local.get 0
    i32.store offset=40
    local.get 5
    local.get 1
    i32.store offset=36
    local.get 5
    local.get 2
    i32.store offset=32
    local.get 5
    i32.load offset=40
    local.set 6
    local.get 5
    i32.load offset=36
    local.set 7
    local.get 6
    local.get 7
    call $newNode
    local.set 8
    local.get 5
    local.get 8
    i32.store offset=28
    local.get 5
    i32.load offset=36
    local.set 9
    local.get 9
    call $cloneNode
    local.set 10
    local.get 5
    local.get 10
    i32.store offset=24
    block  ;; label = @1
      block  ;; label = @2
        loop  ;; label = @3
          local.get 5
          i32.load offset=24
          local.set 11
          i32.const 0
          local.set 12
          local.get 11
          local.set 13
          local.get 12
          local.set 14
          local.get 13
          local.get 14
          i32.ne
          local.set 15
          i32.const 1
          local.set 16
          local.get 15
          local.get 16
          i32.and
          local.set 17
          local.get 17
          i32.eqz
          br_if 1 (;@2;)
          local.get 5
          i32.load offset=32
          local.set 18
          local.get 5
          i32.load offset=24
          local.set 19
          local.get 19
          i32.load offset=8
          local.set 20
          local.get 5
          i32.load offset=40
          local.set 21
          local.get 20
          local.get 21
          local.get 18
          call_indirect (type 0)
          local.set 22
          local.get 5
          local.get 22
          i32.store8 offset=23
          local.get 5
          i32.load8_u offset=23
          local.set 23
          i32.const 24
          local.set 24
          local.get 23
          local.get 24
          i32.shl
          local.set 25
          local.get 25
          local.get 24
          i32.shr_s
          local.set 26
          i32.const -1
          local.set 27
          local.get 26
          local.set 28
          local.get 27
          local.set 29
          local.get 28
          local.get 29
          i32.eq
          local.set 30
          i32.const 1
          local.set 31
          local.get 30
          local.get 31
          i32.and
          local.set 32
          block  ;; label = @4
            block  ;; label = @5
              local.get 32
              i32.eqz
              br_if 0 (;@5;)
              local.get 5
              i32.load offset=24
              local.set 33
              local.get 33
              i32.load offset=12
              local.set 34
              i32.const 0
              local.set 35
              local.get 34
              local.set 36
              local.get 35
              local.set 37
              local.get 36
              local.get 37
              i32.ne
              local.set 38
              i32.const 1
              local.set 39
              local.get 38
              local.get 39
              i32.and
              local.set 40
              block  ;; label = @6
                block  ;; label = @7
                  local.get 40
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 5
                  i32.load offset=24
                  local.set 41
                  local.get 41
                  i32.load offset=12
                  local.set 42
                  local.get 42
                  call $cloneNode
                  local.set 43
                  local.get 5
                  local.get 43
                  i32.store offset=16
                  local.get 5
                  i32.load offset=16
                  local.set 44
                  local.get 5
                  i32.load offset=24
                  local.set 45
                  local.get 45
                  local.get 44
                  i32.store offset=12
                  local.get 5
                  i32.load offset=16
                  local.set 46
                  local.get 5
                  local.get 46
                  i32.store offset=24
                  br 1 (;@6;)
                end
                local.get 5
                i32.load offset=28
                local.set 47
                local.get 5
                i32.load offset=24
                local.set 48
                local.get 48
                local.get 47
                i32.store offset=12
                br 4 (;@2;)
              end
              br 1 (;@4;)
            end
            local.get 5
            i32.load8_u offset=23
            local.set 49
            i32.const 24
            local.set 50
            local.get 49
            local.get 50
            i32.shl
            local.set 51
            local.get 51
            local.get 50
            i32.shr_s
            local.set 52
            i32.const 1
            local.set 53
            local.get 52
            local.set 54
            local.get 53
            local.set 55
            local.get 54
            local.get 55
            i32.eq
            local.set 56
            i32.const 1
            local.set 57
            local.get 56
            local.get 57
            i32.and
            local.set 58
            block  ;; label = @5
              block  ;; label = @6
                local.get 58
                i32.eqz
                br_if 0 (;@6;)
                local.get 5
                i32.load offset=24
                local.set 59
                local.get 59
                i32.load offset=16
                local.set 60
                i32.const 0
                local.set 61
                local.get 60
                local.set 62
                local.get 61
                local.set 63
                local.get 62
                local.get 63
                i32.ne
                local.set 64
                i32.const 1
                local.set 65
                local.get 64
                local.get 65
                i32.and
                local.set 66
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 66
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 5
                    i32.load offset=24
                    local.set 67
                    local.get 67
                    i32.load offset=16
                    local.set 68
                    local.get 68
                    call $cloneNode
                    local.set 69
                    local.get 5
                    local.get 69
                    i32.store offset=12
                    local.get 5
                    i32.load offset=12
                    local.set 70
                    local.get 5
                    i32.load offset=24
                    local.set 71
                    local.get 71
                    local.get 70
                    i32.store offset=16
                    local.get 5
                    i32.load offset=12
                    local.set 72
                    local.get 5
                    local.get 72
                    i32.store offset=24
                    br 1 (;@7;)
                  end
                  local.get 5
                  i32.load offset=28
                  local.set 73
                  local.get 5
                  i32.load offset=24
                  local.set 74
                  local.get 74
                  local.get 73
                  i32.store offset=16
                  br 5 (;@2;)
                end
                br 1 (;@5;)
              end
              i32.const 36
              local.set 75
              local.get 5
              local.get 75
              i32.add
              local.set 76
              local.get 76
              local.set 77
              local.get 5
              local.get 77
              i32.store offset=44
              br 4 (;@1;)
            end
          end
          br 0 (;@3;)
        end
      end
      local.get 5
      i32.load offset=24
      local.set 78
      local.get 78
      call $insertCase1
      local.get 5
      i32.load offset=36
      local.set 79
      local.get 79
      call $top
      local.set 80
      local.get 5
      local.get 80
      i32.store offset=44
    end
    local.get 5
    i32.load offset=44
    local.set 81
    i32.const 48
    local.set 82
    local.get 5
    local.get 82
    i32.add
    local.set 83
    local.get 83
    global.set 0
    local.get 81
    return)
  (func $__comparez (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 2
    i32.const 16
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    local.get 4
    local.get 0
    i32.store offset=8
    local.get 4
    local.get 1
    i32.store offset=4
    local.get 4
    i32.load offset=8
    local.set 5
    local.get 4
    i32.load offset=4
    local.set 6
    local.get 5
    local.set 7
    local.get 6
    local.set 8
    local.get 7
    local.get 8
    i32.lt_u
    local.set 9
    i32.const 1
    local.set 10
    local.get 9
    local.get 10
    i32.and
    local.set 11
    block  ;; label = @1
      block  ;; label = @2
        local.get 11
        i32.eqz
        br_if 0 (;@2;)
        i32.const 255
        local.set 12
        local.get 4
        local.get 12
        i32.store8 offset=15
        br 1 (;@1;)
      end
      local.get 4
      i32.load offset=8
      local.set 13
      local.get 4
      i32.load offset=4
      local.set 14
      local.get 13
      local.set 15
      local.get 14
      local.set 16
      local.get 15
      local.get 16
      i32.gt_u
      local.set 17
      i32.const 1
      local.set 18
      local.get 17
      local.get 18
      i32.and
      local.set 19
      block  ;; label = @2
        local.get 19
        i32.eqz
        br_if 0 (;@2;)
        i32.const 1
        local.set 20
        local.get 4
        local.get 20
        i32.store8 offset=15
        br 1 (;@1;)
      end
      i32.const 0
      local.set 21
      local.get 4
      local.get 21
      i32.store8 offset=15
    end
    local.get 4
    i32.load8_u offset=15
    local.set 22
    i32.const 24
    local.set 23
    local.get 22
    local.get 23
    i32.shl
    local.set 24
    local.get 24
    local.get 23
    i32.shr_s
    local.set 25
    local.get 25
    return)
  (func $report_error (type 2) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    i32.const 2
    local.set 5
    local.get 4
    local.set 6
    local.get 5
    local.set 7
    local.get 6
    local.get 7
    i32.eq
    local.set 8
    i32.const 1
    local.set 9
    local.get 8
    local.get 9
    i32.and
    local.set 10
    block  ;; label = @1
      block  ;; label = @2
        local.get 10
        i32.eqz
        br_if 0 (;@2;)
        i32.const 0
        local.set 11
        i32.const 0
        local.set 12
        local.get 11
        local.get 12
        call 1
        drop
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 13
      i32.const 76
      local.set 14
      local.get 13
      local.set 15
      local.get 14
      local.set 16
      local.get 15
      local.get 16
      i32.eq
      local.set 17
      i32.const 1
      local.set 18
      local.get 17
      local.get 18
      i32.and
      local.set 19
      block  ;; label = @2
        block  ;; label = @3
          local.get 19
          i32.eqz
          br_if 0 (;@3;)
          i32.const 0
          local.set 20
          i32.const 0
          local.set 21
          local.get 20
          local.get 21
          call 1
          drop
          br 1 (;@2;)
        end
        local.get 3
        i32.load offset=12
        local.set 22
        i32.const 69
        local.set 23
        local.get 22
        local.set 24
        local.get 23
        local.set 25
        local.get 24
        local.get 25
        i32.eq
        local.set 26
        i32.const 1
        local.set 27
        local.get 26
        local.get 27
        i32.and
        local.set 28
        block  ;; label = @3
          block  ;; label = @4
            local.get 28
            i32.eqz
            br_if 0 (;@4;)
            i32.const 20
            local.set 29
            i32.const 0
            local.set 30
            local.get 29
            local.get 30
            call 1
            drop
            br 1 (;@3;)
          end
          local.get 3
          i32.load offset=12
          local.set 31
          i32.const 68
          local.set 32
          local.get 31
          local.set 33
          local.get 32
          local.set 34
          local.get 33
          local.get 34
          i32.eq
          local.set 35
          i32.const 1
          local.set 36
          local.get 35
          local.get 36
          i32.and
          local.set 37
          block  ;; label = @4
            block  ;; label = @5
              local.get 37
              i32.eqz
              br_if 0 (;@5;)
              i32.const 39
              local.set 38
              i32.const 0
              local.set 39
              local.get 38
              local.get 39
              call 1
              drop
              br 1 (;@4;)
            end
            local.get 3
            i32.load offset=12
            local.set 40
            i32.const 64
            local.set 41
            local.get 40
            local.set 42
            local.get 41
            local.set 43
            local.get 42
            local.get 43
            i32.eq
            local.set 44
            i32.const 1
            local.set 45
            local.get 44
            local.get 45
            i32.and
            local.set 46
            block  ;; label = @5
              block  ;; label = @6
                local.get 46
                i32.eqz
                br_if 0 (;@6;)
                i32.const 59
                local.set 47
                i32.const 0
                local.set 48
                local.get 47
                local.get 48
                call 1
                drop
                br 1 (;@5;)
              end
              local.get 3
              i32.load offset=12
              local.set 49
              i32.const 61
              local.set 50
              local.get 49
              local.set 51
              local.get 50
              local.set 52
              local.get 51
              local.get 52
              i32.eq
              local.set 53
              i32.const 1
              local.set 54
              local.get 53
              local.get 54
              i32.and
              local.set 55
              block  ;; label = @6
                block  ;; label = @7
                  local.get 55
                  i32.eqz
                  br_if 0 (;@7;)
                  i32.const 78
                  local.set 56
                  i32.const 0
                  local.set 57
                  local.get 56
                  local.get 57
                  call 1
                  drop
                  br 1 (;@6;)
                end
                local.get 3
                i32.load offset=12
                local.set 58
                i32.const 59
                local.set 59
                local.get 58
                local.set 60
                local.get 59
                local.set 61
                local.get 60
                local.get 61
                i32.eq
                local.set 62
                i32.const 1
                local.set 63
                local.get 62
                local.get 63
                i32.and
                local.set 64
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 64
                    i32.eqz
                    br_if 0 (;@8;)
                    i32.const 101
                    local.set 65
                    i32.const 0
                    local.set 66
                    local.get 65
                    local.get 66
                    call 1
                    drop
                    br 1 (;@7;)
                  end
                  local.get 3
                  i32.load offset=12
                  local.set 67
                  i32.const 51
                  local.set 68
                  local.get 67
                  local.set 69
                  local.get 68
                  local.set 70
                  local.get 69
                  local.get 70
                  i32.eq
                  local.set 71
                  i32.const 1
                  local.set 72
                  local.get 71
                  local.get 72
                  i32.and
                  local.set 73
                  block  ;; label = @8
                    block  ;; label = @9
                      local.get 73
                      i32.eqz
                      br_if 0 (;@9;)
                      i32.const 121
                      local.set 74
                      i32.const 0
                      local.set 75
                      local.get 74
                      local.get 75
                      call 1
                      drop
                      br 1 (;@8;)
                    end
                    local.get 3
                    i32.load offset=12
                    local.set 76
                    i32.const 48
                    local.set 77
                    local.get 76
                    local.set 78
                    local.get 77
                    local.set 79
                    local.get 78
                    local.get 79
                    i32.eq
                    local.set 80
                    i32.const 1
                    local.set 81
                    local.get 80
                    local.get 81
                    i32.and
                    local.set 82
                    block  ;; label = @9
                      block  ;; label = @10
                        local.get 82
                        i32.eqz
                        br_if 0 (;@10;)
                        i32.const 141
                        local.set 83
                        i32.const 0
                        local.set 84
                        local.get 83
                        local.get 84
                        call 1
                        drop
                        br 1 (;@9;)
                      end
                      local.get 3
                      i32.load offset=12
                      local.set 85
                      i32.const 46
                      local.set 86
                      local.get 85
                      local.set 87
                      local.get 86
                      local.set 88
                      local.get 87
                      local.get 88
                      i32.eq
                      local.set 89
                      i32.const 1
                      local.set 90
                      local.get 89
                      local.get 90
                      i32.and
                      local.set 91
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 91
                          i32.eqz
                          br_if 0 (;@11;)
                          i32.const 161
                          local.set 92
                          i32.const 0
                          local.set 93
                          local.get 92
                          local.get 93
                          call 1
                          drop
                          br 1 (;@10;)
                        end
                        local.get 3
                        i32.load offset=12
                        local.set 94
                        i32.const 44
                        local.set 95
                        local.get 94
                        local.set 96
                        local.get 95
                        local.set 97
                        local.get 96
                        local.get 97
                        i32.eq
                        local.set 98
                        i32.const 1
                        local.set 99
                        local.get 98
                        local.get 99
                        i32.and
                        local.set 100
                        block  ;; label = @11
                          block  ;; label = @12
                            local.get 100
                            i32.eqz
                            br_if 0 (;@12;)
                            i32.const 181
                            local.set 101
                            i32.const 0
                            local.set 102
                            local.get 101
                            local.get 102
                            call 1
                            drop
                            br 1 (;@11;)
                          end
                          local.get 3
                          i32.load offset=12
                          local.set 103
                          i32.const 42
                          local.set 104
                          local.get 103
                          local.set 105
                          local.get 104
                          local.set 106
                          local.get 105
                          local.get 106
                          i32.eq
                          local.set 107
                          i32.const 1
                          local.set 108
                          local.get 107
                          local.get 108
                          i32.and
                          local.set 109
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 109
                              i32.eqz
                              br_if 0 (;@13;)
                              i32.const 201
                              local.set 110
                              i32.const 0
                              local.set 111
                              local.get 110
                              local.get 111
                              call 1
                              drop
                              br 1 (;@12;)
                            end
                            local.get 3
                            i32.load offset=12
                            local.set 112
                            i32.const 36
                            local.set 113
                            local.get 112
                            local.set 114
                            local.get 113
                            local.set 115
                            local.get 114
                            local.get 115
                            i32.eq
                            local.set 116
                            i32.const 1
                            local.set 117
                            local.get 116
                            local.get 117
                            i32.and
                            local.set 118
                            block  ;; label = @13
                              block  ;; label = @14
                                local.get 118
                                i32.eqz
                                br_if 0 (;@14;)
                                i32.const 222
                                local.set 119
                                i32.const 0
                                local.set 120
                                local.get 119
                                local.get 120
                                call 1
                                drop
                                br 1 (;@13;)
                              end
                              local.get 3
                              i32.load offset=12
                              local.set 121
                              i32.const 35
                              local.set 122
                              local.get 121
                              local.set 123
                              local.get 122
                              local.set 124
                              local.get 123
                              local.get 124
                              i32.eq
                              local.set 125
                              i32.const 1
                              local.set 126
                              local.get 125
                              local.get 126
                              i32.and
                              local.set 127
                              block  ;; label = @14
                                block  ;; label = @15
                                  local.get 127
                                  i32.eqz
                                  br_if 0 (;@15;)
                                  i32.const 245
                                  local.set 128
                                  i32.const 0
                                  local.set 129
                                  local.get 128
                                  local.get 129
                                  call 1
                                  drop
                                  br 1 (;@14;)
                                end
                                local.get 3
                                i32.load offset=12
                                local.set 130
                                i32.const 33
                                local.set 131
                                local.get 130
                                local.set 132
                                local.get 131
                                local.set 133
                                local.get 132
                                local.get 133
                                i32.eq
                                local.set 134
                                i32.const 1
                                local.set 135
                                local.get 134
                                local.get 135
                                i32.and
                                local.set 136
                                block  ;; label = @15
                                  block  ;; label = @16
                                    local.get 136
                                    i32.eqz
                                    br_if 0 (;@16;)
                                    i32.const 267
                                    local.set 137
                                    i32.const 0
                                    local.set 138
                                    local.get 137
                                    local.get 138
                                    call 1
                                    drop
                                    br 1 (;@15;)
                                  end
                                  local.get 3
                                  i32.load offset=12
                                  local.set 139
                                  i32.const 29
                                  local.set 140
                                  local.get 139
                                  local.set 141
                                  local.get 140
                                  local.set 142
                                  local.get 141
                                  local.get 142
                                  i32.eq
                                  local.set 143
                                  i32.const 1
                                  local.set 144
                                  local.get 143
                                  local.get 144
                                  i32.and
                                  local.set 145
                                  block  ;; label = @16
                                    block  ;; label = @17
                                      local.get 145
                                      i32.eqz
                                      br_if 0 (;@17;)
                                      i32.const 287
                                      local.set 146
                                      i32.const 0
                                      local.set 147
                                      local.get 146
                                      local.get 147
                                      call 1
                                      drop
                                      br 1 (;@16;)
                                    end
                                    local.get 3
                                    i32.load offset=12
                                    local.set 148
                                    i32.const 28
                                    local.set 149
                                    local.get 148
                                    local.set 150
                                    local.get 149
                                    local.set 151
                                    local.get 150
                                    local.get 151
                                    i32.eq
                                    local.set 152
                                    i32.const 1
                                    local.set 153
                                    local.get 152
                                    local.get 153
                                    i32.and
                                    local.set 154
                                    block  ;; label = @17
                                      block  ;; label = @18
                                        local.get 154
                                        i32.eqz
                                        br_if 0 (;@18;)
                                        i32.const 304
                                        local.set 155
                                        i32.const 0
                                        local.set 156
                                        local.get 155
                                        local.get 156
                                        call 1
                                        drop
                                        br 1 (;@17;)
                                      end
                                      local.get 3
                                      i32.load offset=12
                                      local.set 157
                                      i32.const 26
                                      local.set 158
                                      local.get 157
                                      local.set 159
                                      local.get 158
                                      local.set 160
                                      local.get 159
                                      local.get 160
                                      i32.eq
                                      local.set 161
                                      i32.const 1
                                      local.set 162
                                      local.get 161
                                      local.get 162
                                      i32.and
                                      local.set 163
                                      block  ;; label = @18
                                        block  ;; label = @19
                                          local.get 163
                                          i32.eqz
                                          br_if 0 (;@19;)
                                          i32.const 324
                                          local.set 164
                                          i32.const 0
                                          local.set 165
                                          local.get 164
                                          local.get 165
                                          call 1
                                          drop
                                          br 1 (;@18;)
                                        end
                                        local.get 3
                                        i32.load offset=12
                                        local.set 166
                                        i32.const 25
                                        local.set 167
                                        local.get 166
                                        local.set 168
                                        local.get 167
                                        local.set 169
                                        local.get 168
                                        local.get 169
                                        i32.eq
                                        local.set 170
                                        i32.const 1
                                        local.set 171
                                        local.get 170
                                        local.get 171
                                        i32.and
                                        local.set 172
                                        block  ;; label = @19
                                          block  ;; label = @20
                                            local.get 172
                                            i32.eqz
                                            br_if 0 (;@20;)
                                            i32.const 349
                                            local.set 173
                                            i32.const 0
                                            local.set 174
                                            local.get 173
                                            local.get 174
                                            call 1
                                            drop
                                            br 1 (;@19;)
                                          end
                                          local.get 3
                                          i32.load offset=12
                                          local.set 175
                                          i32.const 21
                                          local.set 176
                                          local.get 175
                                          local.set 177
                                          local.get 176
                                          local.set 178
                                          local.get 177
                                          local.get 178
                                          i32.eq
                                          local.set 179
                                          i32.const 1
                                          local.set 180
                                          local.get 179
                                          local.get 180
                                          i32.and
                                          local.set 181
                                          block  ;; label = @20
                                            block  ;; label = @21
                                              local.get 181
                                              i32.eqz
                                              br_if 0 (;@21;)
                                              i32.const 369
                                              local.set 182
                                              i32.const 0
                                              local.set 183
                                              local.get 182
                                              local.get 183
                                              call 1
                                              drop
                                              br 1 (;@20;)
                                            end
                                            local.get 3
                                            i32.load offset=12
                                            local.set 184
                                            i32.const 16
                                            local.set 185
                                            local.get 184
                                            local.set 186
                                            local.get 185
                                            local.set 187
                                            local.get 186
                                            local.get 187
                                            i32.eq
                                            local.set 188
                                            i32.const 1
                                            local.set 189
                                            local.get 188
                                            local.get 189
                                            i32.and
                                            local.set 190
                                            block  ;; label = @21
                                              block  ;; label = @22
                                                local.get 190
                                                i32.eqz
                                                br_if 0 (;@22;)
                                                i32.const 389
                                                local.set 191
                                                i32.const 0
                                                local.set 192
                                                local.get 191
                                                local.get 192
                                                call 1
                                                drop
                                                br 1 (;@21;)
                                              end
                                              local.get 3
                                              i32.load offset=12
                                              local.set 193
                                              i32.const 10
                                              local.set 194
                                              local.get 193
                                              local.set 195
                                              local.get 194
                                              local.set 196
                                              local.get 195
                                              local.get 196
                                              i32.eq
                                              local.set 197
                                              i32.const 1
                                              local.set 198
                                              local.get 197
                                              local.get 198
                                              i32.and
                                              local.set 199
                                              block  ;; label = @22
                                                block  ;; label = @23
                                                  local.get 199
                                                  i32.eqz
                                                  br_if 0 (;@23;)
                                                  i32.const 410
                                                  local.set 200
                                                  i32.const 0
                                                  local.set 201
                                                  local.get 200
                                                  local.get 201
                                                  call 1
                                                  drop
                                                  br 1 (;@22;)
                                                end
                                                local.get 3
                                                i32.load offset=12
                                                local.set 202
                                                i32.const 9
                                                local.set 203
                                                local.get 202
                                                local.set 204
                                                local.get 203
                                                local.set 205
                                                local.get 204
                                                local.get 205
                                                i32.eq
                                                local.set 206
                                                i32.const 1
                                                local.set 207
                                                local.get 206
                                                local.get 207
                                                i32.and
                                                local.set 208
                                                block  ;; label = @23
                                                  block  ;; label = @24
                                                    local.get 208
                                                    i32.eqz
                                                    br_if 0 (;@24;)
                                                    i32.const 429
                                                    local.set 209
                                                    i32.const 0
                                                    local.set 210
                                                    local.get 209
                                                    local.get 210
                                                    call 1
                                                    drop
                                                    br 1 (;@23;)
                                                  end
                                                  local.get 3
                                                  i32.load offset=12
                                                  local.set 211
                                                  i32.const 8
                                                  local.set 212
                                                  local.get 211
                                                  local.set 213
                                                  local.get 212
                                                  local.set 214
                                                  local.get 213
                                                  local.get 214
                                                  i32.eq
                                                  local.set 215
                                                  i32.const 1
                                                  local.set 216
                                                  local.get 215
                                                  local.get 216
                                                  i32.and
                                                  local.set 217
                                                  block  ;; label = @24
                                                    block  ;; label = @25
                                                      local.get 217
                                                      i32.eqz
                                                      br_if 0 (;@25;)
                                                      i32.const 450
                                                      local.set 218
                                                      i32.const 0
                                                      local.set 219
                                                      local.get 218
                                                      local.get 219
                                                      call 1
                                                      drop
                                                      br 1 (;@24;)
                                                    end
                                                    local.get 3
                                                    i32.load offset=12
                                                    local.set 220
                                                    i32.const 6
                                                    local.set 221
                                                    local.get 220
                                                    local.set 222
                                                    local.get 221
                                                    local.set 223
                                                    local.get 222
                                                    local.get 223
                                                    i32.eq
                                                    local.set 224
                                                    i32.const 1
                                                    local.set 225
                                                    local.get 224
                                                    local.get 225
                                                    i32.and
                                                    local.set 226
                                                    block  ;; label = @25
                                                      block  ;; label = @26
                                                        local.get 226
                                                        i32.eqz
                                                        br_if 0 (;@26;)
                                                        i32.const 469
                                                        local.set 227
                                                        i32.const 0
                                                        local.set 228
                                                        local.get 227
                                                        local.get 228
                                                        call 1
                                                        drop
                                                        br 1 (;@25;)
                                                      end
                                                      local.get 3
                                                      i32.load offset=12
                                                      local.set 229
                                                      i32.const 5
                                                      local.set 230
                                                      local.get 229
                                                      local.set 231
                                                      local.get 230
                                                      local.set 232
                                                      local.get 231
                                                      local.get 232
                                                      i32.eq
                                                      local.set 233
                                                      i32.const 1
                                                      local.set 234
                                                      local.get 233
                                                      local.get 234
                                                      i32.and
                                                      local.set 235
                                                      block  ;; label = @26
                                                        block  ;; label = @27
                                                          local.get 235
                                                          i32.eqz
                                                          br_if 0 (;@27;)
                                                          i32.const 489
                                                          local.set 236
                                                          i32.const 0
                                                          local.set 237
                                                          local.get 236
                                                          local.get 237
                                                          call 1
                                                          drop
                                                          br 1 (;@26;)
                                                        end
                                                        local.get 3
                                                        i32.load offset=12
                                                        local.set 238
                                                        i32.const 4
                                                        local.set 239
                                                        local.get 238
                                                        local.set 240
                                                        local.get 239
                                                        local.set 241
                                                        local.get 240
                                                        local.get 241
                                                        i32.eq
                                                        local.set 242
                                                        i32.const 1
                                                        local.set 243
                                                        local.get 242
                                                        local.get 243
                                                        i32.and
                                                        local.set 244
                                                        block  ;; label = @27
                                                          block  ;; label = @28
                                                            local.get 244
                                                            i32.eqz
                                                            br_if 0 (;@28;)
                                                            i32.const 515
                                                            local.set 245
                                                            i32.const 0
                                                            local.set 246
                                                            local.get 245
                                                            local.get 246
                                                            call 1
                                                            drop
                                                            br 1 (;@27;)
                                                          end
                                                          local.get 3
                                                          i32.load offset=12
                                                          local.set 247
                                                          i32.const 2
                                                          local.set 248
                                                          local.get 247
                                                          local.set 249
                                                          local.get 248
                                                          local.set 250
                                                          local.get 249
                                                          local.get 250
                                                          i32.eq
                                                          local.set 251
                                                          i32.const 1
                                                          local.set 252
                                                          local.get 251
                                                          local.get 252
                                                          i32.and
                                                          local.set 253
                                                          block  ;; label = @28
                                                            block  ;; label = @29
                                                              local.get 253
                                                              i32.eqz
                                                              br_if 0 (;@29;)
                                                              i32.const 0
                                                              local.set 254
                                                              i32.const 0
                                                              local.set 255
                                                              local.get 254
                                                              local.get 255
                                                              call 1
                                                              drop
                                                              br 1 (;@28;)
                                                            end
                                                            local.get 3
                                                            i32.load offset=12
                                                            local.set 256
                                                            i32.const 1
                                                            local.set 257
                                                            local.get 256
                                                            local.set 258
                                                            local.get 257
                                                            local.set 259
                                                            local.get 258
                                                            local.get 259
                                                            i32.eq
                                                            local.set 260
                                                            i32.const 1
                                                            local.set 261
                                                            local.get 260
                                                            local.get 261
                                                            i32.and
                                                            local.set 262
                                                            block  ;; label = @29
                                                              block  ;; label = @30
                                                                local.get 262
                                                                i32.eqz
                                                                br_if 0 (;@30;)
                                                                i32.const 542
                                                                local.set 263
                                                                i32.const 0
                                                                local.set 264
                                                                local.get 263
                                                                local.get 264
                                                                call 1
                                                                drop
                                                                br 1 (;@29;)
                                                              end
                                                              i32.const 561
                                                              local.set 265
                                                              i32.const 0
                                                              local.set 266
                                                              local.get 265
                                                              local.get 266
                                                              call 1
                                                              drop
                                                            end
                                                          end
                                                        end
                                                      end
                                                    end
                                                  end
                                                end
                                              end
                                            end
                                          end
                                        end
                                      end
                                    end
                                  end
                                end
                              end
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
    i32.const 16
    local.set 267
    local.get 3
    local.get 267
    i32.add
    local.set 268
    local.get 268
    global.set 0
    return)
  (func $_start (type 4)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 0
    i32.const 528
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    local.get 2
    global.set 0
    i32.const 589
    local.set 3
    i32.const 0
    local.set 4
    local.get 3
    local.get 4
    call 1
    drop
    local.get 2
    i32.load offset=524
    local.set 5
    i32.const 3
    local.set 6
    i32.const 0
    local.set 7
    i32.const 593
    local.set 8
    i32.const 1
    local.set 9
    i64.const 2105344
    local.set 10
    i64.const 0
    local.set 11
    i32.const 0
    local.set 12
    i32.const 65535
    local.set 13
    local.get 9
    local.get 13
    i32.and
    local.set 14
    i32.const 65535
    local.set 15
    local.get 12
    local.get 15
    i32.and
    local.set 16
    local.get 6
    local.get 7
    local.get 8
    local.get 14
    local.get 10
    local.get 11
    local.get 16
    local.get 5
    call 2
    local.set 17
    local.get 2
    local.get 17
    i32.store16 offset=522
    local.get 2
    i32.load offset=524
    local.set 18
    local.get 18
    i32.load
    local.set 19
    local.get 2
    local.get 19
    i32.store offset=516
    local.get 2
    i32.load16_u offset=522
    local.set 20
    i32.const 65535
    local.set 21
    local.get 20
    local.get 21
    i32.and
    local.set 22
    block  ;; label = @1
      local.get 22
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 23
      i32.const 65535
      local.set 24
      local.get 23
      local.get 24
      i32.and
      local.set 25
      local.get 25
      call $report_error
      i32.const 1
      local.set 26
      local.get 26
      call 3
      unreachable
    end
    local.get 2
    i32.load offset=524
    local.set 27
    local.get 27
    i32.load
    local.set 28
    local.get 2
    i32.load offset=512
    local.set 29
    local.get 28
    local.get 29
    call 4
    local.set 30
    local.get 2
    local.get 30
    i32.store16 offset=522
    local.get 2
    i32.load16_u offset=522
    local.set 31
    i32.const 65535
    local.set 32
    local.get 31
    local.get 32
    i32.and
    local.set 33
    block  ;; label = @1
      local.get 33
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 34
      i32.const 65535
      local.set 35
      local.get 34
      local.get 35
      i32.and
      local.set 36
      local.get 36
      call $report_error
      i32.const 2
      local.set 37
      local.get 37
      call 3
      unreachable
    end
    i32.const 606
    local.set 38
    i32.const 0
    local.set 39
    local.get 38
    local.get 39
    call 1
    drop
    local.get 2
    i32.load offset=512
    local.set 40
    local.get 40
    i64.load offset=32
    local.set 41
    local.get 41
    i32.wrap_i64
    local.set 42
    local.get 42
    call 0
    local.set 43
    local.get 2
    local.get 43
    i32.store offset=508
    local.get 2
    i32.load offset=508
    local.set 44
    i32.const 0
    local.set 45
    local.get 44
    local.set 46
    local.get 45
    local.set 47
    local.get 46
    local.get 47
    i32.eq
    local.set 48
    i32.const 1
    local.set 49
    local.get 48
    local.get 49
    i32.and
    local.set 50
    block  ;; label = @1
      local.get 50
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 51
      i32.const 65535
      local.set 52
      local.get 51
      local.get 52
      i32.and
      local.set 53
      local.get 53
      call $report_error
      i32.const 3
      local.set 54
      local.get 54
      call 3
      unreachable
    end
    i32.const 606
    local.set 55
    i32.const 0
    local.set 56
    local.get 55
    local.get 56
    call 1
    drop
    local.get 2
    i32.load offset=508
    local.set 57
    local.get 2
    local.get 57
    i32.store offset=496
    local.get 2
    i32.load offset=512
    local.set 58
    local.get 58
    i64.load offset=32
    local.set 59
    local.get 59
    i32.wrap_i64
    local.set 60
    local.get 2
    local.get 60
    i32.store offset=500
    i32.const 496
    local.set 61
    local.get 2
    local.get 61
    i32.add
    local.set 62
    local.get 62
    local.set 63
    local.get 2
    local.get 63
    i32.store offset=492
    i32.const 606
    local.set 64
    i32.const 0
    local.set 65
    local.get 64
    local.get 65
    call 1
    drop
    local.get 2
    i32.load offset=516
    local.set 66
    local.get 66
    call 5
    local.set 67
    local.get 2
    local.get 67
    i32.store16 offset=522
    local.get 2
    i32.load16_u offset=522
    local.set 68
    i32.const 65535
    local.set 69
    local.get 68
    local.get 69
    i32.and
    local.set 70
    block  ;; label = @1
      local.get 70
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 71
      i32.const 65535
      local.set 72
      local.get 71
      local.get 72
      i32.and
      local.set 73
      local.get 73
      call $report_error
      i32.const 8
      local.set 74
      local.get 74
      call 3
      unreachable
    end
    local.get 2
    i32.load offset=488
    local.set 75
    i32.const 3
    local.set 76
    i32.const 0
    local.set 77
    i32.const 593
    local.set 78
    i32.const 0
    local.set 79
    i64.const 2
    local.set 80
    i64.const 0
    local.set 81
    i32.const 65535
    local.set 82
    local.get 79
    local.get 82
    i32.and
    local.set 83
    i32.const 65535
    local.set 84
    local.get 79
    local.get 84
    i32.and
    local.set 85
    local.get 76
    local.get 77
    local.get 78
    local.get 83
    local.get 80
    local.get 81
    local.get 85
    local.get 75
    call 2
    local.set 86
    local.get 2
    local.get 86
    i32.store16 offset=522
    local.get 2
    i32.load offset=488
    local.set 87
    local.get 87
    i32.load
    local.set 88
    local.get 2
    local.get 88
    i32.store offset=484
    local.get 2
    i32.load16_u offset=522
    local.set 89
    i32.const 65535
    local.set 90
    local.get 89
    local.get 90
    i32.and
    local.set 91
    block  ;; label = @1
      local.get 91
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 92
      i32.const 65535
      local.set 93
      local.get 92
      local.get 93
      i32.and
      local.set 94
      local.get 94
      call $report_error
      i32.const 4
      local.set 95
      local.get 95
      call 3
      unreachable
    end
    i32.const 606
    local.set 96
    i32.const 0
    local.set 97
    local.get 96
    local.get 97
    call 1
    drop
    local.get 2
    i32.load offset=488
    local.set 98
    local.get 98
    i32.load
    local.set 99
    i32.const 496
    local.set 100
    local.get 2
    local.get 100
    i32.add
    local.set 101
    local.get 101
    local.set 102
    i32.const 1
    local.set 103
    i32.const 480
    local.set 104
    local.get 2
    local.get 104
    i32.add
    local.set 105
    local.get 105
    local.set 106
    local.get 99
    local.get 102
    local.get 103
    local.get 106
    call 6
    local.set 107
    local.get 2
    local.get 107
    i32.store16 offset=522
    local.get 2
    i32.load16_u offset=522
    local.set 108
    i32.const 65535
    local.set 109
    local.get 108
    local.get 109
    i32.and
    local.set 110
    block  ;; label = @1
      local.get 110
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 111
      i32.const 65535
      local.set 112
      local.get 111
      local.get 112
      i32.and
      local.set 113
      local.get 113
      call $report_error
      i32.const 4
      local.set 114
      local.get 114
      call 3
      unreachable
    end
    local.get 2
    i32.load offset=492
    local.set 115
    local.get 115
    i32.load
    local.set 116
    local.get 2
    local.get 116
    i32.store offset=476
    local.get 2
    i32.load offset=476
    local.set 117
    local.get 2
    local.get 117
    i32.store offset=472
    local.get 2
    i32.load offset=476
    local.set 118
    i32.const 4
    local.set 119
    local.get 118
    local.get 119
    i32.add
    local.set 120
    local.get 2
    local.get 120
    i32.store offset=468
    local.get 2
    i32.load offset=476
    local.set 121
    i32.const 8
    local.set 122
    local.get 121
    local.get 122
    i32.add
    local.set 123
    local.get 2
    local.get 123
    i32.store offset=464
    local.get 2
    i32.load offset=476
    local.set 124
    i32.const 12
    local.set 125
    local.get 124
    local.get 125
    i32.add
    local.set 126
    local.get 2
    local.get 126
    i32.store offset=460
    local.get 2
    i32.load offset=476
    local.set 127
    i32.const 16
    local.set 128
    local.get 127
    local.get 128
    i32.add
    local.set 129
    local.get 2
    local.get 129
    i32.store offset=456
    local.get 2
    i32.load offset=476
    local.set 130
    i32.const 20
    local.set 131
    local.get 130
    local.get 131
    i32.add
    local.set 132
    local.get 2
    local.get 132
    i32.store offset=452
    local.get 2
    i32.load offset=476
    local.set 133
    i32.const 24
    local.set 134
    local.get 133
    local.get 134
    i32.add
    local.set 135
    local.get 2
    local.get 135
    i32.store offset=448
    local.get 2
    i32.load offset=476
    local.set 136
    i32.const 28
    local.set 137
    local.get 136
    local.get 137
    i32.add
    local.set 138
    local.get 2
    local.get 138
    i32.store offset=444
    local.get 2
    i32.load offset=476
    local.set 139
    i32.const 32
    local.set 140
    local.get 139
    local.get 140
    i32.add
    local.set 141
    local.get 2
    local.get 141
    i32.store offset=440
    local.get 2
    i32.load offset=476
    local.set 142
    i32.const 36
    local.set 143
    local.get 142
    local.get 143
    i32.add
    local.set 144
    local.get 2
    local.get 144
    i32.store offset=436
    local.get 2
    i32.load offset=472
    local.set 145
    local.get 145
    i32.load
    local.set 146
    local.get 2
    local.get 146
    i32.store offset=16
    i32.const 610
    local.set 147
    i32.const 16
    local.set 148
    local.get 2
    local.get 148
    i32.add
    local.set 149
    local.get 147
    local.get 149
    call 1
    drop
    local.get 2
    i32.load offset=468
    local.set 150
    local.get 150
    i32.load
    local.set 151
    local.get 2
    local.get 151
    i32.store offset=32
    i32.const 618
    local.set 152
    i32.const 32
    local.set 153
    local.get 2
    local.get 153
    i32.add
    local.set 154
    local.get 152
    local.get 154
    call 1
    drop
    local.get 2
    i32.load offset=464
    local.set 155
    local.get 155
    i32.load
    local.set 156
    local.get 2
    local.get 156
    i32.store offset=48
    i32.const 626
    local.set 157
    i32.const 48
    local.set 158
    local.get 2
    local.get 158
    i32.add
    local.set 159
    local.get 157
    local.get 159
    call 1
    drop
    local.get 2
    i32.load offset=460
    local.set 160
    local.get 160
    i32.load
    local.set 161
    local.get 2
    local.get 161
    i32.store offset=64
    i32.const 634
    local.set 162
    i32.const 64
    local.set 163
    local.get 2
    local.get 163
    i32.add
    local.set 164
    local.get 162
    local.get 164
    call 1
    drop
    local.get 2
    i32.load offset=456
    local.set 165
    local.get 165
    i32.load
    local.set 166
    local.get 2
    local.get 166
    i32.store offset=80
    i32.const 642
    local.set 167
    i32.const 80
    local.set 168
    local.get 2
    local.get 168
    i32.add
    local.set 169
    local.get 167
    local.get 169
    call 1
    drop
    local.get 2
    i32.load offset=452
    local.set 170
    local.get 170
    i32.load
    local.set 171
    local.get 2
    local.get 171
    i32.store offset=96
    i32.const 650
    local.set 172
    i32.const 96
    local.set 173
    local.get 2
    local.get 173
    i32.add
    local.set 174
    local.get 172
    local.get 174
    call 1
    drop
    local.get 2
    i32.load offset=448
    local.set 175
    local.get 175
    i32.load
    local.set 176
    local.get 2
    local.get 176
    i32.store offset=112
    i32.const 658
    local.set 177
    i32.const 112
    local.set 178
    local.get 2
    local.get 178
    i32.add
    local.set 179
    local.get 177
    local.get 179
    call 1
    drop
    local.get 2
    i32.load offset=444
    local.set 180
    local.get 180
    i32.load
    local.set 181
    local.get 2
    local.get 181
    i32.store offset=128
    i32.const 666
    local.set 182
    i32.const 128
    local.set 183
    local.get 2
    local.get 183
    i32.add
    local.set 184
    local.get 182
    local.get 184
    call 1
    drop
    local.get 2
    i32.load offset=440
    local.set 185
    local.get 185
    i32.load
    local.set 186
    local.get 2
    local.get 186
    i32.store offset=144
    i32.const 674
    local.set 187
    i32.const 144
    local.set 188
    local.get 2
    local.get 188
    i32.add
    local.set 189
    local.get 187
    local.get 189
    call 1
    drop
    local.get 2
    i32.load offset=436
    local.set 190
    local.get 190
    i32.load
    local.set 191
    local.get 2
    local.get 191
    i32.store offset=160
    i32.const 682
    local.set 192
    i32.const 160
    local.set 193
    local.get 2
    local.get 193
    i32.add
    local.set 194
    local.get 192
    local.get 194
    call 1
    drop
    local.get 2
    i32.load offset=476
    local.set 195
    local.get 2
    local.get 195
    i32.store offset=176
    i32.const 691
    local.set 196
    i32.const 176
    local.set 197
    local.get 2
    local.get 197
    i32.add
    local.set 198
    local.get 196
    local.get 198
    call 1
    drop
    local.get 2
    i32.load offset=476
    local.set 199
    local.get 199
    call 7
    i32.const 699
    local.set 200
    i32.const 0
    local.set 201
    local.get 200
    local.get 201
    call 1
    drop
    local.get 2
    i32.load offset=476
    local.set 202
    local.get 2
    local.get 202
    i32.store offset=432
    local.get 2
    i32.load offset=476
    local.set 203
    i32.const 4
    local.set 204
    local.get 203
    local.get 204
    i32.add
    local.set 205
    local.get 2
    local.get 205
    i32.store offset=428
    local.get 2
    i32.load offset=476
    local.set 206
    i32.const 8
    local.set 207
    local.get 206
    local.get 207
    i32.add
    local.set 208
    local.get 2
    local.get 208
    i32.store offset=424
    local.get 2
    i32.load offset=476
    local.set 209
    i32.const 12
    local.set 210
    local.get 209
    local.get 210
    i32.add
    local.set 211
    local.get 2
    local.get 211
    i32.store offset=420
    local.get 2
    i32.load offset=476
    local.set 212
    i32.const 16
    local.set 213
    local.get 212
    local.get 213
    i32.add
    local.set 214
    local.get 2
    local.get 214
    i32.store offset=416
    local.get 2
    i32.load offset=476
    local.set 215
    i32.const 20
    local.set 216
    local.get 215
    local.get 216
    i32.add
    local.set 217
    local.get 2
    local.get 217
    i32.store offset=412
    local.get 2
    i32.load offset=476
    local.set 218
    i32.const 24
    local.set 219
    local.get 218
    local.get 219
    i32.add
    local.set 220
    local.get 2
    local.get 220
    i32.store offset=408
    local.get 2
    i32.load offset=476
    local.set 221
    i32.const 28
    local.set 222
    local.get 221
    local.get 222
    i32.add
    local.set 223
    local.get 2
    local.get 223
    i32.store offset=404
    local.get 2
    i32.load offset=476
    local.set 224
    i32.const 32
    local.set 225
    local.get 224
    local.get 225
    i32.add
    local.set 226
    local.get 2
    local.get 226
    i32.store offset=400
    local.get 2
    i32.load offset=476
    local.set 227
    i32.const 36
    local.set 228
    local.get 227
    local.get 228
    i32.add
    local.set 229
    local.get 2
    local.get 229
    i32.store offset=396
    local.get 2
    i32.load offset=432
    local.set 230
    local.get 230
    i32.load
    local.set 231
    local.get 2
    local.get 231
    i32.store offset=192
    i32.const 704
    local.set 232
    i32.const 192
    local.set 233
    local.get 2
    local.get 233
    i32.add
    local.set 234
    local.get 232
    local.get 234
    call 1
    drop
    local.get 2
    i32.load offset=428
    local.set 235
    local.get 235
    i32.load
    local.set 236
    local.get 2
    local.get 236
    i32.store offset=208
    i32.const 712
    local.set 237
    i32.const 208
    local.set 238
    local.get 2
    local.get 238
    i32.add
    local.set 239
    local.get 237
    local.get 239
    call 1
    drop
    local.get 2
    i32.load offset=424
    local.set 240
    local.get 240
    i32.load
    local.set 241
    local.get 2
    local.get 241
    i32.store offset=224
    i32.const 720
    local.set 242
    i32.const 224
    local.set 243
    local.get 2
    local.get 243
    i32.add
    local.set 244
    local.get 242
    local.get 244
    call 1
    drop
    local.get 2
    i32.load offset=420
    local.set 245
    local.get 245
    i32.load
    local.set 246
    local.get 2
    local.get 246
    i32.store offset=240
    i32.const 728
    local.set 247
    i32.const 240
    local.set 248
    local.get 2
    local.get 248
    i32.add
    local.set 249
    local.get 247
    local.get 249
    call 1
    drop
    local.get 2
    i32.load offset=416
    local.set 250
    local.get 250
    i32.load
    local.set 251
    local.get 2
    local.get 251
    i32.store offset=256
    i32.const 736
    local.set 252
    i32.const 256
    local.set 253
    local.get 2
    local.get 253
    i32.add
    local.set 254
    local.get 252
    local.get 254
    call 1
    drop
    local.get 2
    i32.load offset=412
    local.set 255
    local.get 255
    i32.load
    local.set 256
    local.get 2
    local.get 256
    i32.store offset=272
    i32.const 744
    local.set 257
    i32.const 272
    local.set 258
    local.get 2
    local.get 258
    i32.add
    local.set 259
    local.get 257
    local.get 259
    call 1
    drop
    local.get 2
    i32.load offset=408
    local.set 260
    local.get 260
    i32.load
    local.set 261
    local.get 2
    local.get 261
    i32.store offset=288
    i32.const 752
    local.set 262
    i32.const 288
    local.set 263
    local.get 2
    local.get 263
    i32.add
    local.set 264
    local.get 262
    local.get 264
    call 1
    drop
    local.get 2
    i32.load offset=404
    local.set 265
    local.get 265
    i32.load
    local.set 266
    local.get 2
    local.get 266
    i32.store offset=304
    i32.const 760
    local.set 267
    i32.const 304
    local.set 268
    local.get 2
    local.get 268
    i32.add
    local.set 269
    local.get 267
    local.get 269
    call 1
    drop
    local.get 2
    i32.load offset=400
    local.set 270
    local.get 270
    i32.load
    local.set 271
    local.get 2
    local.get 271
    i32.store offset=320
    i32.const 768
    local.set 272
    i32.const 320
    local.set 273
    local.get 2
    local.get 273
    i32.add
    local.set 274
    local.get 272
    local.get 274
    call 1
    drop
    local.get 2
    i32.load offset=396
    local.set 275
    local.get 275
    i32.load
    local.set 276
    local.get 2
    local.get 276
    i32.store offset=336
    i32.const 776
    local.set 277
    i32.const 336
    local.set 278
    local.get 2
    local.get 278
    i32.add
    local.set 279
    local.get 277
    local.get 279
    call 1
    drop
    local.get 2
    i32.load offset=476
    local.set 280
    local.get 2
    local.get 280
    i32.store offset=392
    local.get 2
    i32.load offset=492
    local.set 281
    local.get 281
    i32.load
    local.set 282
    local.get 2
    local.get 282
    i32.store offset=388
    i32.const 1
    local.set 283
    local.get 2
    local.get 283
    i32.store offset=376
    local.get 2
    i32.load offset=388
    local.set 284
    local.get 2
    local.get 284
    i32.store offset=380
    i32.const 3
    local.set 285
    local.get 285
    call 0
    local.set 286
    local.get 2
    local.get 286
    i32.store offset=372
    i32.const 376
    local.set 287
    local.get 2
    local.get 287
    i32.add
    local.set 288
    local.get 288
    local.set 289
    i32.const 372
    local.set 290
    local.get 2
    local.get 290
    i32.add
    local.set 291
    local.get 291
    local.set 292
    local.get 289
    local.get 292
    call 8
    local.set 293
    local.get 2
    local.get 293
    i32.store16 offset=522
    local.get 2
    i32.load16_u offset=522
    local.set 294
    i32.const 65535
    local.set 295
    local.get 294
    local.get 295
    i32.and
    local.set 296
    block  ;; label = @1
      local.get 296
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 297
      i32.const 65535
      local.set 298
      local.get 297
      local.get 298
      i32.and
      local.set 299
      local.get 299
      call $report_error
      i32.const 5
      local.set 300
      local.get 300
      call 3
      unreachable
    end
    i32.const 785
    local.set 301
    i32.const 0
    local.set 302
    local.get 301
    local.get 302
    call 1
    drop
    local.get 2
    i32.load offset=372
    local.set 303
    local.get 303
    i32.load offset=4
    local.set 304
    local.get 2
    local.get 304
    i32.store offset=368
    i32.const 789
    local.set 305
    i32.const 0
    local.set 306
    local.get 305
    local.get 306
    call 1
    drop
    local.get 2
    i32.load offset=368
    local.set 307
    i32.const 360
    local.set 308
    local.get 2
    local.get 308
    i32.add
    local.set 309
    local.get 309
    local.set 310
    local.get 307
    local.get 310
    call 9
    local.get 2
    i32.load offset=364
    local.set 311
    local.get 2
    local.get 311
    i32.store
    i32.const 793
    local.set 312
    local.get 312
    local.get 2
    call 1
    drop
    local.get 2
    i32.load offset=484
    local.set 313
    local.get 313
    call 5
    local.set 314
    local.get 2
    local.get 314
    i32.store16 offset=522
    local.get 2
    i32.load16_u offset=522
    local.set 315
    i32.const 65535
    local.set 316
    local.get 315
    local.get 316
    i32.and
    local.set 317
    block  ;; label = @1
      local.get 317
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 318
      i32.const 65535
      local.set 319
      local.get 318
      local.get 319
      i32.and
      local.set 320
      local.get 320
      call $report_error
      i32.const 8
      local.set 321
      local.get 321
      call 3
      unreachable
    end
    local.get 2
    i32.load offset=356
    local.set 322
    i32.const 3
    local.set 323
    i32.const 0
    local.set 324
    i32.const 593
    local.set 325
    i32.const 0
    local.set 326
    i64.const 64
    local.set 327
    i64.const 0
    local.set 328
    i32.const 65535
    local.set 329
    local.get 326
    local.get 329
    i32.and
    local.set 330
    i32.const 65535
    local.set 331
    local.get 326
    local.get 331
    i32.and
    local.set 332
    local.get 323
    local.get 324
    local.get 325
    local.get 330
    local.get 327
    local.get 328
    local.get 332
    local.get 322
    call 2
    local.set 333
    local.get 2
    local.get 333
    i32.store16 offset=522
    local.get 2
    i32.load offset=356
    local.set 334
    local.get 334
    i32.load
    local.set 335
    local.get 2
    local.get 335
    i32.store offset=352
    local.get 2
    i32.load16_u offset=522
    local.set 336
    i32.const 65535
    local.set 337
    local.get 336
    local.get 337
    i32.and
    local.set 338
    block  ;; label = @1
      local.get 338
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 339
      i32.const 65535
      local.set 340
      local.get 339
      local.get 340
      i32.and
      local.set 341
      local.get 341
      call $report_error
      i32.const 6
      local.set 342
      local.get 342
      call 3
      unreachable
    end
    local.get 2
    i32.load offset=356
    local.set 343
    local.get 343
    i32.load
    local.set 344
    local.get 2
    i32.load offset=348
    local.set 345
    i32.const 360
    local.set 346
    local.get 2
    local.get 346
    i32.add
    local.set 347
    local.get 347
    local.set 348
    i32.const 1
    local.set 349
    local.get 344
    local.get 348
    local.get 349
    local.get 345
    call 10
    local.set 350
    local.get 2
    local.get 350
    i32.store16 offset=522
    local.get 2
    i32.load16_u offset=522
    local.set 351
    i32.const 65535
    local.set 352
    local.get 351
    local.get 352
    i32.and
    local.set 353
    block  ;; label = @1
      local.get 353
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 354
      i32.const 65535
      local.set 355
      local.get 354
      local.get 355
      i32.and
      local.set 356
      local.get 356
      call $report_error
      i32.const 7
      local.set 357
      local.get 357
      call 3
      unreachable
    end
    local.get 2
    i32.load offset=352
    local.set 358
    local.get 358
    call 5
    local.set 359
    local.get 2
    local.get 359
    i32.store16 offset=522
    local.get 2
    i32.load16_u offset=522
    local.set 360
    i32.const 65535
    local.set 361
    local.get 360
    local.get 361
    i32.and
    local.set 362
    block  ;; label = @1
      local.get 362
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.load16_u offset=522
      local.set 363
      i32.const 65535
      local.set 364
      local.get 363
      local.get 364
      i32.and
      local.set 365
      local.get 365
      call $report_error
      i32.const 8
      local.set 366
      local.get 366
      call 3
      unreachable
    end
    local.get 2
    i32.load offset=372
    local.set 367
    local.get 367
    call 11
    local.get 2
    i32.load offset=508
    local.set 368
    local.get 368
    call 11
    i32.const 0
    local.set 369
    local.get 369
    call 3
    unreachable)
  (data $.L.str (i32.const 0) "__WASI_ERRNO_ACCES\0a\00")
  (data $.L.str.1 (i32.const 20) "__WASI_ERRNO_ROFS\0a\00")
  (data $.L.str.2 (i32.const 39) "__WASI_ERRNO_RANGE\0a\00")
  (data $.L.str.3 (i32.const 59) "__WASI_ERRNO_PIPE\0a\00")
  (data $.L.str.4 (i32.const 78) "__WASI_ERRNO_OVERFLOW\0a\00")
  (data $.L.str.5 (i32.const 101) "__WASI_ERRNO_NOTTY\0a\00")
  (data $.L.str.6 (i32.const 121) "__WASI_ERRNO_NOSPC\0a\00")
  (data $.L.str.7 (i32.const 141) "__WASI_ERRNO_NOMEM\0a\00")
  (data $.L.str.8 (i32.const 161) "__WASI_ERRNO_NOLCK\0a\00")
  (data $.L.str.9 (i32.const 181) "__WASI_ERRNO_NOENT\0a\00")
  (data $.L.str.10 (i32.const 201) "__WASI_ERRNO_NOBUFS\0a\00")
  (data $.L.str.11 (i32.const 222) "__WASI_ERRNO_MULTIHOP\0a\00")
  (data $.L.str.12 (i32.const 245) "__WASI_ERRNO_MSGSIZE\0a\00")
  (data $.L.str.13 (i32.const 267) "__WASI_ERRNO_MFILE\0a\00")
  (data $.L.str.14 (i32.const 287) "__WASI_ERRNO_IO\0a\00")
  (data $.L.str.15 (i32.const 304) "__WASI_ERRNO_INVAL\0a\00")
  (data $.L.str.16 (i32.const 324) "__WASI_ERRNO_INPROGRESS\0a\00")
  (data $.L.str.17 (i32.const 349) "__WASI_ERRNO_ILSEQ\0a\00")
  (data $.L.str.18 (i32.const 369) "__WASI_ERRNO_FAULT\0a\00")
  (data $.L.str.19 (i32.const 389) "__WASI_ERRNO_DEADLK\0a\00")
  (data $.L.str.20 (i32.const 410) "__WASI_ERRNO_BUSY\0a\00")
  (data $.L.str.21 (i32.const 429) "__WASI_ERRNO_BADMSG\0a\00")
  (data $.L.str.22 (i32.const 450) "__WASI_ERRNO_BADF\0a\00")
  (data $.L.str.23 (i32.const 469) "__WASI_ERRNO_AGAIN\0a\00")
  (data $.L.str.24 (i32.const 489) "__WASI_ERRNO_AFNOSUPPORT\0a\00")
  (data $.L.str.25 (i32.const 515) "__WASI_ERRNO_ADDRNOTAVAIL\0a\00")
  (data $.L.str.26 (i32.const 542) "__WASI_ERRNO_2BIG\0a\00")
  (data $.L.str.27 (i32.const 561) "something else: good luck.\0a\00")
  (data $.L.str.28 (i32.const 589) "a0\0a\00")
  (data $.L.str.29 (i32.const 593) "storage.byte\00")
  (data $.L.str.30 (i32.const 606) "a1\0a\00")
  (data $.L.str.31 (i32.const 610) "z1: %i\0a\00")
  (data $.L.str.32 (i32.const 618) "z2: %i\0a\00")
  (data $.L.str.33 (i32.const 626) "z3: %i\0a\00")
  (data $.L.str.34 (i32.const 634) "z4: %i\0a\00")
  (data $.L.str.35 (i32.const 642) "z5: %i\0a\00")
  (data $.L.str.36 (i32.const 650) "z6: %i\0a\00")
  (data $.L.str.37 (i32.const 658) "z7: %i\0a\00")
  (data $.L.str.38 (i32.const 666) "z8: %i\0a\00")
  (data $.L.str.39 (i32.const 674) "z9: %i\0a\00")
  (data $.L.str.40 (i32.const 682) "z10: %i\0a\00")
  (data $.L.str.41 (i32.const 691) "gs: %i\0a\00")
  (data $.L.str.42 (i32.const 699) "a3x\0a\00")
  (data $.L.str.43 (i32.const 704) "a1: %i\0a\00")
  (data $.L.str.44 (i32.const 712) "a2: %i\0a\00")
  (data $.L.str.45 (i32.const 720) "a3: %i\0a\00")
  (data $.L.str.46 (i32.const 728) "a4: %i\0a\00")
  (data $.L.str.47 (i32.const 736) "a5: %i\0a\00")
  (data $.L.str.48 (i32.const 744) "a6: %i\0a\00")
  (data $.L.str.49 (i32.const 752) "a7: %i\0a\00")
  (data $.L.str.50 (i32.const 760) "a8: %i\0a\00")
  (data $.L.str.51 (i32.const 768) "a9: %i\0a\00")
  (data $.L.str.52 (i32.const 776) "a10: %i\0a\00")
  (data $.L.str.53 (i32.const 785) "a4\0a\00")
  (data $.L.str.54 (i32.const 789) "a5\0a\00")
  (data $.L.str.55 (i32.const 793) "The size: %i \0a\00"))
