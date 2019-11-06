;;; Thunk test module.

(module
 (import "runtime" "resolve" (func $resolve (param i32) (result i32)))
 (import "runtime" "table" (table 1 funcref))
 (import "runtime" "memory" (memory 0))

 ;; Simple thunk: Returns immediate integer constant 1
 (func $simple_thunk (param i32) (result i32)
       i32.const 1
       i32.const 2
       i32.shl

       i32.const 0x1
       i32.or)

 ;; Thunk with closure: Returns one plus the integer value in the
 ;; closure.
 (func $closure_thunk (param $c i32) (result i32)
       ;; Load closure variable
       local.get $c
       i32.load

       ;; Add 1
       i32.const 1
       i32.add

       ;; Encode as immediate integer
       i32.const 2
       i32.shl
       i32.const 0x1
       i32.or)

 ;; Thunk which returns another thunk (the simple thunk)
 (func $chained_thunk (param i32) (result i32)
       i32.const 4)

 (elem (i32.const 1) $simple_thunk $closure_thunk $chained_thunk)

 ;; Create a simple thunk at address 4
 (data (i32.const 4) "\00\00\00\00\01\00\00\00\00\00\00\00"))
