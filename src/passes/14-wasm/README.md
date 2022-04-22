Roadmap:
[x] Get `vendors/WasmObjectFile` working properly.
[x] Create wasm version of gmp
[x] Lift functions as WASM does not support nested functions


[x] Link with GMP
    - Use LLVM's LLD to link with external code, like GMP.
[x] Produce something VERY simple
[ ] 0.1
    [ ] Support basic data types:
        [x] int (GMP)
        [x] tuple 
        [x] list
        [ ] set
            - red black tree implementation
        [ ] map
    [ ] support basic arithmetic operations (what's supported out of the box by GMP):
        [x] addition
        [x] subtraction
        [ ] multiplication
        [ ] ediv ?
        [ ] bitwise operations


 
Later:
[ ] support partial functions
[ ] inlining things