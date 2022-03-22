// Test functions with several parameters in PascaLIGO

function ab (const (a, b) : int * int) : int is a+b

function abcd (const (a, b, c, d) : int * int * int * int) : int is a+b+c+d+2

function abcde_curried (const _a : int;
                        const _b : int;
                        const c : int;
                        const _d : int;
                        const e : int) : int is c+e+3

function abcde (const (a, b, c, d, e) : int * int * int * int * int) : int is abcde_curried(a, b, c, d, e)
