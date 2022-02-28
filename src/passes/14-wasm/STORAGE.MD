Storage
===
This document describes how storage is serialized for LIGO applications in WebAssembly.

The data structures will roughly have the following layout:

GMP value
    [header with 3 entries, contents of value]

List
    [pointer to value, pointer to next list item]
    [pointer to value, pointer to next list item]
    [pointer to value, null to indicate end of list]
Set
    Size of set
    [
        entry -> pointer
    ]
Map
    Size of map.
    [
        [entry, pointer],
        [entry, pointer],
        [entry, pointer]
    ]
BigMap
    - Same as map?!
Tuple
    - Same as list.

Constructor
    - list of contructors
    - [pointer to contructor, pointer to value]

When storing data structures, pointers should be changed to an easy to deserialize format. Ideally something as offset + pointer in serialized format. 

Can we generate code to loop over the storage data structure?