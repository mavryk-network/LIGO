Serialization format
===
list             = a, b, b, c, c, d, until 0
tuple            = list of pointers to other serializable items 
set              = {left; item; depth; right}
or (contructors) = one pointer to another serializable item
map              = (a, b)

serialize_item
  - list works 
  - tuple works
  - set UGH -> `{left; item; depth; right}`
    

 process_item item   
    process_item left
    process_item right

loop
   change item pointer;
   change pointer to left;
   change pointer to right;

   