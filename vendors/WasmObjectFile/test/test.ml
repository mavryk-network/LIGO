let () = 
  let m = Ast.{
    types = [];
    globals = [];
    tables = [];
    memories = []:
    funcs = [];
    start = None;
    elems = [];
    data  = [];
    imports = [];
    exports = [];
    symbols = []
  }
  in
  Encode.encode m