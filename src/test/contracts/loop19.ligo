// Test while loops in PascaLIGO
function nested_loops (const x:int) : int is block {
  var _x := x;
  var r  := 0;
  while _x > 0 block {
    var _y := _x;
    while _y > 0 block {
      r := r + 1;
      _y := _y - 1;
    };
    _x := _x - 1;
  };
} with r;
