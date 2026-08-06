const n : unit = unit
function m (const x : int) : int is
  begin
    assert (x > 0);
    assert (x < 10)
  end with x