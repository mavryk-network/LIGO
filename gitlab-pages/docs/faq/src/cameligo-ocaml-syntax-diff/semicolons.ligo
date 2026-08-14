type storage is int

[@entry]
function main (const _p : unit; const s : storage) : list (operation) * storage is
  begin
    assert (1 = 1);
    assert (2 = 2); // a trailing semicolon here is fine
  end with ((nil : list (operation)), s)