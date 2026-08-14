module Counter is {
  type storage_type is int
  type return_type is list (operation) * storage_type

  [@entry]
  function main (const _action : unit; const storage : storage_type) : return_type is
    ((nil : list (operation)), storage + 1)
}