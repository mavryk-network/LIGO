  type storage is string

  [@entry]
  function store_hello (const delta : int; const store : storage) : list (operation) * storage is
    ((nil : list (operation)), "Hello")