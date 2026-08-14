[@entry]
function emitEvents (const _u : unit; const storage : int) : list (operation) * int is
  block {
    const event1 : operation = Mavryk.Next.Operation.emit ("%emitEvents", "hi");
    const event2 : operation = Mavryk.Next.Operation.emit ("%emitEvents", 6);
  } with (list [event1; event2], storage)