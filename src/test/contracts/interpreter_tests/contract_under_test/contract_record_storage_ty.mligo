type storage = { foo : int ; bar : string}
type return = operation list * storage
type parameter = One | Two

let main (_action, store : parameter * storage) : return =
   ([] : operation list), {store with foo = store.foo + 1}