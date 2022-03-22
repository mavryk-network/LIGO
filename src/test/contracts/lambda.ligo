function f (const _ : unit) : unit is Unit

function main (const (_, _) : unit * unit) : unit is f (Unit)
