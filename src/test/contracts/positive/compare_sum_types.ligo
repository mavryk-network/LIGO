type foo is Foo | Bar

function main (const (p, _s) : foo * bool) : list(operation) * bool is
  ((nil : list (operation)), p = Foo)