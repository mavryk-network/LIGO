const friends = ("Alice", "Bob")
const (alice, bob) = friends
const (alice, _) = friends
const (alice, _bob) = friends // This alice shadows the previous one
const deep = (1, (2n, "Hello"))
const (_x, (_y, greeting)) = deep // greeting = "Hello"
const film = deep.1.1 ^ ", Dolly!" // film = "Hello, Dolly!"