type t = 
  Solved of Token.t list
| Inject of Token.t list
| Suggestion of (Token.t option -> Token.t list)