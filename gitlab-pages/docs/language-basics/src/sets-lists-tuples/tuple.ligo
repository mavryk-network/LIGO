type two_people is string * string  // Alias

const friends : two_people = ("Alice", "Johnson") // Optional parentheses
const (person_a, person_b) : two_people = friends
function first_person (const (person_a, _) : two_people) : string is person_a
const alice : string = first_person (friends)
const first_name : string = friends.0