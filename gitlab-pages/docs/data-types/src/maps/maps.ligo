type word       is string
type definition is list (string)
type dictionary is map (word, definition)

const empty_dict : dictionary = Map.empty

const dictionary : dictionary =
  Map.literal (list [
    ("one", list ["The number 1."; "A member of a group."]);
    ("two", list ["The number 2"])])