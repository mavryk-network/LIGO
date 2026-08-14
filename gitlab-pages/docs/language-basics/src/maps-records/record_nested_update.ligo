type color is Blue of unit | Green of unit

type preferences is record [
  color : color;
  other : int
]

type account is record [
  id          : int;
  preferences : preferences
]
function change_color_preference (const account : account; const color : color) : account is
  account with record [preferences.color = color]