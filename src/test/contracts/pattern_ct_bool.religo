let y_mismo = (b : bool) => switch (b, b) {
  | (true, true) => true
  | (true, false) => true
  | (false, true) => true
  | (false, false) => false
};
