let y_mismo = (b : bool) => switch (b, b) {
  | (true, true) => true
  | (true, false) => false
  | (false, true) => false
  | (false, false) => false
};
