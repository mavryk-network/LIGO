function incr_map (const l : list (int)) : list (int) is
  List.map (function (const i : int) : int is i + 1, l)