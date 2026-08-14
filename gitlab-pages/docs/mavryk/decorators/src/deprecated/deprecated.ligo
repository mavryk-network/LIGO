[@inline] [@deprecated "Use `List.tail` instead."]
function tail_opt<elt> (const lst : list (elt)) : option (list (elt)) is
  List.tail (lst)