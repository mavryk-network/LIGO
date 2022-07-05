type 'a_x_option x_option = X_some of 'a_x_option | X_none
type 'a_x_list x_list = Fake_non_rec of 'a_x_list | Fake_empty (* can't write rec type in ligo *)

let x_empty (type a) : a x_list = Fake_empty

let a : (int x_list) x_option = X_some (x_empty)
