type 'a field_assign = {
  name : string;
  expr : 'a; 
}

and 'a t = 'a field_assign list
[@@deriving yojson]