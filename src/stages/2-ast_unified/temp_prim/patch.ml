type 'expr t =
  { collection : 'expr
  ; patch_kind : [ `Map | `Record | `Set ]
  ; patch : 'expr
  }
[@@deriving yojson, map]
