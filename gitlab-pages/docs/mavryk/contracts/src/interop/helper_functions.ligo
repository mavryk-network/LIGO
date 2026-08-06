type z_to_v is
  Z of unit
| Y of unit
| X of unit
| W of unit
| V of unit

type w_or_v is michelson_or (unit, "w", unit, "v")
type x_or is michelson_or (unit, "x", w_or_v, "other")
type y_or is michelson_or (unit, "y", x_or, "other")
type z_or is michelson_or (unit, "z", y_or, "other")

type test is record [
  z : string;
  y : int;
  x : string;
  w : bool;
  v : int
]

function make_concrete_sum (const r : z_to_v) : z_or is
  case r of [
    Z (_u) -> (M_left (unit) : z_or)
  | Y (_u) -> (M_right (M_left (unit)) : z_or)
  | X (_u) -> (M_right (M_right (M_left (unit))) : z_or)
  | W (_u) -> (M_right (M_right (M_right (M_left (unit)))) : z_or)
  | V (_u) -> (M_right (M_right (M_right (M_right (unit)))) : z_or)
  ]

function make_concrete_record (const r : test) : string * int * string * bool * int is
  (r.z, r.y, r.x, r.w, r.v)

function make_abstract_sum (const zv : z_or) : z_to_v is
  case zv of [
    M_left (_n) -> Z (unit)
  | M_right (yv) ->
      case yv of [
        M_left (_n) -> Y (unit)
      | M_right (xv) ->
          case xv of [
            M_left (_n) -> X (unit)
          | M_right (wv) ->
              case wv of [
                M_left (_n) -> W (unit)
              | M_right (_n) -> V (unit)
              ]
          ]
      ]
  ]

function make_abstract_record (const z : string; const y : int; const x : string; const w : bool; const v : int) : test is
  record [ z = z; y = y; x = x; w = w; v = v ]