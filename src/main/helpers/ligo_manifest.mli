type t =
  { name : string
  ; version : string
  ; description : string
  ; scripts : (string * string) list
  ; main : string option
  ; author : string
  ; type_ : string
  ; storage_fn : string option
  ; storage_arg : string option
  ; repository : Repository_url.t
  ; license : string
  ; readme : string
  ; ligo_manifest_path : string
  }
[@@deriving to_yojson]

val validate : t -> (t, string) result
val read : project_root:string option -> (t, string) result
