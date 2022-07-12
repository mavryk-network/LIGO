use std::fmt::Write;
use std::fs::OpenOptions;

use proc_macro::TokenStream;
use syn::Fields::*;
use syn::{
  AngleBracketedGenericArguments, FieldsNamed, FieldsUnnamed, GenericArgument, GenericParam, Item,
  ItemEnum, ItemStruct, PathArguments, TypeParam,
};

struct StructData {
  name:        String,
  type_params: Vec<String>,
  fields:      Vec<(String, String, bool)>,
}

struct EnumData {
  name:        String,
  type_params: Vec<String>,
  variants:    Vec<(String, Vec<String>)>,
}

enum ItemData {
  Struct(StructData),
  Enum(EnumData),
}

static mut DATATYPES: Vec<ItemData> = Vec::new();

fn ty_to_string(t: &syn::Type) -> (String, bool) {
  match t {
    syn::Type::Path(syn::TypePath {
      path: syn::Path { segments, .. },
      ..
    }) => {
      let (v, no_inline): (Vec<String>, bool) =
        segments
          .iter()
          .fold((Vec::new(), false), |acc, s| -> (Vec<String>, bool) {
            let (mut v, mut no_inline) = acc;
            let mut r_args = String::new();
            match &s.arguments {
              PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                for ga in args.iter() {
                  match &ga {
                    GenericArgument::Type(t) => {
                      let (x, no_inline_t) = ty_to_string(t);
                      let x = x + " ";
                      no_inline = no_inline || no_inline_t;
                      r_args.push_str(x.as_str())
                    }
                    _ => (),
                  }
                }
              }
              _ => (),
            };

            let s2 = if s.ident == "i32" {
              "int32".to_string()
            } else {
              s.ident.to_string()
            };
            v.push(r_args + s2.as_str());
            return (v, no_inline);
          });
      let result = v.join(".");

      return (result, no_inline);
    }
    syn::Type::Ptr(syn::TypePtr { elem, .. }) => {
      let (v, _) = ty_to_string(elem);
      (v, true) // should not be inlined
    }
    _ => panic!("Type not supported yet"),
  }
}

pub fn datatype_helper(input: TokenStream) -> TokenStream {
  let token_copy = input.clone();
  let input = syn::parse_macro_input!(input as Item);
  match input {
    Item::Struct(ItemStruct {
      ident,
      fields,
      generics,
      ..
    }) => {
      let type_params: Vec<String> = generics.params.iter().fold(vec![], |mut acc, param| -> _ {
        match param {
          GenericParam::Type(TypeParam { ident, .. }) => {
            acc.push(ident.to_string());
            acc
          }
          _ => acc,
        }
      });
      let name = ident.to_string();
      match fields {
        Named(FieldsNamed { named, .. }) => {
          let fields = named
            .iter()
            .map(|field| -> (String, String, bool) {
              let (ty, no_inline) = ty_to_string(&field.ty);
              (field.ident.as_ref().unwrap().to_string(), ty, no_inline)
            })
            .collect();
          unsafe {
            DATATYPES.push(ItemData::Struct(StructData {
              name,
              type_params,
              fields,
            }))
          }
        }
        _ => panic!("Not supported fields kind"),
      }
    }
    Item::Enum(ItemEnum {
      ident,
      variants,
      generics,
      ..
    }) => {
      let type_params: Vec<String> = generics.params.iter().fold(vec![], |mut acc, param| -> _ {
        match param {
          GenericParam::Type(TypeParam { ident, .. }) => {
            acc.push(ident.to_string());
            acc
          }
          _ => acc,
        }
      });
      let ident = &ident;
      let name = ident.to_string();
      let variants: Vec<(String, Vec<String>)> = variants
        .iter()
        .map(|f| -> (String, Vec<String>) {
          match &f.fields {
            Unnamed(FieldsUnnamed { unnamed, .. }) => {
              let args: Vec<String> = unnamed
                .iter()
                .map(|f| -> String { ty_to_string(&f.ty).0 })
                .collect();
              return (f.ident.to_string(), args);
            }
            Named(FieldsNamed { named, .. }) => {
              let args: Vec<String> = named
                .iter()
                .map(|f| -> String { ty_to_string(&f.ty).0 })
                .collect();
              return (f.ident.to_string(), args);
            }
            Unit => return (f.ident.to_string(), vec![]),
          }
        })
        .collect();
      unsafe {
        DATATYPES.push(ItemData::Enum(EnumData {
          name,
          type_params,
          variants,
        }))
      }
    }
    _ => {
      panic!("Not supported yet...")
    }
  };
  return token_copy;
}

pub fn produce_datatype_file(input: TokenStream) -> TokenStream {
  let d: &Vec<ItemData> = unsafe { &DATATYPES };
  let mut datatype_file = String::new();
  write!(
    &mut datatype_file,
    "
[@@@warning \"-27-26\"]

(* Do not change, this file is generated by the Rust `expose_datatype` macro. *)
module W = WasmObjectFile  
module A = W.Ast
module T = W.Types
module S = W.Source
module ValueVar = Stage_common.Types.ValueVar
    
let at = S.no_region

let const n = S.{{ it = A.Const {{ it = I32 n; at}}; at }}

let var_to_string name =  
  let name, hash = ValueVar.internal_get_name_and_counter name in
  name ^ \"#\" ^ (string_of_int hash)

type locals = (string * T.value_type) list

"
  )
  .unwrap();

  // write the types
  for item in d.iter() {
    match item {
      ItemData::Struct(StructData {
        name,
        type_params,
        fields,
      }) => {
        let type_record_fields: String = fields.iter().fold(
          String::new(),
          |acc: String, param: &(String, String, bool)| -> String {
            let mut type_record_fields = acc;
            let type_ = param.1.as_str().to_string();
            let type_exists = type_params.iter().any(|x| *x == type_);
            let first_char: char = type_.chars().nth(0).unwrap();
            let type_record_field;
            if type_exists {
              type_record_field = format!("{} : '{}", param.0.as_str(), type_.to_lowercase());
            } else if first_char.is_uppercase() {
              type_record_field = format!("{}: {};", param.0.as_str(), type_.to_lowercase());
            } else {
              type_record_field = format!("{}: {};", param.0.as_str(), "int32");
            }
            type_record_fields.push_str(type_record_field.as_str());
            return type_record_fields;
          },
        );

        let mut type_params_ = String::new();
        if type_params.len() > 0 {
          let type_params: Vec<String> = type_params.iter().map(|x| "'".to_string() + x).collect();
          type_params_ = type_params.join(" ").to_lowercase();
        };

        write!(
          &mut datatype_file,
          "
and {2} {0} = {{
  {1}
}}
",
          name.to_lowercase(),
          type_record_fields,
          type_params_
        )
        .unwrap();
      }
      ItemData::Enum(EnumData {
        name,
        variants,
        type_params,
      }) => {
        let mut counter = 0;
        let type_params_exists = type_params.is_empty() == false;
        let variants_ = variants.iter().fold(String::new(), |acc, param| -> _ {
          let mut variants_ = acc;

          if param.1.len() > 0 {
            let variant_args: Vec<String> = param
              .1
              .iter()
              .map(|x| -> String {
                (if type_params_exists {
                  "'".to_string()
                } else {
                  "".to_string()
                }) + x.to_lowercase().as_str()
              })
              .collect();

            variants_.push_str(format!("\n| {} of {}", param.0, variant_args.join(" * ")).as_str());
          } else if param.0 == "Instructions" {
            variants_.push_str("\n| Instructions of A.instr list")
          } else {
            variants_.push_str(format!("\n| {}", param.0).as_str())
          }
          counter = counter + 1;
          return variants_;
        });

        let mut class_params = "".to_string();
        if type_params_exists {
          let class_params2: Vec<String> = type_params
            .iter()
            .map(|x| "'".to_string() + x.to_string().to_lowercase().as_str())
            .collect();
          let class_params2: String = class_params2.join(";");
          class_params = "".to_string() + class_params2.as_str() + " ";
        };

        write!(
          &mut datatype_file,
          "
and {2}{0} = {1}
",
          name.to_lowercase(),
          variants_,
          class_params,
        )
        .unwrap();
      }
    }
  }

  // write the sizes of datatypes
  write!(
    &mut datatype_file,
    "
  let rec int32_size () = 4l
  and bool_size () = 4l
  and t_size () = int32_size ()
  and usize_size () = int32_size ()
  "
  )
  .unwrap();
  for item in d.iter() {
    match item {
      ItemData::Struct(StructData {
        name,
        type_params: _,
        fields,
      }) => {
        let calculation = fields.iter().fold(String::new(), |mut acc, field| {
          let (n, type_name, no_inline) = field;
          if *no_inline {
            acc.push_str("+ 4l");
          } else {
            let type_name_orig = type_name.split(" ");
            let type_name_vec: Vec<&str> = type_name_orig.map(|x| x).collect();
            let type_name_orig = type_name_vec.iter().nth_back(0);
            acc.push_str(format!("+ {0}_size ()", type_name_orig.unwrap().to_lowercase()).as_str())
          }
          acc
        });
        // let calculation = fields.len() * 4;

        write!(
          &mut datatype_file,
          "and {0}_size () = Int32.(0l {1})\n",
          name.to_lowercase(),
          calculation
        )
        .unwrap()
      }
      ItemData::Enum(EnumData {
        name,
        variants,
        type_params,
      }) => {
        let a = variants.iter().fold(String::new(), |mut acc, v| -> String {
          let (n, x) = v;

          let type_name_orig = x.join(" ");
          if type_name_orig != "" {
            let type_name_orig = type_name_orig.split(" ");
            let type_name_vec: Vec<&str> = type_name_orig.map(|x| x).collect();
            let type_name_orig = type_name_vec.iter().nth_back(0);

            // println!("check: {0} ({1})", n, x.join(" "));

            acc.push_str(
              (" ".to_string() + type_name_orig.unwrap().to_lowercase().as_str() + "_size (); ")
                .as_str(),
            );
          }
          acc
        });

        write!(
          &mut datatype_file,
          "and {0}_size () = Int32.(4l + (List.fold_left ~f:max ~init:0l [{1}]))\n",
          name.to_lowercase(),
          a
        )
        .unwrap()
      }
    }
  }

  // write the functions
  write!(
    &mut datatype_file,
    "
let rec malloc: name:string -> size:int32 -> string * A.instr list = fun ~name ~size ->
    let malloc_local = var_to_string (ValueVar.fresh ~name ()) in
    (malloc_local, S.[
      {{ it = A.Const {{ it = I32 size; at}}; at }};
      {{ it = Call \"malloc\"; at }};
      {{ it = LocalSet malloc_local; at }};
    ])

and alloc_value: alloc:string -> A.instr list = fun ~alloc ->
  S.[
    {{ it = A.LocalGet alloc; at }}
  ]

and load: alloc:string -> offset:int32 -> string -> locals * A.instr list = 
  fun ~alloc ~offset name ->
    let name_l = var_to_string (ValueVar.fresh ~name ()) in
    [name_l, T.I32Type], alloc_value ~alloc 
    @
    (if Int32.(offset = 0l) then 
    []
    else (
      S.[{{ it = A.Const {{ it = I32 offset; at}}; at }};
      {{ it = Binary (I32 Add); at }}]
    ))
    @
    S.[
      {{ it = Load {{ty = I32Type; align = 0; offset = 0l; sz = None}}; at }};
      {{ it = LocalSet name_l; at }}
    ]

and store: alloc:string -> offset:int32 -> locals * A.instr list -> locals * A.instr list = fun ~alloc ~offset value ->
    let locals, value = value in
    locals,
    alloc_value ~alloc 
    @
    (if Int32.(offset = 0l) then 
    []
    else (
      S.[{{ it = A.Const {{ it = I32 offset; at}}; at }};
      {{ it = Binary (I32 Add); at }}]
    ))
    @
    value 
    @ 
    S.[
      {{ it = Store {{ty = I32Type; align = 0; offset = 0l; sz = None}}; at }};
    ] 
    
and store_int32: ?alloc:string -> ?offset:int32 -> int32 -> locals * A.instr list = fun ?alloc ?offset n ->
  match alloc, offset with 
    Some alloc, Some offset  -> store ~alloc ~offset ([], S.[{{ it = A.Const {{ it = I32 n; at }}; at }}])
  | _ -> [], S.[{{ it = A.Const {{ it = I32 n; at }}; at }}]
  

and store_u8: ?alloc:string -> ?offset:int32 -> int32 -> locals * A.instr list = fun ?alloc ?offset n -> store_int32 ?alloc ?offset n

and store_usize: ?alloc:string -> ?offset:int32 -> int32 -> locals * A.instr list = fun ?alloc ?offset n -> store_int32 ?alloc ?offset n

and store_bool: ?alloc:string -> ?offset:int32 -> bool -> locals * A.instr list = fun ?alloc ?offset b ->
  if b then 
    store_int32 ?alloc ?offset 1l 
  else 
    store_int32 ?alloc ?offset 0l

and store_string : ?alloc:string -> ?offset:int32 -> string -> locals * A.instr list = 
  fun ?alloc ?offset s -> failwith \"todo\"

")
  .unwrap();

  for item in d.iter() {
    match item {
      ItemData::Struct(StructData {
        name,
        type_params,
        fields,
      }) => {
        let type_params_exists = type_params.is_empty() == false;
        let (ty_params, ty_params2): (Vec<String>, Vec<String>) = type_params.iter().fold(
          (vec![], vec![]),
          |acc, param| -> (Vec<String>, Vec<String>) {
            let (mut ty_params, mut ty_params2) = acc;
            let type_param_lowercase = param.to_string().to_lowercase();
            ty_params.push("'".to_string() + type_param_lowercase.as_str());
            ty_params2.push(type_param_lowercase);
            (ty_params, ty_params2)
          },
        );

        let ty_p = ty_params.join(" ").to_string();
        let ty = if type_params_exists {
          "".to_string() + ty_p.as_str() + " " + name.to_string().to_lowercase().as_str()
        } else {
          name.to_string().to_lowercase()
        };
        let ty_p2 = ty_params2.join(" ").to_string();
        let mut counter = 0;
        let (store_fields, load_fields, locals, instructions) = fields.iter().fold(
          (String::new(), String::new(), String::new(), String::new()),
          |acc, field| -> (String, String, String, String) {
            let (store_fields, load_fields, locals, instructions) = acc;
            let field = &field;
            let field_type = field.1.split(" ");
            let field_type_vec: Vec<&str> = field_type.map(|x| x).collect();
            let not_inline = field.2;
            let top_type_name = field_type_vec.iter().nth_back(0).unwrap();
            let type_names_tail: Vec<&&str> = field_type_vec
              .iter()
              .take(field_type_vec.len() - 1)
              .collect();
            let additional_calls: String = type_names_tail
              .iter()
              .fold(String::new(), |acc, x| -> String {
                format!("(fun ?alloc ?offset a -> 
                  let l, i = store_{0} ?alloc ?offset a {1} in 
                  l, i)", x.to_lowercase(), acc 
                )
                });
            let result =
                if not_inline {
                  store_fields
                  + format!(
                    "let locals_{0}, {0}_instructions = store ~alloc ~offset:Int32.(offset + {1}l) (store_{2} value.{0} {3}) in \n",
                    field.0,
                    counter,
                    top_type_name.to_lowercase(),
                    additional_calls,
                  )
                  .as_str()
              } else {
                store_fields
                  + format!(
                    "let locals_{0}, {0}_instructions = store_{2} ~alloc ~offset:Int32.(offset + {1}l) value.{0} {3} in \n",
                    field.0,
                    counter,
                    top_type_name.to_lowercase(),
                    additional_calls,
                  )
                  .as_str()
              };
            let locals = locals + format!("locals_{0} @", field.0).as_str();
            let instructions = instructions + format!("{0}_instructions @", field.0).as_str();
            let load_fields = load_fields + format!("let locals_{0}, {0}_instructions = load ~alloc ~offset:{1}l \"{0}\" in ", field.0, counter).as_str();
            counter = counter + 4;
            (result, load_fields, locals, instructions)
          },
        );
        write!(
          &mut datatype_file,
          "
            
and load_{0}: alloc:string -> locals * A.instr list = fun ~alloc ->
  {8}
  let locals = {6} [] in
  let instructions = {7} [] in
  locals, instructions

and store_{0}: {3} ?alloc:string -> ?offset:int32 -> {1} -> {2} locals * A.instr list = fun ?alloc ?offset value {5} -> 
  let offset = match offset with 
    Some offset -> offset 
  | None -> 0l
  in
  let alloc, alloc_instructions, return_ = match alloc with 
    Some alloc -> alloc, [], []
  | None -> let alloc, i = malloc ~name:\"malloc_{0}\" ~size:({0}_size ()) in alloc, i, alloc_value ~alloc
  in
  {4}
  (alloc, T.I32Type) :: {6} [],
  alloc_instructions @
  {7}
  return_
        ",
          name.to_lowercase(), // 0
          ty, // 1
          if type_params_exists { // 2
            format!("(?alloc:string -> ?offset:int32 -> '{0} -> locals * A.instr list) ->", ty_p2)
          } else {
            "".to_string()
          },
          if type_params_exists { // 3
            "'".to_string() + ty_p2.as_str() + "."
          } else {
            "".to_string()
          },
          store_fields, // 4
          if type_params_exists { // 5
            "store_".to_string() + ty_p2.as_str()
          } else {
            "".to_string()
          },
          locals, // 6
          instructions, // 7
          load_fields // 8
        )
        .unwrap()
      }
      ItemData::Enum(EnumData {
        name,
        variants,
        type_params,
      }) => {
        let type_params_exists = type_params.is_empty() == false;
        let (ty_params, ty_params2): (Vec<String>, Vec<String>) = type_params.iter().fold(
          (vec![], vec![]),
          |acc, param| -> (Vec<String>, Vec<String>) {
            let (mut ty_params, mut ty_params2) = acc;
            let type_param_lowercase = param.to_string().to_lowercase();
            ty_params.push("'".to_string() + type_param_lowercase.as_str());
            ty_params2.push(type_param_lowercase);
            (ty_params, ty_params2)
          },
        );
        let ty = if type_params_exists {
          ty_params.join(" ").to_string() + " " + name.to_string().to_lowercase().as_str()
        } else {
          name.to_string().to_lowercase()
        };
        let ty_p2 = ty_params2.join(" ").to_string();
        let mut index = 0;

        let mut choices = String::new();
        let mut counter = 0;
        for _ in variants.iter() {
          choices.push_str(format!("{{it = {0}l; at}}; ", counter).as_str());
          counter = counter + 1
        }

        let (variants_, load_blocks) = variants
          .iter()
          .fold((String::new(), String::new()), |acc, param| -> (String, String) {
            let (mut acc, block) = acc;

            // sanity hack
            if param.0 == "Instructions" {
              acc.push_str("| Instructions e ->  [], e");
              return (acc, block)
            }

            let p = &param.1;
            let type_name_orig = p.join("");
            let type_name_orig = type_name_orig.split(" ");
            let type_name_vec:Vec<&str> = type_name_orig.map(|x| x).collect();
            let type_name_orig = type_name_vec.iter().nth_back(0);
            let type_names_tail:Vec<&&str> =  type_name_vec.iter().take(type_name_vec.len() - 1).collect();
            let additional_calls:String = type_names_tail.iter().fold(String::new(), |acc, x| -> String {
              format!("(fun ?alloc ?offset a -> let l, i = store_{0} ?alloc ?offset a {1} in l, i )", x.to_lowercase(), acc)              
            });
            let type_name_orig = type_name_orig.unwrap().to_lowercase();
            let mut type_size_name = "0l".to_string();
            if &type_name_orig != "" {
              type_size_name = String::new() + type_name_orig.as_str() + "_size ()"
            }
            let args = if param.1.len() > 0 {
              let one_char_arg: char = param.1.join("").chars().nth(0).unwrap();
              one_char_arg.to_string().to_lowercase()
            } else {
              "".to_string()
            };
            let store_value =
              if param.1.len() > 0 {
                format!("let value_locals, value_instructions = store_{0} ~alloc ~offset:Int32.(offset + 4l) {1} {2} in", type_name_orig, args, additional_calls)
              } else {
                "let value_locals, value_instructions = [], [] in".to_string()
              };
            acc.push_str(format!("| {0} {1}-> 
              let alloc, alloc_l, alloc_instructions, return_l = match alloc with 
                Some alloc -> alloc, [], [], []
              | None -> let a, i = malloc ~name:\"{2}_{0}_malloc\" ~size:Int32.(4l + {3}) in a, [(a, T.I32Type)], i, alloc_value ~alloc:a
              in  
              let header_locals, header_instructions = store_int32 ~alloc ~offset {4}l in
              {5}
              alloc_l @ header_locals @ value_locals,
              alloc_instructions @
              header_instructions @
              value_instructions @ 
              return_l
              ", 
              param.0, args, name, type_size_name, index, store_value).as_str());
            let block = block + format!(
              "and load_{0}_{1}: symbol:string -> locals * A.instr list = fun ~symbol ->
                [], []
              ", 
              name.to_lowercase(), param.0.to_lowercase()).as_str();
            index = index + 1;
            (acc, block)
    });

        write!(
          &mut datatype_file,
          "
{6}
        
and store_{0}: {5} ?alloc:string -> ?offset:int32 -> {1} -> {4} locals * A.instr list = fun ?alloc ?offset value {3} -> 
  let offset = match offset with 
    Some offset -> offset 
  | None -> 0l
  in
  match value with 
  {2}
          ",
          name.to_lowercase(), // 0
          ty, // 1
          variants_, // 2
          if type_params_exists { // 3
            "store_".to_string() + ty_p2.as_str()
          } else {
            "".to_string()
          },
          if type_params_exists { // 4
            format!("(?alloc:string -> ?offset:int32 -> '{0} -> locals * A.instr list) ->", ty_p2)
          } else {
            "".to_string()
          },
          if type_params_exists { // 5
            format!("'{0}.", ty_p2)
          } else {
            "".to_string()
          },
          load_blocks
        )
        .unwrap()
      }
    }
  }

  let output = OpenOptions::new()
    .create(true)
    .write(true)
    .truncate(true)
    .open("../../src/passes/14-wasm/mem_helpers.ml");

  match output {
    Ok(mut f) => {
      use std::io::Write;
      write!(f, "{}", datatype_file).unwrap();
    }
    Err(..) => panic!(),
  };

  return input;
}
