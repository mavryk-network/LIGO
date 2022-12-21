open Js_of_ocaml
module Api = Ligo_api
module Default_options = Compiler_options.Default_options
module Raw_options = Compiler_options.Raw_options

let source =
  {|
type storage = int;

type parameter =
| ["Increment", int]
| ["Decrement", int]
| ["Reset"];

/* Two entrypoints */

const add = (store: storage, delta: int) => store + delta;
const sub = (store: storage, delta: int) => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */

const main = (action: parameter, store: storage) : [ list<operation> , storage ] => {
 return [
   list([]),    // No operations
   (match (action, {
    Increment: n => add (store, n),
    Decrement: n => sub (store, n),
    Reset:     ()  => 0}))
  ]
};

/* Tests for main access point */

const test_initial_storage = (() => {
  let initial_storage = 42;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  return assert(Test.get_storage(taddr) == initial_storage)
}) ();

const test_increment = (() => {
  let initial_storage = 42;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (1)), 1 as mutez);
  return assert(Test.get_storage(taddr) == initial_storage + 1);
}) ();

|}


let main source =
  let entry_point = "main" in
  let views = Default_options.views in
  let syntax = "cameligo" in
  let protocol_version = "lima" in
  let display_format = Simple_utils.Display.human_readable in
  let michelson_format = `Text in
  let michelson_comments = [ `Source ] in
  let raw_options =
    Raw_options.make
      ~entry_point
      ~syntax
      ~views
      ~protocol_version
      ~disable_michelson_typechecking: false
      ~experimental_disable_optimizations_for_debugging: false
      ~enable_typed_opt: false
      ~no_stdlib: false
      ~warning_as_error: false
      ~no_colour: true
      ~constants: Default_options.constants
      ~file_constants: None
      ~project_root: None
      ~warn_unused_rec: true
      ()
  in
  match
    Api.Compile.contract raw_options
      (Api.Compile.Text (source)) 
      display_format
      michelson_format
      michelson_comments
      ()
  with
  | Ok (a, b) ->
    print_endline a;
    print_endline b;
    a
  | Error (a, b) ->
    print_endline "error";
    print_endline a;
    print_endline b;
    "<failed>"


let _ =
  Js.export
    "compile"
    (object%js
       method main str = Js.to_string str |> main |> Js.string
    end)


let () = print_endline @@ main source
