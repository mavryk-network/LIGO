import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";

let ligoEditor = new EditorView({
  state: EditorState.create({
    extensions: [basicSetup, javascript()],
    doc: `

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

`,
  }),
  parent: document.getElementById("ligo"),
});

let michelsonEditor = new EditorView({
  state: EditorState.create({
    extensions: [basicSetup, javascript()],
    doc: `
`,
  }),
  parent: document.getElementById("michelson"),
});

document.getElementById("compile").addEventListener("click", function () {
  let michelson = compile.main(ligoEditor.state.doc.toJSON().join("\n"));
  console.log(michelson);
  michelsonEditor.setState(
    EditorState.create({
      extensions: [basicSetup],
      doc: michelson,
    })
  );
  // console.log(compile.main());
});
