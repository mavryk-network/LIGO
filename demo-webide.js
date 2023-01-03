import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";
import _BLS12381 from "@prometheansacrifice/ocaml-bls12-381";
import HaclWasm from "@prometheansacrifice/hacl-wasm";
import * as Editor from "./editor";

async function initialize() {
  let BLS12381 = await _BLS12381();
  window._BLS12381 = BLS12381;
  let loaded = await HaclWasm.getInitializedHaclModule();
  window._HACL = loaded;
}

initialize().then(() => {
  console.log("All WASM dependencies loaded");
  let editor = Editor.initialize();
  document.getElementById("compile").addEventListener("click", function () {
    let michelson = compile.main(ligoEditor.state.doc.toJSON().join("\n"));
    console.log(michelson);
    editor.setState(
      EditorState.create({
        extensions: [basicSetup],
        doc: michelson,
      })
    );
  });
});
