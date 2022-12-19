import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";
import _BLS12381 from "@prometheansacrifice/ocaml-bls12-381";
import HaclWasm from "@prometheansacrifice/hacl-wasm";
import _SECP256K1 from "@prometheansacrifice/secp256k1-wasm";
import * as Editor from "./editor";

async function initialize() {
  window._BLS12381 = await _BLS12381();
  window._HACL = await HaclWasm.getInitializedHaclModule();
  window._SECP256K1 = await _SECP256K1();
}

async function loadJSBundle(path) {
  return new Promise((resolve) => {
    // TODO(prometheansacrifice) Handle error while loading JS
    const script = document.createElement("script");
    script.src = path;
    script.onload = function () {
      resolve();
    };
    document.head.appendChild(script);
  });
}

let { ligoEditor, michelsonEditor } = Editor.initialize();
initialize().then(async () => {
  console.log("All WASM dependencies loaded");
  await loadJSBundle("/js_main.bc.runtime.js");
  await loadJSBundle("/js_main.bc.js");
  document.getElementById("compile").addEventListener("click", function () {
    let michelson = compile.main(
      ligoEditor.state.doc.toJSON().join("\n"),
      document.getElementById("syntax").value
    );
    console.log(michelson);
    michelsonEditor.setState(
      EditorState.create({
        extensions: [basicSetup],
        doc: michelson,
      })
    );
  });
});
