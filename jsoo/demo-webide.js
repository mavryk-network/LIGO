import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";
import _BLS12381 from "@prometheansacrifice/ocaml-bls12-381";
import HaclWasm from "@prometheansacrifice/hacl-wasm";
import _SECP256K1 from "@prometheansacrifice/secp256k1-wasm";
import * as Editor from "./editor";

function getSyntax() {
  return document.getElementById("syntax").value;
}

function getIR() {
  return document.getElementById("ir").value;
}

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
    script.onerror = function (e) {
      reject(e);
    };
    document.head.appendChild(script);
  });
}

let { ligoEditor, michelsonEditor } = Editor.initialize();
function handleCompileClick(compile) {
  let compileFn;
  let ir = getIR();
  switch (ir) {
    case "cst":
      compileFn = compile.loadCst.bind(compile);
      break;
    default:
      compileFn = compile.main.bind(compile);
  }
  let michelson = compileFn(
    ligoEditor.state.doc.toJSON().join("\n"),
    getSyntax()
  );
  michelsonEditor.setState(
    EditorState.create({
      extensions: [basicSetup],
      doc: michelson,
    })
  );
}

function handlePrintCSTClick(compile) {
  let syntax = getSyntax();
  let cst = compile.print(ligoEditor.state.doc.toJSON().join("\n"));
  michelsonEditor.setState(
    EditorState.create({
      extensions: [basicSetup],
      doc: cst,
    })
  );
}

async function handleFileUpload(e) {
  let { files } = e.target;

  if (files.length < 1) {
    console.error("No files selected");
    return;
  }

  let { type } = files[0];

  if (type === "" || type === "application/json") {
    ligoEditor.setState(
      EditorState.create({
        extensions: [basicSetup],
        doc: await files[0].text(),
      })
    );
  } else {
    alert("Unrecognised file type");
  }
}

initialize().then(async () => {
  console.log("All WASM dependencies loaded");
  await loadJSBundle("/js_main.bc.runtime.js");
  await loadJSBundle("/js_main.bc.js");
  document
    .getElementById("compile")
    .addEventListener("click", () => handleCompileClick(window.compile));
  document
    .getElementById("print-cst")
    .addEventListener("change", () => handlePrintCSTClick(window.compile));
  document
    .getElementById("source-file")
    .addEventListener("change", handleFileUpload);

  // test area. Put stuff you'd like to run immediately on browser reload. Useful for quicker dev cycle
  handleCompileClick(window.compile);
});
