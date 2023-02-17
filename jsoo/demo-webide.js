import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";
import _BLS12381 from "@prometheansacrifice/ocaml-bls12-381";
import HaclWasm from "@prometheansacrifice/hacl-wasm";
import _SECP256K1 from "@prometheansacrifice/secp256k1-wasm";
import * as Editor from "./editor";

async function validateFileUpload(e) {
  let { files } = e.target;

  if (files.length < 1) {
    console.error("No files selected");
    return false;
  }

  let { type } = files[0];

  if (type === "" || type === "application/json") {
    return true;
  } else {
    alert("Unrecognised file type");
    return false;
  }
}

let incrementalOps = [];

function updateProgress(completed, total) {
  document.getElementById("progress").innerHTML = `${completed}/${total}`;
}

let stopIncreCompilation = false;
async function handleIncrementalOpsFileUpload(e) {
  let compileFn = ocaml.compileIncremental.bind(window.ocaml);
  if (!validateFileUpload(e)) {
    return;
  }

  let { files } = e.target;
  console.log("Got files", files);
  function indexFromFilename(a) {
    return parseInt(a.match(/ops-([0-9]+).minified.json/)[1]);
  }

  files = Array.from(files).sort((x, y) => {
    return indexFromFilename(x.name) - indexFromFilename(y.name);
  });

  let fileIndex = +(localStorage.getItem("fileIndex") || 0);
  let opIndex = +(localStorage.getItem("opIndex") || 0);
  for (let i = fileIndex; i < files.length; i++) {
    console.log("Receiving file", i + 1);
    console.log(files[i]);
    let json = await files[i].text();
    let jsonLength = json.length;
    console.log("Parsing");
    let jsonEntries = JSON.parse(json);
    for (let jsonI = opIndex; jsonI < jsonEntries.length; ++jsonI) {
      let jsonEntry = jsonEntries[jsonI];
      if (jsonI % 1000 === 0) {
        console.log("compiling in OCaml layer", i, jsonI);
      }
      compileFn(jsonEntry, i, jsonI);
      updateProgress(jsonI, jsonLength);

      if (stopIncreCompilation) {
        break;
      }
    }
    if (stopIncreCompilation) {
      break;
    }
    console.log("done");
  }
  console.log(ocaml.toString().call());
}

function JSONToOps(json) {
  // @ts-ignore
  let queue = [{ path: "", json: json }];
  function keyToPath(parent, key) {
    return typeof key === "number" ? `${parent}.[${key}]` : `${parent}.${key}`;
  }
  let ops = [];
  while (queue.length > 0) {
    let entry = queue.shift();
    if (!entry) {
      throw new Error(
        "Weird that queue length wasn't 0, but shift() returned undefined"
      );
    }
    let { path, json } = entry;
    if (typeof json === "object" && json instanceof Array) {
      // Ideally, it could have been just,
      // ops.push({ op: "SET", path, v: [] });
      // But, in the OCaml layer, we need to appropriate initialise
      // the array with enough space. So, we pass down the size of
      // the array too.
      ops.push({ op: "SET", path, v: { type: "array", length: json.length } });
      // This can be optimised. Hence, keeping the logic duplicated.
      Object.keys(json).forEach(function (key) {
        // @ts-ignore
        let value = json[key];
        let childPath = keyToPath(path, +key);
        queue.push({ path: childPath, json: value });
      });
    } else if (typeof json === "object" && json === null) {
      // Noop
    } else if (typeof json === "object") {
      // Ideally, it could have been just,
      // ops.push({ op: "SET", path, v: {} });
      // But, in the OCaml layer, we need to appropriate initialise
      // the hashtbl with enough space. So, we pass down the size of
      // the object in terms of number of key-value pairs too.
      ops.push({
        op: "SET",
        path,
        v: { type: "object", length: Object.keys(json).length },
      });
      Object.keys(json).forEach(function (key) {
        // @ts-ignore
        let value = json[key];
        let childPath = keyToPath(path, key);
        queue.push({ path: childPath, json: value });
      });
    } else if (typeof json === "number" || typeof json === "string") {
      if (path === "") {
        throw new Error("Cant have root json as number or string");
      }
      ops.push({ op: "SET", path, v: json });
    } else {
    }
  }
  return ops;
}

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
  let compileFn, michelson;
  console.log("compiling");
  let ir = getIR();
  switch (ir) {
    case "cst":
      compileFn = compile.loadCst.bind(compile);
      michelson = compileFn(
        ligoEditor.state.doc.toJSON().join("\n"),
        getSyntax()
      );
      michelsonEditor.setState(
        EditorState.create({
          extensions: [basicSetup],
          doc: michelson,
        })
      );
      break;
    case "ast-typed":
      compileFn = compile.loadAstTyped.bind(compile);
      let code = ligoEditor.state.doc.toJSON().join("\n");
      michelson = compileFn(code, getSyntax());
      michelsonEditor.setState(
        EditorState.create({
          extensions: [basicSetup],
          doc: michelson,
        })
      );
      break;
    case "ast-typed-json-ops":
      compileFn = compile.loadAstTypedOps.bind(compile);
      michelson = compileFn(incrementalOps, getSyntax());
      michelsonEditor.setState(
        EditorState.create({
          extensions: [basicSetup],
          doc: michelson,
        })
      );
      break;
    default:
      compileFn = compile.main.bind(compile);
      michelson = compileFn(
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
  document
    .getElementById("json-ops-file")
    .addEventListener("change", handleIncrementalOpsFileUpload);
  // test area. Put stuff you'd like to run immediately on browser reload. Useful for quicker dev cycle
  // handleCompileClick(window.compile);
});
