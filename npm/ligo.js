import _BLS12381 from "@ligolang/ocaml-bls12-381";
import HaclWasm from "@ligolang/hacl-wasm";
import _SECP256K1 from "@ligolang/secp256k1-wasm";

async function initialize() {
  window._BLS12381 = await _BLS12381();
  window._HACL = await HaclWasm.getInitializedHaclModule();
  window._SECP256K1 = await _SECP256K1();
}

async function loadJSBundle(url) {
  return new Promise((resolve) => {
    // TODO(prometheansacrifice) Handle error while loading JS
    const script = document.createElement("script");
    script.src = url.pathname;
    script.onload = function () {
      resolve();
    };
    document.head.appendChild(script);
  });
}

export async function compile(code, syntax) {
  await initialize();
  console.log("All WASM dependencies loaded");
  await loadJSBundle(new URL("js_main.bc.runtime.js", import.meta.url));
  await loadJSBundle(new URL("js_main.bc.js", import.meta.url));
  return window.compile.main(code, syntax);
}

// Cant use TS because rollup expects an es6 module at the consumer end. If necessary, also distribute an es6 export
