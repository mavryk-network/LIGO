import { EditorState } from '@codemirror/state';
// @ts-ignore
import HaclWasm from '@ligolang/hacl-wasm';
// @ts-ignore
import _BLS12381 from '@ligolang/ocaml-bls12-381';
// @ts-ignore
import _SECP256K1 from '@ligolang/secp256k1-wasm';
import { basicSetup } from 'codemirror';

import * as Editor from './editor';

let ligoEditor: any;
async function initialize() {
  let editor = await Editor.initialize();
  ligoEditor = editor.ligoEditor;
  // @ts-ignore
  window._BLS12381 = await _BLS12381();
  // @ts-ignore
  window._HACL = await HaclWasm.getInitializedHaclModule();
  // @ts-ignore
  window._SECP256K1 = await _SECP256K1();
}

async function loadJSBundle(path: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const script = document.createElement('script');
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

async function handleFileUpload(e: any) {
  let { files } = e.target;

  if (files.length < 1) {
    console.error('No files selected');
    return;
  }

  let { type } = files[0];

  if (type === '' || type === 'application/json') {
    ligoEditor.setState(
      EditorState.create({
        extensions: [basicSetup],
        doc: await files[0].text(),
      })
    );
  } else {
    alert('Unrecognised file type');
  }
}

function handleCompileClick() {
  // @ts-ignore
  let lexer;
  // @ts-ignore
  let syntax = document.getElementById('syntax')?.value;
  switch (syntax) {
    case 'jsligo':
      // @ts-ignore
      lexer = window.ligoJS.jsligoLexer.bind(window.ligo);
      break;
    case 'mligo':
      // @ts-ignore
      lexer = window.ligoML.cameligoLexer.bind(window.ligo);
      break;
    default:
      throw new Error('Unrecognised syntax ' + syntax);
  }
  lexer(ligoEditor.state.doc.toJSON().join('\n'));
}

async function main() {
  // @ts-ignore
  // window.ligo.compile();
  await initialize();
  console.log('All WASM dependencies loaded');
  let app, path;
  // app = "JsligoLexerJS";
  // path = "";
  // await loadJSBundle(`${path}/${app}.bc.runtime.js`);
  // await loadJSBundle(`${path}/${app}.bc.js`);
  app = 'CameligoLexerJS';
  path = '';
  await loadJSBundle(`${path}/${app}.bc.runtime.js`);
  await loadJSBundle(`${path}/${app}.bc.js`);
  document.getElementById('source-file')?.addEventListener('change', handleFileUpload);
  document.getElementById('lex')?.addEventListener('click', handleCompileClick);

  handleCompileClick();
}

main();
