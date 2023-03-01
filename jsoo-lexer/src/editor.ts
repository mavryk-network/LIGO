import { javascript } from '@codemirror/lang-javascript';
import { EditorState } from '@codemirror/state';
import { EditorView, basicSetup } from 'codemirror';

import { cameligo as fetchDefaultCameligoEx } from './default-code';

export async function initialize() {
  let ligoEditor = new EditorView({
    state: EditorState.create({
      extensions: [basicSetup, javascript()],
      doc: await fetchDefaultCameligoEx(),
    }),
    parent: document.getElementById('ligo') || document.body,
  });
  return { ligoEditor };
}
