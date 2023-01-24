import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";
import { ml, js } from "./example-code";

export function initialize() {
  let ligoEditor = new EditorView({
    state: EditorState.create({
      extensions: [basicSetup, javascript()],
      doc: ml,
    }),
    parent: document.getElementById("ligo"),
  });
  let michelsonEditor = new EditorView({
    state: EditorState.create({
      extensions: [basicSetup, javascript()],
      doc: ` `,
    }),
    parent: document.getElementById("michelson"),
  });
  return { ligoEditor, michelsonEditor };
}
