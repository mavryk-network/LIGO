import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";

export function initialize() {
  return new EditorView({
    state: EditorState.create({
      extensions: [basicSetup, javascript()],
      doc: ``,
    }),
    parent: document.getElementById("michelson"),
  });
}
