import { EditorState } from "@codemirror/state";
import { EditorView, basicSetup } from "codemirror";
import { javascript } from "@codemirror/lang-javascript";
import { compile } from "ligolang";
import * as Editor from "./editor";

let { ligoEditor, michelsonEditor } = Editor.initialize();

document.getElementById("compile").addEventListener("click", async function () {
  try {
    let michelson = await compile(
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
  } catch (e) {
    console.log(e);
  }
});
