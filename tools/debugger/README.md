# Debugger

A debugger for LIGO contracts for VSCode IDE.

It consists of two parts:

* Haskell backend in [`ligo-debugger`](./ligo-debugger) folder;
* VSCode extension in [`vscode-plugin`](./vscode-plugin) folder.

## How to build

To build the plugin, run `make package`; this will add `.vsix` file to `vscode-plugin` folder.
You can then use this file to install the extension from VSCode interface:

To build and install the plugin at once, run `make install-plugin`.
If this is not the first time you install the plugin, you may need to reload VSCode manually.
