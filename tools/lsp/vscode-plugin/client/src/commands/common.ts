import * as vscode from 'vscode'
import { extname, basename } from 'path';
import { execFileSync } from 'child_process';

export const ligoOutput = vscode.window.createOutputChannel('LIGO Compiler')
let lastContractPath;

type BinaryInfo = {
  name: string,
  path: string,
}

function extToDialect(ext : string) {
  switch (ext) {
    case '.ligo': return 'pascaligo'
    case '.mligo': return 'cameligo'
    case '.religo': return 'reasonligo'
    default:
      console.error('Unknown dialect');
      return undefined
  }
}

export function getBinaryPath(info: BinaryInfo, config: vscode.WorkspaceConfiguration) {
  let binaryPath = config.get<string>(info.path)
  if (binaryPath) {
    return binaryPath
  }

  try {
    vscode.window.showWarningMessage(`'${info.name}' binary not found through the configuration for the Visual Studio Code extension. Using PATH.`)

    binaryPath = execFileSync('which', [info.name]).toString().trim()

    vscode.window.showWarningMessage(`${info.path} variable was updated to ${binaryPath}`)
    config.update(info.path, binaryPath)
    return binaryPath
  } catch {
    vscode.window.showWarningMessage(`'${info.name}' binary not found in PATH. You won't be able to compile and deploy contracts
                                      without providing path to ${info.name} using ${info.path} variable,
                                      located in VSCode settings`)

    return undefined
  }
}

/* eslint-disable no-shadow */
/* eslint-disable no-unused-vars */
export enum CommandRequiredArguments {
  NoArgs,
  Path,
  FileName,
  PathAndExt
}

export type ContractFileData = {
  path: string,
  ext: string,
}

export function getLastContractPath() {
  let path = vscode.window.activeTextEditor.document.uri.fsPath;
  const ext = extname(path);
  if (ext !== '.ligo' && ext !== '.mligo' && ext !== '.religo') {
    if (!lastContractPath) {
      return undefined;
    }
    path = lastContractPath;
  }

  lastContractPath = path;
  return { path, ext }
}

export async function executeCommand(
  binary: BinaryInfo,
  command,
  commandArgs = CommandRequiredArguments.Path,
  showOutput = true,
  errorPrefix = undefined,
) {
  const contractInfo = getLastContractPath()
  const ligoPath = getBinaryPath({ name: 'ligo', path: 'ligoLanguageServer.ligoBinaryPath' }, vscode.workspace.getConfiguration());

  if (!ligoPath || ligoPath === '') {
    vscode.window.showWarningMessage('LIGO executable not found. Aborting ...');
    return undefined;
  }

  let finalCommand;
  switch (commandArgs) {
    case CommandRequiredArguments.NoArgs:
      finalCommand = command();
      break;
    case CommandRequiredArguments.Path:
      finalCommand = command(contractInfo.path);
      break;
    case CommandRequiredArguments.FileName:
      finalCommand = command(contractInfo.path, command(basename(contractInfo.path).split('.')[0]));
      break;
    case CommandRequiredArguments.PathAndExt:
      finalCommand = command(contractInfo.path, extToDialect(extname(contractInfo.path)));
      break;
    default:
      console.error('Unknown command')
      return undefined;
  }
  try {
    const result = execFileSync(ligoPath, finalCommand)
    if (showOutput) {
      ligoOutput.appendLine(result)
      ligoOutput.show();
    }
    return result;
  } catch (error) {
    if (errorPrefix) {
      ligoOutput.appendLine(errorPrefix)
    }
    ligoOutput.appendLine(error.message);
    ligoOutput.show();
  }
  return undefined
}
