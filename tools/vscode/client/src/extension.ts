/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import { join } from 'path';
import { platform } from 'process';

import { LspExtension } from './lsp/LspExtension';
import { LigoContext } from './common/LigoContext';
import { LigoProtocolClient } from './common/LigoProtocolClient';

import { DebuggerExtension } from './debugger/DebuggerExtension';
import LigoServer from './debugger/LigoServer';
import { getCurrentWorkspacePath } from './debugger/base';
import { trackLigoPathChanges } from './common/config';

/**
 * Activates the `ligo-vscode` extension, initializing both LIGO Language Server
 * and LIGO Debugger.
 */
export async function activate(context: vscode.ExtensionContext) {
  // MAVRYK: PascaLIGO. Instrumented + made the debugger setup non-fatal so a missing
  // debugger adapter (M6, not built) can never prevent the LSP from starting.
  console.log('[MavrykLigo] activate() start');
  const ligoContext = new LigoContext(context);

  const adapterPath = join(context.extensionPath, 'bin', `ligo-debugger${platform === 'win32' ? '.exe' : ''}`);

  context.subscriptions.push(
    trackLigoPathChanges()
  )

  const server = new LigoServer(getCurrentWorkspacePath()?.fsPath, adapterPath, []);
  const client = new LigoProtocolClient(server.address());
  context.subscriptions.push(server)

  try {
    console.log('[MavrykLigo] starting LspExtension...');
    context.subscriptions.push(new LspExtension(ligoContext, client));
    console.log('[MavrykLigo] LspExtension started OK');
  } catch (e) {
    console.error('[MavrykLigo] LspExtension FAILED:', e);
    vscode.window.showErrorMessage(
      'MavrykLigo: language server failed to start: ' + ((e as Error)?.stack ?? e)
    );
  }

  try {
    context.subscriptions.push(new DebuggerExtension(ligoContext, server, client));
  } catch (e) {
    // Debugger is M6 (unbuilt); never let it break the LSP.
    console.error('[MavrykLigo] DebuggerExtension failed (ignored):', e);
  }
  console.log('[MavrykLigo] activate() end');
}

