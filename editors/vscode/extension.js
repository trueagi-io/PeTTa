// Minimal VS Code extension that registers the PeTTa DAP server (dap.sh) as a
// debug adapter of type "petta". Plain JavaScript -- no build step or npm
// install is required; VS Code provides the `vscode` module at runtime.

const vscode = require('vscode');
const path = require('path');
const fs = require('fs');

function findRepoRoot() {
  // This file lives at <repo>/editors/vscode/extension.js, so the repo root is
  // two directories up. Fall back to the first workspace folder containing dap.sh.
  const guess = path.resolve(__dirname, '..', '..');
  if (fs.existsSync(path.join(guess, 'dap.sh'))) {
    return guess;
  }
  for (const folder of vscode.workspace.workspaceFolders || []) {
    const root = folder.uri.fsPath;
    if (fs.existsSync(path.join(root, 'dap.sh'))) {
      return root;
    }
  }
  return guess;
}

function activate(context) {
  const factory = {
    createDebugAdapterDescriptor() {
      const dap = path.join(findRepoRoot(), 'dap.sh');
      // Launch the adapter; it speaks DAP over stdio.
      return new vscode.DebugAdapterExecutable('sh', [dap]);
    }
  };
  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory('petta', factory)
  );
}

function deactivate() {}

module.exports = { activate, deactivate };
