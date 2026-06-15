import * as path from "path";
import * as fs from "fs";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext): void {
  const config = vscode.workspace.getConfiguration("quant-lsp");

  if (!config.get<boolean>("enable", true)) {
    return;
  }

  const serverPath = resolveServerPath(config.get<string>("serverPath", ""));
  if (!serverPath) {
    vscode.window
      .showWarningMessage(
        "Quant: glados-lsp binary not found. Build it with `cabal install lsp-server` or set quant-lsp.serverPath.",
        "Open Settings",
      )
      .then((action) => {
        if (action === "Open Settings") {
          void vscode.commands.executeCommand(
            "workbench.action.openSettings",
            "quant-lsp.serverPath",
          );
        }
      });
    return;
  }

  const workspaceRoot =
    vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? process.cwd();

  const serverOptions: ServerOptions = {
    command: serverPath,
    transport: TransportKind.stdio,
    options: { cwd: workspaceRoot },
  };

  const traceLevel = config.get<string>("trace.server", "off");

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "quant" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.{qa,quant}"),
    },
    outputChannelName: "Quant Language Server",
    traceOutputChannel: vscode.window.createOutputChannel(
      "Quant LSP Trace",
      "log",
    ),
    initializationOptions: {
      trace: traceLevel,
    },
  };

  client = new LanguageClient(
    "quant-lsp",
    "Quant Language Server",
    serverOptions,
    clientOptions,
  );

  client.start();
  context.subscriptions.push(client);
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

function resolveServerPath(configured: string): string | undefined {
  if (configured && path.isAbsolute(configured)) {
    return fs.existsSync(configured) ? configured : undefined;
  }

  const name = configured || "glados-lsp";
  return findOnPath(name);
}

function findOnPath(name: string): string | undefined {
  const dirs = (process.env.PATH ?? "").split(path.delimiter);
  for (const dir of dirs) {
    const candidate = path.join(dir, name);
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      return candidate;
    } catch {
      // not found or not executable, try next
    }
  }
  return undefined;
}
