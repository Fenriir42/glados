import * as path from "path";
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

  const serverOptions: ServerOptions = {
    command: serverPath,
    transport: TransportKind.stdio,
  };

  const traceLevel = config.get<string>("trace.server", "off");

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "quant" }],
    synchronize: {
      fileEvents:
        vscode.workspace.createFileSystemWatcher("**/*.{qa,quant}"),
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

function resolveServerPath(configured: string): string {
  if (configured && path.isAbsolute(configured)) {
    return configured;
  }
  // Fall back to looking up 'glados-lsp' in PATH
  return "glados-lsp";
}
