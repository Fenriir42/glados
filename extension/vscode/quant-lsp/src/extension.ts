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
  // Register unconditionally so clicking a code lens never gets "command not found",
  // even if the server binary is missing or the client hasn't started yet.
  context.subscriptions.push(
    vscode.commands.registerCommand(
      "quant-lsp.runFile",
      (filePath: string) => {
        const terminal =
          vscode.window.terminals.find((t) => t.name === "Quant") ??
          vscode.window.createTerminal("Quant");
        terminal.show(true);
        terminal.sendText(`glados compiler "${filePath}"`);
      },
    ),
  );

  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory(
      "quant",
      new QuantDebugAdapterDescriptorFactory(),
    ),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(
      "quant-lsp.showReferences",
      (
        uriStr: string,
        position: { line: number; character: number },
        locations: {
          uri: string;
          range: {
            start: { line: number; character: number };
            end: { line: number; character: number };
          };
        }[],
      ) => {
        const uri = vscode.Uri.parse(uriStr);
        const pos = new vscode.Position(position.line, position.character);
        const locs = locations.map(
          (loc) =>
            new vscode.Location(
              vscode.Uri.parse(loc.uri),
              new vscode.Range(
                new vscode.Position(
                  loc.range.start.line,
                  loc.range.start.character,
                ),
                new vscode.Position(
                  loc.range.end.line,
                  loc.range.end.character,
                ),
              ),
            ),
        );
        void vscode.commands.executeCommand(
          "editor.action.showReferences",
          uri,
          pos,
          locs,
        );
      },
    ),
  );

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

  const statusBar = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Left,
    100,
  );
  statusBar.text = "$(sync~spin) Quant: indexing…";
  context.subscriptions.push(statusBar);

  client.onNotification(
    "$/quant/indexingStatus",
    (params: { indexing: boolean }) => {
      if (params.indexing) {
        statusBar.show();
      } else {
        statusBar.hide();
      }
    },
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

class QuantDebugAdapterDescriptorFactory
  implements vscode.DebugAdapterDescriptorFactory
{
  createDebugAdapterDescriptor(
    _session: vscode.DebugSession,
    _executable: vscode.DebugAdapterExecutable | undefined,
  ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
    const config = vscode.workspace.getConfiguration("quant-lsp");
    const configured = config.get<string>("debugAdapterPath", "");
    const adapterPath = resolveAdapterPath(configured);
    if (!adapterPath) {
      void vscode.window
        .showWarningMessage(
          "Quant: quant-dap binary not found. Build it with `cabal install dap-server` or set quant-lsp.debugAdapterPath.",
          "Open Settings",
        )
        .then((action) => {
          if (action === "Open Settings") {
            void vscode.commands.executeCommand(
              "workbench.action.openSettings",
              "quant-lsp.debugAdapterPath",
            );
          }
        });
      return undefined;
    }
    const session = _session.configuration as {
      stdlib?: string;
    };
    const args = session.stdlib ? ["--stdlib", session.stdlib] : [];
    return new vscode.DebugAdapterExecutable(adapterPath, args);
  }
}

function resolveAdapterPath(configured: string): string | undefined {
  if (configured && path.isAbsolute(configured)) {
    return fs.existsSync(configured) ? configured : undefined;
  }
  const name = configured || "quant-dap";
  return findOnPath(name);
}
