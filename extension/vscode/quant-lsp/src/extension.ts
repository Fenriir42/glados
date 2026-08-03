import * as path from "path";
import * as fs from "fs";
import { spawn } from "child_process";
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

  registerTestController(context);

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
        "Quant: quant-lsp binary not found. Build it with `make install` or set quant-lsp.serverPath.",
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

  const name = configured || "quant-lsp";
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

// ---------------------------------------------------------------------------
// Test Explorer integration

// Map: workspace folder fsPath → root TestItem for that project
const projectItems = new Map<string, vscode.TestItem>();

function registerTestController(context: vscode.ExtensionContext): void {
  const ctrl = vscode.tests.createTestController("quant", "Quant");
  context.subscriptions.push(ctrl);

  ctrl.resolveHandler = async (item) => {
    if (!item) await discoverAllTests(ctrl);
  };

  const watcher = vscode.workspace.createFileSystemWatcher("**/*_test.qa");
  context.subscriptions.push(watcher);
  watcher.onDidCreate((uri) => discoverTestsInFile(ctrl, uri));
  watcher.onDidChange((uri) => discoverTestsInFile(ctrl, uri));
  watcher.onDidDelete((uri) => removeTestFile(ctrl, uri));

  ctrl.createRunProfile(
    "Run",
    vscode.TestRunProfileKind.Run,
    (request, token) => { void runQuantTests(ctrl, request, token, false); },
    true,
  );

  const covProfile = ctrl.createRunProfile(
    "Run with Coverage",
    vscode.TestRunProfileKind.Coverage,
    (request, token) => { void runQuantTests(ctrl, request, token, true); },
    true,
  );
  covProfile.loadDetailedCoverage = loadDetailedCoverage;
}

// Find the workspace folder that contains uri, returning its fsPath.
function workspaceOf(uri: vscode.Uri): string | undefined {
  return vscode.workspace.getWorkspaceFolder(uri)?.uri.fsPath;
}

// Return (or create) the root TestItem for the given workspace folder.
function getOrCreateProjectItem(
  ctrl: vscode.TestController,
  wsRoot: string,
): vscode.TestItem {
  const existing = projectItems.get(wsRoot);
  if (existing) return existing;
  const wsUri = vscode.Uri.file(wsRoot);
  const label = path.basename(wsRoot);
  const item = ctrl.createTestItem(`project:${wsRoot}`, label, wsUri);
  item.canResolveChildren = true;
  ctrl.items.add(item);
  projectItems.set(wsRoot, item);
  return item;
}

async function discoverAllTests(ctrl: vscode.TestController): Promise<void> {
  const uris = await vscode.workspace.findFiles(
    "**/*_test.qa",
    "**/node_modules/**",
  );
  await Promise.all(uris.map((u) => discoverTestsInFile(ctrl, u)));
}

async function discoverTestsInFile(
  ctrl: vscode.TestController,
  uri: vscode.Uri,
): Promise<void> {
  let content: string;
  try {
    content = new TextDecoder().decode(
      await vscode.workspace.fs.readFile(uri),
    );
  } catch {
    removeTestFile(ctrl, uri);
    return;
  }

  const wsRoot = workspaceOf(uri);
  const parent = wsRoot
    ? getOrCreateProjectItem(ctrl, wsRoot)
    : undefined;

  const fileId = `file:${uri.toString()}`;
  let fileItem = parent
    ? parent.children.get(fileId)
    : ctrl.items.get(fileId);

  if (!fileItem) {
    // Show path relative to workspace so the label is "tests/math_test.qa"
    const label = wsRoot
      ? path.relative(wsRoot, uri.fsPath)
      : path.basename(uri.fsPath);
    fileItem = ctrl.createTestItem(fileId, label, uri);
    if (parent) parent.children.add(fileItem);
    else ctrl.items.add(fileItem);
  }

  const fns = scanTestFunctions(content);
  const children: vscode.TestItem[] = [];
  for (const fn of fns) {
    const child = ctrl.createTestItem(`fn:${uri.toString()}::${fn}`, fn, uri);
    const lineNo = findFunctionLine(content, fn);
    if (lineNo >= 0) child.range = new vscode.Range(lineNo, 0, lineNo, 0);
    children.push(child);
  }
  fileItem.children.replace(children);
}

function removeTestFile(ctrl: vscode.TestController, uri: vscode.Uri): void {
  const fileId = `file:${uri.toString()}`;
  const wsRoot = workspaceOf(uri);
  if (wsRoot) {
    const proj = projectItems.get(wsRoot);
    if (proj) {
      proj.children.delete(fileId);
      if (proj.children.size === 0) {
        ctrl.items.delete(proj.id);
        projectItems.delete(wsRoot);
      }
      return;
    }
  }
  ctrl.items.delete(fileId);
}

function scanTestFunctions(src: string): string[] {
  const results: string[] = [];
  for (const line of src.split("\n")) {
    const m = line.trimStart().match(/^fn\s+(test_\w+)\s*\(/);
    if (m) results.push(m[1]);
  }
  return results;
}

function findFunctionLine(src: string, fnName: string): number {
  const lines = src.split("\n");
  for (let i = 0; i < lines.length; i++) {
    if (new RegExp(`^\\s*fn\\s+${fnName}\\s*\\(`).test(lines[i])) return i;
  }
  return -1;
}

// Map from FileCoverage uri → detailed per-line data, populated during a
// coverage run so loadDetailedCoverage can return it on demand.
const coverageDetails = new Map<string, vscode.FileCoverageDetail[]>();

async function loadDetailedCoverage(
  _run: vscode.TestRun,
  coverage: vscode.FileCoverage,
  _token: vscode.CancellationToken,
): Promise<vscode.FileCoverageDetail[]> {
  return coverageDetails.get(coverage.uri.toString()) ?? [];
}

async function runQuantTests(
  ctrl: vscode.TestController,
  request: vscode.TestRunRequest,
  token: vscode.CancellationToken,
  withCoverage: boolean,
): Promise<void> {
  coverageDetails.clear();
  const run = ctrl.createTestRun(request);

  // Collect all leaf (function-level) test items that will run, grouped by
  // the workspace root they belong to so we can batch per project.
  type ProjectBatch = {
    wsRoot: string;
    // null fns = run everything; non-null = only these file URIs
    files: Map<string, { fileItem: vscode.TestItem; fns: Set<string> | null }>;
    runAll: boolean; // true when the project root item itself was selected
  };
  const batches = new Map<string, ProjectBatch>();

  const getOrCreateBatch = (wsRoot: string): ProjectBatch => {
    let b = batches.get(wsRoot);
    if (!b) { b = { wsRoot, files: new Map(), runAll: false }; batches.set(wsRoot, b); }
    return b;
  };

  const markAllFunctions = (run_: vscode.TestRun, fileItem: vscode.TestItem) => {
    fileItem.children.forEach((fn) => run_.started(fn));
  };

  const collectItem = (item: vscode.TestItem) => {
    // Project root item
    if (item.id.startsWith("project:")) {
      const wsRoot = item.id.slice("project:".length);
      const b = getOrCreateBatch(wsRoot);
      b.runAll = true;
      item.children.forEach((fileItem) => markAllFunctions(run, fileItem));
      return;
    }
    // File item
    if (item.id.startsWith("file:")) {
      const wsRoot = workspaceOf(item.uri!) ?? item.uri!.fsPath;
      const b = getOrCreateBatch(wsRoot);
      if (!b.runAll) {
        b.files.set(item.id, { fileItem: item, fns: null });
        markAllFunctions(run, item);
      }
      return;
    }
    // Function item
    const fileItem = item.parent;
    if (!fileItem) return;
    const wsRoot = workspaceOf(item.uri!) ?? item.uri!.fsPath;
    const b = getOrCreateBatch(wsRoot);
    if (!b.runAll) {
      const existing = b.files.get(fileItem.id);
      if (!existing) {
        b.files.set(fileItem.id, { fileItem, fns: new Set([item.label]) });
      } else if (existing.fns !== null) {
        existing.fns.add(item.label);
      }
      run.started(item);
    }
  };

  if (request.include) {
    for (const item of request.include) collectItem(item);
  } else {
    ctrl.items.forEach((item) => collectItem(item));
  }

  for (const [, batch] of batches) {
    if (token.isCancellationRequested) break;

    const covPath = withCoverage
      ? path.join(require("os").tmpdir(), `quant-cov-${Date.now()}.json`)
      : null;

    if (batch.runAll) {
      // Run the entire test suite in one shot
      const args = ["test"];
      if (covPath) args.push("--cov-out", covPath);
      const output = await spawnGlados(args, batch.wsRoot, token);
      if (output === null) { run.end(); return; }

      // Collect all function test items across all files for this project
      const allFnItems = new Map<string, vscode.TestItem>();
      const projItem = projectItems.get(batch.wsRoot);
      projItem?.children.forEach((fileItem) => {
        fileItem.children.forEach((fn) => allFnItems.set(fn.label, fn));
      });
      reportResults(run, output, allFnItems);
    } else {
      // Run individual files
      for (const [, { fileItem, fns }] of batch.files) {
        if (token.isCancellationRequested) break;

        const fileUri = fileItem.uri;
        if (!fileUri) continue;

        const fnItems = new Map<string, vscode.TestItem>();
        fileItem.children.forEach((fn) => {
          if (!fns || fns.has(fn.label)) fnItems.set(fn.label, fn);
        });

        const args = ["test", fileUri.fsPath];
        if (covPath) args.push("--cov-out", covPath);

        const output = await spawnGlados(args, batch.wsRoot, token);
        if (output === null) { fnItems.forEach((fn) => run.skipped(fn)); continue; }
        reportResults(run, output, fnItems);
      }
    }

    if (covPath && fs.existsSync(covPath)) {
      try {
        const covJson = JSON.parse(fs.readFileSync(covPath, "utf8")) as CovReport;
        fs.unlinkSync(covPath);
        await reportCoverage(run, covJson, batch.wsRoot);
      } catch { /* skip */ }
    }
  }

  run.end();
}

function reportResults(
  run: vscode.TestRun,
  output: string,
  testItems: Map<string, vscode.TestItem>,
): void {
  // Strip ANSI escape codes
  const text = output.replace(/\x1b\[[0-9;]*[a-zA-Z]/g, "");
  const lines = text.split("\n");

  // Tracks the current failing function while we collect its error lines
  let failFn: string | null = null;
  let failLines: string[] = [];
  const reported = new Set<string>();

  const flush = () => {
    if (failFn !== null) {
      const item = testItems.get(failFn);
      if (item) {
        run.failed(item, new vscode.TestMessage(failLines.join("\n").trim()));
        reported.add(failFn);
      }
      failFn = null;
      failLines = [];
    }
  };

  // Result line: "  test_foo ........ ok" or "  test_foo ........ FAIL"
  const RE_RESULT = /^\s+(test_\w+)\s*\.+\s+(ok|FAIL)\s*$/;
  // Detail line: "    <error message>" (4-space indent)
  const RE_DETAIL = /^    (.+)/;

  for (const line of lines) {
    const rm = line.match(RE_RESULT);
    if (rm) {
      flush();
      const [, fnName, verdict] = rm;
      if (verdict === "ok") {
        const item = testItems.get(fnName);
        if (item) { run.passed(item); reported.add(fnName); }
      } else {
        failFn = fnName;
      }
      continue;
    }
    if (failFn) {
      const dm = line.match(RE_DETAIL);
      if (dm) {
        failLines.push(dm[1]);
      } else if (line.trim() !== "") {
        flush();
      }
    }
  }
  flush();
}

// ---------------------------------------------------------------------------
// Coverage reporting

interface CovFunction { name: string; covered: boolean }
interface CovFile {
  path: string;
  lines_hit: number;
  lines_total: number;
  branches_hit: number;
  branches_total: number;
  functions: CovFunction[];
}
interface CovReport {
  files: CovFile[];
}

async function reportCoverage(
  run: vscode.TestRun,
  report: CovReport,
  projectRoot: string,
): Promise<void> {
  for (const file of report.files) {
    const absPath = path.isAbsolute(file.path)
      ? file.path
      : path.join(projectRoot, file.path);
    const uri = vscode.Uri.file(absPath);

    let src = "";
    try {
      src = new TextDecoder().decode(await vscode.workspace.fs.readFile(uri));
    } catch {
      continue;
    }

    const detail = buildLineDetail(src, file.functions);
    coverageDetails.set(uri.toString(), detail);

    const executed = detail.filter(
      (d) => d instanceof vscode.StatementCoverage && d.executed,
    ).length;
    const total = detail.length;

    run.addCoverage(
      vscode.FileCoverage.fromDetails(uri, detail),
    );
    void executed; void total; // used via detail
  }
}

// Build per-line StatementCoverage entries by finding each function's line
// range in the source and marking it covered/uncovered accordingly.
function buildLineDetail(
  src: string,
  functions: CovFunction[],
): vscode.StatementCoverage[] {
  const srcLines = src.split("\n");
  const nLines = srcLines.length;

  // Find the start line of each function declaration
  const fnStarts = new Map<string, number>();
  for (const { name } of functions) {
    for (let i = 0; i < nLines; i++) {
      if (new RegExp(`^\\s*fn\\s+${name}\\s*\\(`).test(srcLines[i])) {
        fnStarts.set(name, i);
        break;
      }
    }
  }

  // Sort functions by start line so we can compute ranges
  const sorted = [...functions]
    .filter((f) => fnStarts.has(f.name))
    .sort((a, b) => (fnStarts.get(a.name) ?? 0) - (fnStarts.get(b.name) ?? 0));

  // Build a line→covered map: each function owns lines from its start to the
  // line before the next function (or end of file).
  const lineCovered = new Map<number, boolean>();
  for (let i = 0; i < sorted.length; i++) {
    const fn = sorted[i];
    const start = fnStarts.get(fn.name) ?? 0;
    const end =
      i + 1 < sorted.length
        ? (fnStarts.get(sorted[i + 1].name) ?? nLines) - 1
        : nLines - 1;
    for (let ln = start; ln <= end; ln++) {
      const trimmed = srcLines[ln].trim();
      if (trimmed !== "" && !trimmed.startsWith("#") && !trimmed.startsWith("//")) {
        lineCovered.set(ln, fn.covered);
      }
    }
  }

  const result: vscode.StatementCoverage[] = [];
  for (const [ln, covered] of lineCovered) {
    result.push(
      new vscode.StatementCoverage(
        covered ? 1 : 0,
        new vscode.Position(ln, 0),
      ),
    );
  }
  return result;
}

function spawnGlados(
  args: string[],
  cwd: string | undefined,
  token: vscode.CancellationToken,
): Promise<string | null> {
  const configured = vscode.workspace
    .getConfiguration("quant-lsp")
    .get<string>("gladosPath", "");

  let proc: ReturnType<typeof spawn>;

  if (configured) {
    // Explicit path set: use it directly, warn if missing.
    const bin = path.isAbsolute(configured)
      ? (fs.existsSync(configured) ? configured : undefined)
      : findOnPath(configured);
    if (!bin) {
      void vscode.window.showWarningMessage(
        `Quant: glados binary not found at "${configured}". Check quant-lsp.gladosPath.`,
      );
      return Promise.resolve(null);
    }
    proc = spawn(bin, args, { cwd, env: process.env, stdio: "pipe" });
  } else {
    // No explicit path: invoke through a login shell so Nix / profile
    // environment variables (PATH, QUANT_STDLIB, etc.) are available.
    const shell = process.env.SHELL || "/bin/bash";
    const cmd = ["glados", ...args]
      .map((a) => `'${a.replace(/'/g, "'\\''")}'`)
      .join(" ");
    proc = spawn(shell, ["-l", "-c", cmd], { cwd, stdio: "pipe" });
  }

  return new Promise((resolve) => {
    let out = "";
    proc.stdout?.on("data", (d: Buffer) => { out += d.toString(); });
    proc.stderr?.on("data", (d: Buffer) => { out += d.toString(); });
    proc.on("close", () => resolve(out));
    proc.on("error", () => resolve(out));
    token.onCancellationRequested(() => { proc.kill(); resolve(null); });
  });
}

// ---------------------------------------------------------------------------
// Debug adapter

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
