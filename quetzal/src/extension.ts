import * as vscode from "vscode";
import { WasmContext, Memory } from "@vscode/wasm-component-model";

import { formatter } from "./formatter";

export async function activate(context: vscode.ExtensionContext): Promise<void> {
	const channel = vscode.window.createOutputChannel("Calculator");
	context.subscriptions.push(channel);

	const log = vscode.window.createOutputChannel("Calculator - Log", { log: true });
	context.subscriptions.push(log);

	// Load the Wasm module
	const filename = vscode.Uri.joinPath(
		context.extensionUri,
		"target",
		"wasm32-unknown-unknown",
		"debug",
		"quetzal.wasm",
	);
	const bits = (await vscode.workspace.fs.readFile(filename)) as Uint8Array<ArrayBuffer>;
	const module = await WebAssembly.compile(bits);

	// The context for the WASM module
	const wasmContext: WasmContext.Default = new WasmContext.Default();

	// Instantiate the module
	const instance = await WebAssembly.instantiate(module, {});
	// Bind the WASM memory to the context
	wasmContext.initialize(new Memory.Default(instance.exports));

	// Bind the TypeScript Api
	const api = formatter._.exports.bind(instance.exports as formatter._.Exports, wasmContext);

	context.subscriptions.push(
		vscode.commands.registerCommand("koatl.format", () => {
			channel.show();
			channel.appendLine("Running formatter example");
			channel.appendLine(`Add ${api.format("  let x = 1 + 2;   ")}`);
		}),
	);

	channel.show();
	channel.appendLine("Activating");

	vscode.languages.registerDocumentFormattingEditProvider("koatl", {
		provideDocumentFormattingEdits(document) {
			channel.show();
			channel.appendLine("Running formatter");

			const doc = document.getText();
			const formatted = api.format(doc);

			channel.appendLine(formatted);

			return [vscode.TextEdit.replace(new vscode.Range(0, 0, document.lineCount, 0), formatted)];
		},
	});
}
