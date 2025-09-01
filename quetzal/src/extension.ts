import * as vscode from "vscode";
import { WasmContext, Memory } from "@vscode/wasm-component-model";

import { formatter } from "./formatter";

export async function activate(context: vscode.ExtensionContext): Promise<void> {
	const log = vscode.window.createOutputChannel("Quetzal", { log: true });
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

	vscode.languages.registerDocumentFormattingEditProvider("koatl", {
		provideDocumentFormattingEdits(document) {
			const doc = document.getText();
			const formatted = api.format(doc);

			return [vscode.TextEdit.replace(new vscode.Range(0, 0, document.lineCount, 0), formatted)];
		},
	});
}
