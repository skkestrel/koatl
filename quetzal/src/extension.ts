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

			// Get the diff instead of the full formatted text
			const diffHunks = api.formatDiff(doc);

			// Convert diff hunks to TextEdits
			const edits: vscode.TextEdit[] = [];

			for (const hunk of diffHunks) {
				// Process each hunk based on its type and lines
				let currentOldLine = hunk.oldStart;
				let currentNewLine = hunk.newStart;

				// Group consecutive changes of the same type
				let pendingDeletes: string[] = [];
				let pendingInserts: string[] = [];
				let deleteStartLine = currentOldLine;

				const flushPendingChanges = () => {
					if (pendingDeletes.length > 0 || pendingInserts.length > 0) {
						const startPos = new vscode.Position(deleteStartLine, 0);
						const endPos = new vscode.Position(deleteStartLine + pendingDeletes.length, 0);
						const range = new vscode.Range(startPos, endPos);
						const replacement = pendingInserts.join("");
						edits.push(vscode.TextEdit.replace(range, replacement));

						pendingDeletes = [];
						pendingInserts = [];
					}
				};

				for (const line of hunk.lines) {
					switch (line.tag) {
						case formatter.OhtliDiffTag.equal:
							flushPendingChanges();
							currentOldLine++;
							currentNewLine++;
							deleteStartLine = currentOldLine;
							break;

						case formatter.OhtliDiffTag.delete:
							if (pendingDeletes.length === 0) {
								deleteStartLine = currentOldLine;
							}
							pendingDeletes.push(line.value);
							currentOldLine++;
							break;

						case formatter.OhtliDiffTag.insert:
							pendingInserts.push(line.value);
							currentNewLine++;
							break;
					}
				}

				// Flush any remaining changes
				flushPendingChanges();
			}

			return edits;
		},
	});
}
