import * as vscode from "vscode"
import * as path from "node:path"
import * as fs from "node:fs"

export function activate(context: vscode.ExtensionContext) {
	const diagnosticCollection =
		vscode.languages.createDiagnosticCollection("markdownrealcode")
	context.subscriptions.push(diagnosticCollection)

	const validatePaths = (doc: vscode.TextDocument) => {
		if (!doc.fileName.endsWith(".src.md")) {
			return
		}

		const content = doc.getText()
		const regex = /\[>src:([^\]]+)\]/g // Match [>src:FILEPATH]
		const diagnostics: vscode.Diagnostic[] = []
		const match = regex.exec(content)

		if (match !== null) {
			const filepath = match[1] // Extract FILEPATH from the capture group
			if (filepath) {
				const start = doc.positionAt(match.index + 6) // Start after "[>src:"
				const end = doc.positionAt(match.index + 6 + filepath.length) // End before "]"
				const range = new vscode.Range(start, end)

				const currentDir = path.dirname(doc.uri.fsPath)
				const absolutePath = path.join(currentDir, filepath)

				if (
					!fs.existsSync(absolutePath) ||
					!fs.statSync(absolutePath).isFile()
				) {
					diagnostics.push(
						new vscode.Diagnostic(
							range,
							`Invalid file path: ${filepath}`,
							vscode.DiagnosticSeverity.Error,
						),
					)
				}
			}

			diagnosticCollection.set(doc.uri, diagnostics)
		}
	}

	context.subscriptions.push(
		vscode.workspace.onDidChangeTextDocument((event) => {
			validatePaths(event.document)
		}),
	)

	context.subscriptions.push(
		vscode.workspace.onDidOpenTextDocument((doc) => {
			validatePaths(doc)
		}),
	)

	vscode.workspace.textDocuments.forEach(validatePaths)
}
