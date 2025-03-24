import * as vscode from "vscode"
import * as path from "node:path"
import * as fs from "node:fs"

export function activate(context: vscode.ExtensionContext) {
	const diagnosticCollection =
		vscode.languages.createDiagnosticCollection("markdown")
	context.subscriptions.push(diagnosticCollection)

	const validatePaths = (doc: vscode.TextDocument) => {
		if (!doc.fileName.endsWith(".src.md")) {
			return
		}

		const content = doc.getText()
		// const regex = /\[>src="([^\]]+)"\]/g // Match [>src:FILEPATH]
		//		const regex = /\[>.*\]/g // Match [>ANYTHING]
		const regex = /\[>(.*)\]/g
		const diagnostics: vscode.Diagnostic[] = []
		let match: RegExpExecArray | null

		// biome-ignore lint/suspicious/noAssignInExpressions: whatever
		while ((match = regex.exec(content)) !== null) {
			const rest = match[1] // Extract FILEPATH from the capture group
			if (!rest) {
				continue
			}
			if (!rest.startsWith('src="')) {
				diagnostics.push(
					new vscode.Diagnostic(
						new vscode.Range(
							doc.positionAt(match.index + 1),
							doc.positionAt(match.index + 1 + rest.length + 1),
						),
						`Invalid reference format. Expected [>src="FILEPATH"]`,
						vscode.DiagnosticSeverity.Error,
					),
				)
			}
			if (!rest.endsWith('"')) {
				diagnostics.push(
					new vscode.Diagnostic(
						new vscode.Range(
							doc.positionAt(match.index + rest.length + 2),
							doc.positionAt(match.index + rest.length + 3),
						),
						`Invalid reference format: Missing closing quote.`,
						vscode.DiagnosticSeverity.Error,
					),
				)
				continue
			}
			const filepath = rest.substring(5, rest.length - 1)
			const start = doc.positionAt(match.index + 7) // Start after "[>src:"
			const end = doc.positionAt(match.index + 7 + filepath.length) // End before "]"
			const range = new vscode.Range(start, end)

			// Resolve the filepath relative to the current file's directory
			const currentDir = path.dirname(doc.uri.fsPath)
			const absolutePath = path.join(currentDir, filepath)

			// Check if the path exists and is a file
			if (!fs.existsSync(absolutePath) || !fs.statSync(absolutePath).isFile()) {
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

	const provider = vscode.languages.registerCompletionItemProvider(
		["markdown"],
		{
			provideCompletionItems(
				document: vscode.TextDocument,
				position: vscode.Position,
			) {
				const linePrefix = document
					.lineAt(position)
					.text.substr(0, position.character)

				if (!linePrefix.endsWith("[>")) {
					return undefined
				}

				const completion = new vscode.CompletionItem('src=""')
				completion.insertText = new vscode.SnippetString('src="${1}"')
				completion.documentation = new vscode.MarkdownString(
					"Adds src attribute to [> tag",
				)

				completion.range = new vscode.Range(position, position)

				completion.commitCharacters = ['"']

				return [completion]
			},
		},
		">",
	)

	context.subscriptions.push(provider)
}
