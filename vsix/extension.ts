import * as path from "node:path"
import { workspace, type ExtensionContext } from "vscode"
import {
	LanguageClient,
	type LanguageClientOptions,
	type ServerOptions,
	TransportKind,
} from "vscode-languageclient/node"

let client: LanguageClient

export function activate(context: ExtensionContext) {
	const platform = process.platform
	const arch = process.arch

	let binaryName: string
	if (platform === "win32" && arch === "x64") {
		binaryName = "mdrc-lsp-windows.exe"
	} else if (platform === "linux" && arch === "x64") {
		binaryName = "mdrc-lsp-linux"
	} else if (platform === "darwin" && arch === "x64") {
		binaryName = "mdrc-lsp-darwin-x64"
	} else if (platform === "darwin" && arch === "arm64") {
		binaryName = "mdrc-lsp-darwin-arm64"
	} else {
		throw new Error(`Unsupported platform: ${platform}-${arch}`)
	}

	const serverCommand = context.asAbsolutePath(path.join("bin", binaryName))
	const serverOptions: ServerOptions = {
		command: serverCommand,
		args: [],
		transport: TransportKind.stdio,
	}

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "markdownrealcode" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/*.src.md"),
		},
	}

	client = new LanguageClient(
		"markdownRealCodeLsp",
		"Markdown Real Code LSP",
		serverOptions,
		clientOptions,
	)

	client.start()
}

export function deactivate(): Thenable<void> | undefined {
	return client ? client.stop() : undefined
}
