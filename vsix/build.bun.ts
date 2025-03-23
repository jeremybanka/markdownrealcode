#!/usr/bin/env bun

import * as Bun from "bun"
import * as VSCE from "@vscode/vsce"
import { platform, arch } from "node:os"
import { join } from "node:path"

const currentPlatform = platform()

await Promise.all([
	Bun.build({
		entrypoints: [join(import.meta.dir, "extension.ts")],
		outdir: `./dist`,
		packages: `bundle`,
		minify: true,
		sourcemap: `external`,
		format: `cjs`,
		target: `node`,
		external: ["vscode"],
	}),
	Bun.$`cabal build -O2 mdrc-lsp`.then((result) => {
		console.log(result.stdout)
		Bun.$`cp $(cabal list-bin mdrc-lsp) vsix/bin/mdrc-lsp-$(uname -s | tr '[:upper:]' '[:lower:]')-$(uname -m)`
	}),
])

// Step 2: Determine the platform-specific LSP binary
const platformMap: Record<string, Record<string, string>> = {
	win32: { x64: "mdrc-lsp-windows.exe" },
	linux: { x64: "mdrc-lsp-linux" },
	darwin: { x64: "mdrc-lsp-darwin-x64", arm64: "mdrc-lsp-darwin-arm64" },
}

const currentArch = arch()
const binaryName = platformMap[currentPlatform]?.[currentArch]
if (!binaryName) {
	throw new Error(
		`Unsupported platform/arch: ${currentPlatform}-${currentArch}`,
	)
}

// Step 3: Create the VSIX package
await VSCE.createVSIX({
	cwd: import.meta.dir,
	packagePath: `markdownrealcode-vscode-${currentPlatform}-${currentArch}.vsix`, // Output file
	dependencies: false,
})

console.log(`VSIX created successfully for ${currentPlatform}-${currentArch}`)
