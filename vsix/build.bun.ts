#!/usr/bin/env bun

import * as Bun from "bun"
import * as VSCE from "@vscode/vsce"
import { platform, arch } from "node:os"
import { join } from "node:path"

const currentPlatform = platform()
const currentArch = arch()

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
	Bun.$`cabal build -O2 mdrc-lsp`.then(async (result) => {
		const mdrcLspPath = (await Bun.$`cabal list-bin mdrc-lsp`).text().trim()
		return Bun.$`cp ${mdrcLspPath} vsix/bin/mdrc-lsp-${currentPlatform}-${currentArch}`
	}),
] as const)

// Step 2: Determine the platform-specific LSP binary
const platformMap: Record<string, Record<string, string>> = {
	win32: { x64: "mdrc-lsp-windows.exe" },
	linux: { x64: "mdrc-lsp-linux" },
	darwin: { x64: "mdrc-lsp-darwin-x64", arm64: "mdrc-lsp-darwin-arm64" },
}

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
