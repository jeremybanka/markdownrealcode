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
		outdir: join(import.meta.dir, "dist"),
		packages: `bundle`,
		minify: true,
		sourcemap: `external`,
		format: `cjs`,
		target: `node`,
		external: ["vscode"],
	}),
	// Bun.$`cabal build -O2 mdrc-lsp`.then(async (result) => {
	// 	const mdrcLspPath = (await Bun.$`cabal list-bin mdrc-lsp`).text().trim()
	// 	return Bun.$`cp ${mdrcLspPath} vsix/bin/mdrc-lsp-${currentPlatform}-${currentArch}`
	// }),
] as const)

// Step 3: Create the VSIX package
await VSCE.createVSIX({
	cwd: import.meta.dir,
	packagePath: `markdownrealcode-vscode-${currentPlatform}-${currentArch}.vsix`,
	dependencies: false,
})

console.log(`VSIX created successfully for ${currentPlatform}-${currentArch}`)
