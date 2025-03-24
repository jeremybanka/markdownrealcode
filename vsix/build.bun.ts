#!/usr/bin/env bun

import * as Bun from "bun"
import * as VSCE from "@vscode/vsce"
import { join } from "node:path"

await Promise.all([
	Bun.build({
		entrypoints: [join(import.meta.dir, "extension.ts")],
		outdir: import.meta.dir,
		packages: `bundle`,
		minify: true,
		format: `cjs`,
		target: `node`,
		external: ["vscode"],
	}),
	// Bun.$`cabal build -O2 mdrc-lsp`.then(async (result) => {
	// 	const mdrcLspPath = (await Bun.$`cabal list-bin mdrc-lsp`).text().trim()
	// 	return Bun.$`cp ${mdrcLspPath} vsix/bin/mdrc-lsp-${currentPlatform}-${currentArch}`
	// }),
] as const)

await VSCE.createVSIX({
	cwd: import.meta.dir,
	packagePath: `markdownrealcode.vsix`,
	dependencies: false,
})

console.log(`VSIX created successfully.`)
