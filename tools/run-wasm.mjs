// Runs a ChocoPy program compiled with --target=wasm32 under Node's WASI.
// Usage: node tools/run-wasm.mjs prog.wasm
import { readFile } from "node:fs/promises";
import { WASI } from "node:wasi";

const wasi = new WASI({ version: "preview1", returnOnExit: true });
const module = await WebAssembly.compile(await readFile(process.argv[2]));
const instance = await WebAssembly.instantiate(module, wasi.getImportObject());
process.exitCode = wasi.start(instance);
