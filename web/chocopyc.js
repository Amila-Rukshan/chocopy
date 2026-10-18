// Compiles ChocoPy to wasm in the browser with chocopyc built by Emscripten
// (compiler/chocopyc.mjs and compiler/chocopyc.wasm; see the README).

import createChocoPyc from "./compiler/chocopyc.mjs";

// The compiler's wasm is fetched and compiled once; each compile runs in a
// fresh instance, since LLVM keeps command-line options in global state.
let compilerModule;

async function loadCompilerModule() {
  if (!compilerModule) {
    const url = new URL("./compiler/chocopyc.wasm", import.meta.url);
    // Node's fetch cannot read files; this lets tests use the same code
    if (url.protocol === "file:") {
      const { readFile } = await import("node:fs/promises");
      compilerModule = WebAssembly.compile(await readFile(url));
    } else {
      compilerModule = WebAssembly.compileStreaming(fetch(url));
    }
  }
  return compilerModule;
}

// Compiles ChocoPy source text. Resolves to { wasm, diagnostics }: wasm is the
// program's module bytes, or null if compilation failed, in which case
// diagnostics has the compiler's error messages.
export async function compileChocoPy(source, fileName = "program.py") {
  const module = await loadCompilerModule();
  let diagnostics = "";
  const compiler = await createChocoPyc({
    instantiateWasm(imports, done) {
      WebAssembly.instantiate(module, imports).then((instance) =>
        done(instance, module));
      return {};
    },
    print: (line) => (diagnostics += line + "\n"),
    printErr: (line) => (diagnostics += line + "\n"),
  });

  // The file name appears in runtime error messages, so keep its directories
  const outputName = fileName.replace(/\.py$/, "") + ".wasm";
  const dir = fileName.substring(0, fileName.lastIndexOf("/"));
  if (dir)
    compiler.FS.mkdirTree(dir);
  compiler.FS.writeFile(fileName, source);
  const exitCode = compiler.callMain([fileName, "-o", outputName]);
  const wasm = exitCode === 0 ? compiler.FS.readFile(outputName) : null;
  return { wasm, diagnostics };
}
