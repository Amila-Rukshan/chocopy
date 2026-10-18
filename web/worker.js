// Compiles and runs ChocoPy programs off the page's main thread, so a program
// that never ends cannot freeze the page: the page terminates the worker.
//
// Messages in:  { type: "load" } or { type: "run", source, fileName }
// Messages out: { type: "status", text }, { type: "output", text },
//               { type: "compileError", diagnostics },
//               { type: "done", exitCode, compileMs, size }, { type: "error", text }

import { compileChocoPy, loadCompilerModule } from "./chocopyc.js";
import { runChocoPy } from "./chocopy-wasm.js";

self.onmessage = async ({ data }) => {
  try {
    if (data.type === "load") {
      await loadCompilerModule();
      self.postMessage({ type: "status", text: "Compiler ready" });
      return;
    }

    self.postMessage({ type: "status", text: "Compiling…" });
    const start = performance.now();
    const { wasm, diagnostics } = await compileChocoPy(data.source, data.fileName);
    const compileMs = Math.round(performance.now() - start);
    if (!wasm) {
      self.postMessage({ type: "compileError", diagnostics });
      return;
    }

    self.postMessage({ type: "status", text: "Running…" });
    const exitCode = await runChocoPy(wasm, (text) =>
      self.postMessage({ type: "output", text }));
    self.postMessage({ type: "done", exitCode, compileMs, size: wasm.length });
  } catch (e) {
    self.postMessage({ type: "error", text: String(e) });
  }
};
