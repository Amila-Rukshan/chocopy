// Runs ChocoPy programs compiled with `chocopyc --target=wasm32` in the
// browser. The modules only import WASI's fd_write and proc_exit, so this
// provides just those two instead of a full WASI implementation.

class ProcExit extends Error {
  constructor(code) {
    super(`exit ${code}`);
    this.code = code;
  }
}

// Runs a compiled module. onOutput receives each chunk of stdout/stderr text.
// Resolves to the program's exit code.
export async function runChocoPy(wasmBytes, onOutput) {
  let memory;
  const decoder = new TextDecoder();
  const wasi = {
    fd_write(fd, iovs, iovsLen, writtenPtr) {
      const view = new DataView(memory.buffer);
      let written = 0;
      for (let i = 0; i < iovsLen; i++) {
        const buf = view.getUint32(iovs + i * 8, true);
        const len = view.getUint32(iovs + i * 8 + 4, true);
        onOutput(decoder.decode(new Uint8Array(memory.buffer, buf, len)));
        written += len;
      }
      view.setUint32(writtenPtr, written, true);
      return 0;
    },
    proc_exit(code) {
      throw new ProcExit(code);
    },
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, {
    wasi_snapshot_preview1: wasi,
  });
  memory = instance.exports.memory;
  try {
    instance.exports._start();
    return 0;
  } catch (e) {
    if (e instanceof ProcExit)
      return e.code;
    throw e;
  }
}
