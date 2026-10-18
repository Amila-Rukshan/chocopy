// Checks the wasm build of chocopyc in the assembled site (chocopyc.js, as the
// playground uses it; see tools/build-site.mjs) against the native chocopyc's
// --target=wasm32: for each program, both compilers must produce the same
// module, and running it (chocopy-wasm.js) must print the same output and
// exit with the same code.
//
// Usage: node tools/test-browser-compiler.mjs [--site=site]
//          [--native=build/bin/chocopyc] prog.py...
import { execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

let site = "site";
let nativeCompiler = "build/bin/chocopyc";
const files = [];
for (const arg of process.argv.slice(2)) {
  if (arg.startsWith("--site="))
    site = arg.slice("--site=".length);
  else if (arg.startsWith("--native="))
    nativeCompiler = arg.slice("--native=".length);
  else
    files.push(arg);
}

const siteURL = pathToFileURL(resolve(site) + "/");
const { compileChocoPy } = await import(new URL("chocopyc.js", siteURL));
const { runChocoPy } = await import(new URL("chocopy-wasm.js", siteURL));

async function run(wasm) {
  let output = "";
  const exitCode = await runChocoPy(wasm, (text) => (output += text));
  return { output, exitCode };
}

const tmp = mkdtempSync(join(tmpdir(), "chocopy-"));
let failures = 0;
for (const file of files) {
  const fail = (why) => {
    console.log(`FAIL ${file}: ${why}`);
    failures++;
  };

  const { wasm, diagnostics } = await compileChocoPy(readFileSync(file, "utf8"), file);
  if (!wasm) {
    fail(`compilation failed\n${diagnostics}`);
    continue;
  }
  const nativeOut = join(tmp, "native.wasm");
  execFileSync(nativeCompiler, ["--target=wasm32", file, "-o", nativeOut]);
  const nativeWasm = readFileSync(nativeOut);

  const got = await run(wasm);
  const expected = await run(nativeWasm);
  if (got.output !== expected.output || got.exitCode !== expected.exitCode)
    fail(`output differs\n--- native\n${expected.output}--- browser\n${got.output}`);
  else if (Buffer.compare(Buffer.from(wasm), nativeWasm) !== 0)
    fail("module differs from the native compiler's");
  else
    console.log(`ok   ${file}`);
}
rmSync(tmp, { recursive: true });
console.log(`${files.length - failures} passed, ${failures} failed`);
process.exitCode = failures ? 1 : 0;
