// Assembles the playground site: the files in web/ and the wasm build of
// chocopyc.
//
// Usage: node tools/build-site.mjs [--compiler=build-wasm/bin] [--out=site]
import { copyFileSync, cpSync, mkdirSync, rmSync } from "node:fs";
import { join } from "node:path";

const root = new URL("..", import.meta.url).pathname;
const options = { compiler: "build-wasm/bin", out: "site" };
for (const arg of process.argv.slice(2)) {
  const [, name, value] = arg.match(/^--(\w+)=(.*)$/) ?? [];
  if (!(name in options)) {
    console.error(`Unknown argument ${arg}`);
    process.exit(1);
  }
  options[name] = value;
}

rmSync(options.out, { recursive: true, force: true });
cpSync(join(root, "web"), options.out, { recursive: true });

const compilerDir = join(options.out, "compiler");
mkdirSync(compilerDir);
for (const file of ["chocopyc.mjs", "chocopyc.wasm"])
  copyFileSync(join(options.compiler, file), join(compilerDir, file));

console.log(`Site written to ${options.out}`);
