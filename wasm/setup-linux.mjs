import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import path from "node:path";
import { downloads, fetchPinned, ensureLlvmSource, root, unpackPinned } from "./toolchain-downloads.mjs";

if (process.platform !== "linux" || process.arch !== "x64") {
  throw new Error("Release toolchain setup requires Linux x64.");
}
const cache = path.join(root, ".cache");
const wasi = await unpackPinned("wasi", path.join(cache, "wasi-sdk"));
const binaryen = await unpackPinned("binaryen", path.join(cache, "binaryen"));
const flang = path.join(cache, "flang-wasm");
const marker = path.join(flang, ".pepper-native-flang-sha256");
if (await fs.readFile(marker, "utf8").catch(() => "") !== downloads.llvm.sha256) {
  const source = await ensureLlvmSource();
  const build = path.join(cache, "llvm-native-build");
  execFileSync("cmake", ["-S", path.join(source, "llvm"), "-B", build, "-G", "Ninja",
    "-DCMAKE_BUILD_TYPE=Release", "-DLLVM_ENABLE_PROJECTS=clang;mlir;flang",
    "-DLLVM_TARGETS_TO_BUILD=X86;WebAssembly", "-DLLVM_INCLUDE_TESTS=OFF",
    "-DFLANG_INCLUDE_TESTS=OFF", "-DLLVM_INCLUDE_BENCHMARKS=OFF",
    "-DLLVM_ENABLE_ZLIB=OFF", "-DLLVM_ENABLE_ZSTD=OFF",
    "-DLLVM_ENABLE_LIBXML2=OFF", "-DLLVM_ENABLE_TERMINFO=OFF",
  ], { stdio: "inherit" });
  execFileSync("cmake", ["--build", build, "--target", "flang", "--parallel", "4"], { stdio: "inherit" });
  await fs.mkdir(path.join(flang, "bin"), { recursive: true });
  await fs.mkdir(path.join(flang, "include", "flang"), { recursive: true });
  await fs.copyFile(path.join(build, "bin", "flang"), path.join(flang, "bin", "flang"));
  await fs.chmod(path.join(flang, "bin", "flang"), 0o755);
  await fs.copyFile(path.join(wasi, "bin", "llvm-ar"), path.join(flang, "bin", "llvm-ar"));
  await fs.chmod(path.join(flang, "bin", "llvm-ar"), 0o755);
  await fs.writeFile(marker, downloads.llvm.sha256);
  await fs.rm(build, { recursive: true, force: true });
}
const browserArchive = await fetchPinned("chromium");
const browserDir = path.join(cache, "chromium");
await fs.mkdir(browserDir, { recursive: true });
execFileSync("unzip", ["-q", "-o", browserArchive, "-d", browserDir], { stdio: "inherit" });
const env = {
  CHROME_BIN: path.join(browserDir, "chrome-linux64", "chrome"),
  FLANG_WASM_DIR: flang,
  SDN_WASI_CLANG: path.join(wasi, "bin", "clang"),
  SDN_WASI_CLANGXX: path.join(wasi, "bin", "clang++"),
  SDN_WASI_SYSROOT: path.join(wasi, "share", "wasi-sysroot"),
  SDN_WASI_RESOURCE_DIR: path.join(wasi, "lib", "clang", "21"),
};
if (!process.env.GITHUB_ENV || !process.env.GITHUB_PATH) throw new Error("GITHUB_ENV and GITHUB_PATH are required.");
await fs.appendFile(process.env.GITHUB_ENV, Object.entries(env).map(([k, v]) => `${k}=${v}\n`).join(""));
await fs.appendFile(process.env.GITHUB_PATH, `${path.join(binaryen, "bin")}\n`);
