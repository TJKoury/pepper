# ifx in Docker

Build and test PEPPER with Intel Fortran (ifx) on any machine with Docker,
the same way the CI job "ifx (ubuntu-latest)" does: Ubuntu 24.04, Intel's apt
package `intel-oneapi-compiler-fortran-2026.2`, fpm 0.13.0. Use it to
reproduce an ifx failure of the CI locally.

```sh
tools/ifx_docker/ifx.sh image                  # build the image (once; ~1 GB download)
tools/ifx_docker/ifx.sh test                   # unit tests, then test_parallel with OpenMP
tools/ifx_docker/ifx.sh tspice                 # build tspice, run all the families
tools/ifx_docker/ifx.sh tspice F_AZL 'spk*'    # ... or some of them
tools/ifx_docker/ifx.sh family F_AZL -O0       # one family, its source compiled with other options
tools/ifx_docker/ifx.sh run 'ifx --version'    # any command
tools/ifx_docker/ifx.sh shell                  # an interactive shell
```

Each command first copies the repository (without `.git` and the build
directories) into the Docker volume `pepper-ifx-work`, where the ifx builds
stay between runs: fpm compiles only what changed. Logs, and the output of the
tspice families that failed (`build/ifx_docker/tspice/F_*/output.txt`), are
written to `build/ifx_docker/` in the repository.

## Notes

- **x86-64 only.** Intel's compiler runs only on x86-64. On an ARM machine
  (Apple silicon), Docker runs the image under emulation: correct, but a full
  build of tspice takes about 25 minutes.
- **At most 4 tspice families at once** (`IFX_JOBS`, default 4). Under
  emulation, more parallel families fail at random with no output at all,
  failures that do not happen when they run alone.
- **`family`** recompiles only the test family's source (`tspice/src/<name>.f90`)
  with the given ifx options and links it with the rest of the last tspice
  build, in about a minute: a quick way to see whether an option (`-O0`,
  `-fp-model ...`) changes a failure. To try options on library code, use
  `run` to compile a library source with them and link it before the
  archive.
- **Versions.** Keep `IFX_VERSION` and `FPM_VERSION` in the `Dockerfile` the
  same as in `.github/workflows/ci.yml`; `docker build --build-arg
  IFX_VERSION=...` tries another release.
- **Clean start:** `docker volume rm pepper-ifx-work` discards the builds.

Two ifx problems found this way (both invisible with gfortran): Intel's default
floating-point model (`fast`) undoing SPICELIB's careful arithmetic, fixed with
`-fp-model precise` (`fpm.toml`); and SPICELIB callers passing one variable as
an input and the output of the legacy wrappers, fixed by passing copies
(`src/legacy`).
