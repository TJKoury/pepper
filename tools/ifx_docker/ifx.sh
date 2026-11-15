#!/usr/bin/env bash
# Build and test PEPPER with Intel Fortran (ifx) in Docker, as the CI job
# "ifx (ubuntu-latest)" does. See tools/ifx_docker/README.md.
#
#   tools/ifx_docker/ifx.sh image                 build the Docker image (once)
#   tools/ifx_docker/ifx.sh test                  unit tests, then test_parallel with OpenMP
#   tools/ifx_docker/ifx.sh tspice [FAMILY ...]   build tspice, run the families (all by default)
#   tools/ifx_docker/ifx.sh family F_NAME [FLAGS] one tspice family, its source compiled with FLAGS
#   tools/ifx_docker/ifx.sh run 'COMMAND'         any command, in the copy of the repository
#   tools/ifx_docker/ifx.sh shell                 an interactive shell there
#
# Each command first copies the repository (without .git and the build
# directories) into a Docker volume, where the ifx builds stay between runs
# (fpm then compiles only what changed). Logs and the output of failed
# tspice families are written to build/ifx_docker/ in the repository.
#
# Environment: IFX_IMAGE (default pepper-ifx), IFX_VOLUME (default
# pepper-ifx-work), IFX_JOBS (tspice families run at once, default 4: under
# emulation, more give spurious failures with no output).

set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
image="${IFX_IMAGE:-pepper-ifx}"
volume="${IFX_VOLUME:-pepper-ifx-work}"
jobs="${IFX_JOBS:-4}"
out="$repo/build/ifx_docker"
mkdir -p "$out"

# Run a bash script in the container, in /work, after copying the
# repository there. -t (a terminal) only when there is one.
in_container() {
    local tty=()
    if [ -t 0 ] && [ -t 1 ]; then tty=(-it); fi
    docker run --rm ${tty[@]+"${tty[@]}"} --platform linux/amd64 \
        -v "$repo":/src:ro -v "$volume":/work -v "$out":/out \
        "$image" bash -c "
            set -eo pipefail
            source /opt/intel/oneapi/setvars.sh > /dev/null
            rsync -a --delete --exclude .git --exclude /build/ --exclude /tspice/build/ \
                  --exclude /doc/ --exclude /.pixi/ --exclude '/web copy/' /src/ /work/
            cd /work
            $1"
}

command="${1:-}"
[ $# -gt 0 ] && shift

case "$command" in

image)
    docker build --platform linux/amd64 -t "$image" "$repo/tools/ifx_docker"
    ;;

test)
    # as the CI job: unit tests, then the parallel test with OpenMP
    in_container "
        python3 tools/clean_stale.py
        fpm test 2>&1 | tee /out/test.log
        fpm test test_parallel --flag '-qopenmp -traceback -g' 2>&1 | tee /out/test_parallel.log"
    ;;

tspice)
    # build tspice, then run the families with the executable just built;
    # the output of the failed ones is copied to build/ifx_docker/tspice/
    in_container "
        python3 tools/clean_stale.py
        (cd tspice && fpm build) > /out/tspice_build.log 2>&1 || { tail -40 /out/tspice_build.log; exit 1; }
        exe=\$(ls -t tspice/build/ifx_*/app/tspice | head -1)
        status=0
        python3 tools/run_tspice.py -j $jobs \"\$exe\" $* 2>&1 | tee /out/tspice.log || status=1
        rm -rf /out/tspice && mkdir -p /out/tspice
        for d in build/tspice/F_*; do [ -d \"\$d\" ] && cp -r \"\$d\" /out/tspice/; done
        exit \$status"
    ;;

family)
    # One tspice family, its source (tspice/src/<family>.f90) compiled with
    # the given ifx flags (e.g. -O0, or -fp-model precise), linked with the
    # rest of the last tspice build ('tspice' first). A quick way to see
    # whether an option changes the result, without the 25-minute build.
    family="${1:?usage: ifx.sh family F_NAME [FLAGS]}"
    flags="${2:-}"
    upper=$(echo "$family" | tr '[:lower:]' '[:upper:]')
    lower=$(echo "$family" | tr '[:upper:]' '[:lower:]')
    in_container "
        lib=\$(ls -td /work/tspice/build/ifx_*/tspice/libtspice.a 2>/dev/null | head -1)
        [ -n \"\$lib\" ] || { echo 'no tspice build: run ifx.sh tspice first'; exit 1; }
        mods=\$(dirname \$(dirname \$lib))
        dir=/tmp/family && mkdir -p \$dir && cd \$dir
        cat > driver.f90 <<EOF
      program driver
      use pepper_legacy, pepper_legacy_failed => failed
      logical :: ok, bad
      character(80) :: cmline
      cmline = ' '
      call tsetup ( cmline, 'spice{0-9}{0-9}.log', '1.0.0' )
      call $upper ( ok )
      call tclose
      call t_anybad ( bad )
      if ( bad ) stop 1
      end program driver
EOF
        ifx -c -recursive -fp-model precise -assume byterecl $flags -I\$mods /work/tspice/src/$lower.f90 -o family.o
        ifx -recursive -I\$mods driver.f90 family.o \$lib -o driver
        status=0
        ./driver > /out/$lower.txt 2>&1 || status=\$?
        grep -E 'CASE|FAILURE|SUCCESS' /out/$lower.txt | head -20 || true
        echo \"$upper: exit \$status (output: build/ifx_docker/$lower.txt)\"
        exit \$status"
    ;;

run)
    in_container "${1:?usage: ifx.sh run 'COMMAND'}"
    ;;

shell)
    in_container "exec bash"
    ;;

*)
    sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//'
    [ -z "$command" ] || exit 2
    ;;

esac
