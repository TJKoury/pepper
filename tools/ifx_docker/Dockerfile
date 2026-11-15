# Intel Fortran (ifx) for PEPPER, as in the CI job "ifx (ubuntu-latest)":
# Ubuntu, Intel's apt package of the compiler, fpm, python3. Built and used
# by tools/ifx_docker/ifx.sh (see the README there).
#
# x86-64 only (Intel's compiler): on an ARM machine (Apple silicon) Docker
# runs it under emulation, slowly but correctly.
FROM ubuntu:24.04

# keep these the same as .github/workflows/ci.yml
ARG IFX_VERSION=2026.2
ARG FPM_VERSION=0.13.0

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install -y --no-install-recommends wget gpg ca-certificates python3 rsync git make gcc g++ \
 && wget -qO- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
      | gpg --dearmor > /usr/share/keyrings/oneapi-archive-keyring.gpg \
 && echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" \
      > /etc/apt/sources.list.d/oneAPI.list \
 && apt-get update \
 && apt-get install -y --no-install-recommends "intel-oneapi-compiler-fortran-${IFX_VERSION}" \
 && rm -rf /opt/intel/oneapi/mpi /opt/intel/oneapi/debugger /opt/intel/oneapi/documentation \
           /var/lib/apt/lists/*

RUN wget -q "https://github.com/fortran-lang/fpm/releases/download/v${FPM_VERSION}/fpm-${FPM_VERSION}-linux-x86_64-gcc-12" \
      -O /usr/local/bin/fpm \
 && chmod +x /usr/local/bin/fpm

ENV FPM_FC=ifx
WORKDIR /work
