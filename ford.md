---
project: PEPPER
summary: A modern Fortran, thread-safe reimagining of NAIF's SPICELIB (N0067).
author: Jacob Williams
src_dir: ./src
output_dir: ./doc
project_github: https://github.com/jacobwilliams/pepper
predocmark: >
docmark: !
docmark_alt: *
predocmark_alt: |
display: public
         protected
source: true
graph: true
search: true
preprocess: true
fpp_extensions: F90
macro: SPICE_OS_MACOS
       SPICE_COMPILER_GFORTRAN
extra_filetypes: inc !
md_extensions: markdown.extensions.toc
front_page_file: README.md
logo: ./media/logo.svg
---

PEPPER is a modern Fortran library derived from NAIF's SPICE Toolkit for
Fortran (SPICELIB, N0067), made thread safe: all state belongs to an instance
(`type(pepper_spice)`) or to a context (`type(pepper_context)`, one per
thread), and the procedures are type-bound procedures of the context with
SPICELIB's names and arguments (`call ctx%spkezr(...)`).

The documentation of each procedure is NAIF's: its abstract and argument
descriptions on its interface's page, and its full header (Particulars,
Examples, ...) at the top of its implementation, in the source pages of the
submodules (FORD documents separate module procedures through their
interfaces only). The procedures of the plain modules (pepper_arrays,
pepper_math, pepper_strings, ...) have their full header on their own pages.
