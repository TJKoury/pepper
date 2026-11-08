

PEPPER: Planetary Ephemerides, Positions, Pointing, Events & Rotations. A modern Fortran reimagining of SPICELIB.

This is the plan to convert SPICELIB into PEPPER:

1. First, we need to combine the spicelib code that NAIF releases in code archives for different platforms into one single codebase. Note: all we care about is src/spicelib, not the other tools in the archives. This will organize all the code in a unified structure, making it easier to manage and maintain. Platform-specific files will be placed in separate directories to keep the code modular and maintainable.
2. convert all the code from fixed-form to free-form Fortran.
3. Refactor the code to adopt modern Fortran practices, such as using modules, explicit interfaces, and allocatable arrays. This will improve code readability, maintainability, and performance.

At each stage, we must test the code to make sure we haven't broken anything. To do this we will use the NAIF test suite (tspice).

We will test on gfortran on mac, using FPM and a pixi environment. But we want the code to be portable and work across different platforms and compilers as well. The only platforms to consider are Windows (x64), Linux (x64), and Mac (arm64). The only compilers to consider are gfortran and intel (ifx).

we want to start with the latest SPICELIB relase (N0067)

### links:

spicelib archives for each platform/compiler: https://naif.jpl.nasa.gov/naif/toolkit_FORTRAN.html
tspice: https://naif.jpl.nasa.gov/pub/naif/misc/tspice/N0067/
