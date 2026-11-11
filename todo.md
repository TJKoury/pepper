todo notes

fix aliasing issues
remove line ending check for platform



* remove include files completely. replace with modules and parameters
* get rid of the 'statement functions' (e.g., in jul2gr)
* clean up code docstrings, remove unnecessary clutter
* consolidate duplicate parameters (ones defined in multiple fiels)
* inline !! comment docstrings for parameters
* remove legacy wrappers that are no longer needed
* can we remove the 'umbrella' routines that do nothing?
* remove superfluous 'return' statements at the end of procedures.
* initialize all type components to defaults values (0, .false., '', etc.)

* I don't this these are real constraints:

    #if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
        parameter            ( filen = 128 )
    #else
        parameter            ( filen = 255 )
    #endif

    note: integer(ip), parameter :: filen = 255
    is in lots of other places with no platform checks.

* note: I think there are aliasing bugs with ifx with character strings.
* need to test with ifx in the CI.

* start to modernize/improve the actual algorithms and code from spicelib
  * remove arbitrary limitations that were due to static arrays in fortran 77. Examples: maximum number of kernels, maximum length of strings, maximum number of bodies, maximum number of frames, and so on. (inventory and plan: limits.md)
  * improve performance of core routines using modern Fortran features (e.g., array operations, parallelization)
  * improve old fashioned code patterns by using modern Fortran constructs
  * goal is to make the code more maintainable, efficient, and compatible with modern Fortran standards
  * further refactor code to improve readability and maintainability
  * remove functions that are no longer necessary with modern Fortran features. Example: trimming strings, etc. use intrinsic functions instead.

* longer-term items:
  * add JSON kernel support [use json-fortran. see json spice repo for a python implementation]
  * add programatic frames and bodies [see other github repo]
  * splining of ephemeris and rotations? [bspline-fortran]
  * python interface (like spiceypy)
  * compare to the Rust implementation (ANISE)
* reloading a binary kernel after UNLOAD while another context still has the
  old unit open fails (GETFAT: SPICE(EXTERNALOPEN); gfortran connects a file
  to one unit only). Contexts close such units lazily (units_check); the
  instance cannot close them safely (NEWUNIT numbers are reused). See
  in_memory_kernels_design.md.
