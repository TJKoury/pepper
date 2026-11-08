#if !defined(SPICE_OS_MACOS) && !defined(SPICE_OS_LINUX) && \
    !defined(SPICE_OS_WINDOWS) && !defined(SPICE_OS_CYGWIN)
#error "No SPICE_OS_* macro defined (see the platform feature in fpm.toml)"
#endif
#if !defined(SPICE_COMPILER_GFORTRAN) && !defined(SPICE_COMPILER_INTEL)
#error "No SPICE_COMPILER_* macro defined (see the platform feature in fpm.toml)"
#endif
!$Procedure ZZPLATFM ( Private --- Get platform attributes )

      subroutine zzplatfm ( key, value )

! $ Abstract
!
!     Return platform ID and various attributes of the intended
!     environment
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      character*(*)         key
      character*(*)         value

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     KEY        I   String indicating what information to return.
!     VALUE      O   String containing the requested information.
!
! $ Detailed_Input
!
!     KEY         is a string value that indicates which platform
!                 specific information is desired.  Acceptable inputs
!                 are:
!
!                    'SYSTEM'      -  System Identification String
!                    'O/S'         -  Operating System or Environment
!                    'COMPILER'    -  NAIF Supported Compiler
!                    'FILE_FORMAT' -  Native Binary File Format
!                    'TEXT_FORMAT' -  Native Text File Format
!                    'READS_BFF'   -  List of supported binary file
!                                     formats.
!
!                 Note: The comparison is case-insensitive, and the
!                       supplied value must fit into a string buffer
!                       KYSIZE characters in length.
!
! $ Detailed_Output
!
!     VALUE       is the string that holds the information requested
!                 by the input string KEY. VALUE must be able to
!                 contain the maximum number of characters returned
!                 by this routine, WDSIZE, or truncation will occur.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the KEY is invalid, then VALUE is set to the value
!        stored in the character string parameter DEFRPY defined
!        below.
!
!     2) If VALUE is not large enough to contain the requested
!        KEY's value, then truncation will occur.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves to identify the platform and compiler
!     used in creating SPICELIB.  It is provided so that routines
!     and programs can make run-time decisions based upon the
!     ambient Fortran environment.
!
!     Operating Systems:
!
!        This routine is now aware of the operating systems for which
!        the code is intended for compilation.  In some cases this may
!        be more than one operating system, particularly in the case
!        of the PC.
!
!     Binary File Format:
!
!        This routine now adds the capability to return at run time
!        the binary file architecture that is native to the system.
!
!     Text File Format:
!
!        This routine now has the capability to return at run time
!        the mechanism (or line terminator) used to delimit lines
!        in text files.  In most cases it will return common labels
!        for the special characters FORTRAN considers line break
!        indicators.
!
!     Binary File Formats Read:
!
!        This returns a space delimited list of all the binary file
!        formats this environment can read for DAF/DAS based files.
!
! $ Examples
!
!     This routine could be used so that a single routine
!     could be written that translates the meaning of IOSTAT values
!     that depend upon the machine and compiler.  At run time
!     the routine could look up the appropriate message to associate
!     with an IOSTAT value.
!
! $ Restrictions
!
!     1) VALUE must be large enough to contain the requested
!        information, otherwise truncation will occur.
!
!     2) The string passed in via the KEY input must be capable
!        of being properly copied into the KEYCPY buffer internal
!        to this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 28-NOV-2021 (BVS)
!
!        Updated for:
!
!           MAC-OSX-M1-64BIT-CLANG_C
!
!        environment.
!
!-    SPICELIB Version 3.0.0, 10-MAR-2014 (BVS)
!
!        Updated for:
!
!           PC-CYGWIN-64BIT-GCC_C
!           PC-CYGWIN-64BIT-GFORTRAN
!           PC-CYGWIN-GFORTRAN
!           PC-LINUX-64BIT-IFORT
!           SUN-SOLARIS-64BIT-INTEL
!
!        environments.
!
!-    SPICELIB Version 2.9.0, 16-MAR-2010 (EDW)
!
!        Updated for:
!
!           MAC-OSX-64BIT-INTEL_C
!           PC-64BIT-MS_C
!           SUN-SOLARIS-64BIT-NATIVE_C
!           MAC-OSX-64BIT-GFORTRAN
!           MAC-OSX-64BIT-IFORT
!           PC-LINUX-64BIT-GFORTRAN
!           PC-WINDOWS-64BIT-IFORT
!           SUN-SOLARIS-INTEL-64BIT-CC_C
!           SUN-SOLARIS-INTEL-CC_C
!           SUN-SOLARIS-INTEL
!
!        environments.
!
!-    SPICELIB Version 2.8.0, 12-JAN-2009 (EDW)
!
!        Added MAC-OSX-GFORTRAN PC-LINUX-GFORTRAN environments.
!
!-    SPICELIB Version 2.7.0, 19-FEB-2008 (BVS)
!
!        Added PC-LINUX-IFORT environment.
!
!-    SPICELIB Version 2.6.0, 15-NOV-2006 (NJB)
!
!        Added PC-WINDOWS-IFORT, MAC-OSX-IFORT, and MAC-OSX-INTEL_C
!        environments.
!
!-    SPICELIB Version 2.5.0, 21-FEB-2006 (NJB)
!
!        Added PC-LINUX-64BIT-GCC_C environment.
!
!        Corrected error in in-line comments:  changed keyword
!        from FILE_ARCH to FILE_FORMAT.
!
!-    SPICELIB Version 2.4.0, 14-MAR-2005 (BVS)
!
!        Added SUN-SOLARIS-64BIT-GCC_C environment.
!
!-    SPICELIB Version 2.3.0, 31-DEC-2004 (BVS)
!
!        Added PC CYGWIN environments. Changed OS for PC-LAHEY,
!        PC-DIGITAL, and PC-MS_C to 'MICROSOFT WINDOWS'.
!
!-    SPICELIB Version 2.2.0, 07-JUL-2002 (EDW)
!
!        Added Mac OS X Unix environment.
!
!-    SPICELIB Version 2.1.0, 06-FEB-2002 (FST)
!
!        Updated the 'TEXT_FORMAT' key value for the PC-LINUX_C
!        environment.  Previous versions incorrectly indicated
!        'CR-LF' as line terminators.
!
!-    SPICELIB Version 2.0.0, 05-JUN-2001 (FST)
!
!        Added TEXT_FORMAT and READS_BFF key/value pairs.
!        Modified the header slightly to improve word choice;
!        specifically binary file format replaces file
!        architecture.
!
!        Updated the compiler entry for the PC-LINUX
!        environment to refer to g77 as opposed to f2c.
!
!        Updated the compiler entry for the MACPPC environment.
!        This environment is now officially tied to Absoft
!        Fortran.
!
!        Updated the compiler entry for the PC-LAHEY environment.
!        The compiler for this environment is LF95, the latest
!        offering from Lahey.
!
!-    SPICELIB Version 1.0.0, 22-FEB-1999 (FST)
!
!-&


! $ Index_Entries
!
!     fetch platform dependent information
!
!-&

!
!     SPICELIB Functions
!
      integer               isrchc

!
!     Local Parameters
!

!
!     Array index parameters for each of the key/value pairs.
!
!     SYSTEM Index.
!
      integer               idxsys
      parameter           ( idxsys = 1 )

!
!     O/S Index.
!
      integer               idxos
      parameter           ( idxos  = idxsys + 1 )

!
!     Compiler Index.
!
      integer               idxcmp
      parameter           ( idxcmp = idxos  + 1 )

!
!     Binary File Format Index.
!
      integer               idxfmt
      parameter           ( idxfmt = idxcmp + 1 )

!
!     Text File Format Index
!
      integer               idxtfm
      parameter           ( idxtfm = idxfmt + 1 )

!
!     Reads Binary File Format Index.
!
      integer               idxrbf
      parameter           ( idxrbf = idxtfm + 1 )

!
!     Size of the buffer in which KEY is placed.
!
      integer               kysize
      parameter           ( kysize = 64 )

!
!     Maximum Size of local string returned in VALUE
!
      integer               wdsize
      parameter           ( wdsize = 32 )

!
!     Number of Platform Dependent values stored here.
!
      integer               nattr
      parameter           ( nattr  =  6 )

!
!     Default Reply in the event of an invalid KEY.
!
      character*(wdsize)     defrpy
      parameter            ( defrpy = '<UNAVAILABLE>' )

!
!     Local Variables
!

      character*(kysize)    keycpy
      character*(kysize)    keyval ( nattr )
      character*(wdsize)    attcpy ( 0 : nattr )

      integer               index

      logical               first

!
!     Saved Variables
!
      save                  first
      save                  attcpy
      save                  keyval

!
!     Data Statements
!
      data   first        /.true./

!
!     Make the initial assignments to the saved character array.
!
      if ( first ) then
!
!        Store the keys in the KEYVAL array.
!
         keyval (idxsys) = 'SYSTEM'
         keyval (idxos)  = 'O/S'
         keyval (idxcmp) = 'COMPILER'
         keyval (idxfmt) = 'FILE_FORMAT'
         keyval (idxtfm) = 'TEXT_FORMAT'
         keyval (idxrbf) = 'READS_BFF'

!
!        Set the default reply to be the zero'th component of ATTCPY.
!        This obviates IF-THEN-ELSE branching all together.
!
         attcpy (0) = defrpy

!
!        Platform/Environment specific assignments follow.
!





#if defined(SPICE_OS_MACOS)
         attcpy (idxsys) = 'INTEL MAC'
#else
         attcpy (idxsys) = 'PC'
#endif




#if defined(SPICE_OS_MACOS)
         attcpy (idxos) = 'MAC OS-X'
#elif defined(SPICE_OS_LINUX)
         attcpy (idxos) = 'LINUX'
#elif defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
         attcpy (idxos) = 'MICROSOFT WINDOWS'
#else
         attcpy (idxos) = 'MICROSOFT WINDOWS/CYGWIN'
#endif



#if defined(SPICE_COMPILER_INTEL)
         attcpy (idxcmp) = 'INTEL FORTRAN/64BIT'
#else
         attcpy (idxcmp) = 'GFORTRAN/64BIT'
#endif



         attcpy (idxfmt) = 'LTL-IEEE'



#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
         attcpy (idxtfm) = 'CR-LF'
#else
         attcpy (idxtfm) = 'LF'
#endif



         attcpy (idxrbf) = 'BIG-IEEE LTL-IEEE'

!
!        Don't execute these assignments again.
!
         first = .false.

      end if

!
!     Determine which KEY was passed in; do this by converting KEY
!     to the known member of the equivalence class of possible
!     values.
!
      call ucase ( key,    keycpy )
      call ljust ( keycpy, keycpy )

!
!     Find out which key we were given.  In the event that one of the
!     KEYVALs (or some equivalent string) was not passed in, ISRCHC
!     returns a value of zero.
!
      index = isrchc ( keycpy, nattr, keyval )
      value = attcpy ( index )

      return
      end
