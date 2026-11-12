!$Procedure      ZZGETENV ( Get environment variable value. )

      SUBROUTINE ZZGETENV ( ENVVAR, VALUE )
      use pepper_legacy

! $ Abstract
!
!     Get the value of a specified environment variable or VAX DCL
!     symbol, if it exists.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      CHARACTER*(*)         ENVVAR
      CHARACTER*(*)         VALUE

!
!     Length of an environment variable or DCL symbol name.
!
      INTEGER               ENVLEN
      PARAMETER           ( ENVLEN = 32 )
!
!     Length of an environment variable or DCL symbol value.
!
      INTEGER               VALLEN
      PARAMETER           ( VALLEN = 255 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ENVVAR     I   The name of the environment variable or symbol.
!     VALUE      O   The value of the environment variable or symbol.
!     ENVLEN     P   Maximum length of an environment variable or symbol
!     VALLEN     P   Maximum length of a value.
!
! $ Detailed_Input
!
!     ENVVAR   This is the name of the environment variable, or DCL
!              symbol, whose value is desired. The significant, i.e.,
!              nonblank, portion of the environment variable, or DCL
!              symbol, name may be at most ENVLEN characters in length
!              and may not contain embedded blanks.
!
!              A standard convention used for naming environment
!              variables is to use only the upper case characters
!              'A' - 'Z', the digits '0' - '9', and the underscore
!              character '_', in the names. We do not enforce this
!              convention but we strongly recommend its use for
!              interface consistency across heterogeneous computing
!              environments.
!
!              For a particular operating system and compiler the
!              maximum allowed length of an environment variable name
!              may be less than ENVLEN. Consult the appropriate
!              operating system and/or compiler manuals for details.
!
! $ Detailed_Output
!
!     VALUE    This is the value obtained for the environment variable
!              ENVVAR if it is defined. The result will be left
!              justified on output.
!
!              If any of the following are true:
!
!                 1) a value for the environment variable cannot be
!                    obtained,
!
!                 2) the significant portion of ENVVAR contains
!                    embedded blanks,
!
!                 3) the input ENVVAR is blank,
!
!                 4) The input ENVVAR contains characters other than
!                    the upper case characters 'A' - 'Z', the digits
!                    '0' - '9', and the underscore '_',
!
!                 5) The value for the environment variable is too long
!                    to fit in the available space,
!
!              then VALUE will be blank.
!
! $ Parameters
!
!     ENVLEN   The maximum allowed length of an environment variable
!              or DCL symbol name.
!
!     VALLEN   The maximum allowed length of an environment variable
!              or DCL symbol value.
!
! $ Exceptions
!
!     None.
!
!     1) If a value for the environment variable cannot be obtained,
!        a blank string will be returned.
!
!     2) If the significant portion of ENVVAR contains embedded blanks,
!        a blank string will be returned.
!
!     3) If the input ENVVAR is blank, a blank string will be returned.
!
!     4) If the value for the environment variable is too long to fit
!        in the available space, a blank string will be returned.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     PEPPER: call GET_ENVIRONMENT_VARIABLE (standard Fortran; SPICELIB
!     called GETENV with GFORTRAN and GETENVQQ with Intel Fortran).
!     Given the name of an environment variable, it stores in VALUE the
!     value of the specified
!     environment variable or a blank string if an error occurs.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are WIN-NT
!
!-    SPICELIB Version 2.0.3, 21-SEP-1999 (NJB)
!
!        CSPICE environments were added.  Some typos were corrected.
!
!-    Beta Version 2.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    Beta Version 2.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    Beta Version 2.0.0, 05-APR-1998 (NJB)
!
!        Added the PC-LINUX environment.
!
!-    Beta Version 1.0.0, 31-MAY-1996 (KRG)
!
!-&

! $ Index_Entries
!
!      get environment variable value
!
!-&

!
!     SPICELIB functions
!

!
!     Local variables
!
      CHARACTER*(VALLEN)    MYVALU
      INTEGER               LENGTH
      INTEGER               STATUS
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZGETENV' )
      END IF
!
!     We do three things:
!
!        1) Check to see if the input is blank.
!        2) Attempt to get the value.
!        3) If we got a nonblank value, see if it will fit in the
!           space provided.
!
      IF ( ENVVAR .EQ. ' ' ) THEN

         MYVALU = ' '

      ELSE
!
!        PEPPER: GET_ENVIRONMENT_VARIABLE in place of GETENV and
!        GETENVQQ. A value that does not fit in VALUE is returned
!        blank, as before.
!
         CALL GET_ENVIRONMENT_VARIABLE ( ENVVAR, MYVALU, LENGTH, STATUS )

         IF (      ( STATUS .NE. 0 )                                    &
              .OR. ( LENGTH .GT. LEN (VALUE) ) ) THEN
            MYVALU = ' '
         END IF

      END IF

      VALUE = MYVALU

      CALL CHKOUT ( 'ZZGETENV' )
      RETURN
      END
