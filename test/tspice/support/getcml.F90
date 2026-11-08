!$Procedure      GETCML ( Get the command line )

      SUBROUTINE GETCML ( LINE )

! $ Abstract
!
!     Return command line arguments in a single string.
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


      CHARACTER*(*)         LINE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     LINE       O   The command line arguments.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     LINE      is the string containing the command line arguments.
!               The actual command is not contained in the string.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!     This routine does not participate in the SPICELIB error handling
!     mechanism.
!
!     Any other exceptions are unknown at this time.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The source code for platforms for which we have no documentation
!     was created as a result of porting the Sun OS/Sun Fortran version
!     of the routine to detemine if it executed successfully on the
!     target platform. This is how we generated the code for the Sun
!     Solaris/Sun Fortran, Silicon Graphics IRIX/SGI Fortran, and
!     DEC Alpha OSF-1/DEC Fortran platforms.
!
! $ Examples
!
!     If inputs is a Fortran program, and the following command is
!     executed from a command line,
!
!       % inputs this is the command line input
!
!     the following string will be returned by GETCML:
!
!       this is the command line input
!
!     The Mac Classic version creates a window into which the user
!     inputs the command line arguments (if any).
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     1. "FORTRAN/9000 Reference HP 9000 Series 700 Computers,"
!        First Edition, June 1991, Hewlett Packard Company,
!        pages 9-29, 9-30, 18-2 through 18-5.
!
!     2. "Language Guide," accompaniment to Microsoft FORTRAN
!        PowerStation Compiler, version 1.0, 1993, pages 296-297,
!        401-402.
!
!     3. "Absoft FORTRAN 77 Compatibility Libraries,"
!        Absoft Corporation, 1991-1992, pages 3-10 and 3-14.
!
!     4. "Lahey F77L3-EM/32 FORTRAN Programmer's Reference," Lahey
!        Computer Systems, Inc., Revision C, January 1992, page 35.
!
! $ Author_and_Institution
!
!     K.R. Gehringer (JPL)
!     H.A. Neilan    (JPL)
!     M.J. Spencer   (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.16.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 6.15.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 6.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 6.13.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 6.12.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 6.11.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 6.10.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 6.9.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 6.8.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 6.7.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 6.6.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 6.5.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 6.4.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 6.3.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 6.1.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 6.1.0, 14-NOV-2001 (EDW)
!
!        Add code to create an input window for arguments
!        under Mac Absoft (Classic) compiler. The enviroment
!        ID is MACPPC.
!
!-    SPICELIB Version 6.0.0, 14-OCT-1999 (WLT)
!
!        The VMS environment was modified to return the command
!        line after applying LCASE.
!
!-    SPICELIB Version 5.0.3, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitely given.  New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitely given.  New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitely given.  Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 5.0.0, 24-NOV-1998 (WLT)
!
!        Added references for the MAC.
!
!-    SPICELIB Version 4.0.0, 05-APR-1998 (NJB)
!
!        Added references for the PC-LINUX environment.  Added SAVE
!        statements for the variable FIRST.
!
!-    SPICELIB Version 3.2.0, 18-AUG-1995 (KRG) (HAN)
!
!        Added code for the PC-Lahey environment. Increased the size
!        of ARGLEN to 512 as a result of truncation problems on some
!        Unix platforms.
!
!-    SPICELIB Version 3.1.0, 5-JAN-1995 (HAN)
!
!        Removed Sun Solaris environment since it is now the same
!        as the Sun OS 4.1.x environment.
!        Removed DEC Alpha/OpenVMS environment since it is now the
!        same as the VAX environment.
!
!-    SPICELIB Version 3.0.0, 8-JUL-1994 (HAN)
!
!        Added code for the NeXT/Absoft Fortran, HP-UX/HP Fortran,
!        Silicon Graphics (SGI) IRIX/SGI Fortran,
!        Sun Solaris/Sun Fortran, and DEC Alpha OSF-1/DEC Fortran
!        environments.
!
!-    SPICELIB Version 2.0.0, 15-MAR-1994 (HAN) (MJS)
!
!        Added code for the HP and the PC/Microsoft Powerstation
!        environments.
!
!-    SPICELIB Version 1.0.0, 19-NOV-1992 (HAN)
!
!-&

! $ Index_Entries
!
!   get command line arguments
!
!-&



!
#if defined(SPICE_COMPILER_INTEL)
#else
!     Other functions
!
      INTEGER               IARGC

!
#endif
!     Local Variables
!
      INTEGER               ARGLEN
      PARAMETER           ( ARGLEN = 512 )

      CHARACTER*(ARGLEN)    ARGMNT

      INTEGER               HOWMNY
      INTEGER               I
#if defined(SPICE_COMPILER_INTEL)
      INTEGER*2             STATUS
#else
#endif

      LOGICAL               FIRST

      SAVE                  FIRST

      DATA                  FIRST  / .TRUE. /

!
#if defined(SPICE_COMPILER_INTEL)
!     Call the FORTRAN library function NARGS to determine how many
!     words are on the command line. This number includes the command
!     itself; we won't include that in the output string. Get the
!     words one at a time and construct the output string and we're
!     done.
#else
!     Call the FORTRAN library function iargc to determine how many
!     words are on the command line. Then, get the arguments one at a
!     time and construct the output string.
#endif
!
#if defined(SPICE_COMPILER_INTEL)
      HOWMNY = NARGS() - 1
#else
      HOWMNY = IARGC()
#endif
      I      = 1
      LINE   = ' '

      IF ( HOWMNY .EQ. 0 ) THEN
         RETURN
      END IF

      DO WHILE ( I .LE. HOWMNY )

#if defined(SPICE_COMPILER_INTEL)
         CALL GETARG ( I, ARGMNT, STATUS )
#else
         CALL GETARG ( I, ARGMNT )
#endif

         IF ( FIRST ) THEN
            CALL SUFFIX ( ARGMNT, 0, LINE )
            FIRST = .FALSE.
         ELSE
            CALL SUFFIX ( ARGMNT, 1, LINE )
         END IF

         I = I + 1

      END DO

      RETURN
      END
