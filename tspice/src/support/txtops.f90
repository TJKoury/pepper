!$Procedure      TXTOPS ( Text file, open scratch )

      SUBROUTINE TXTOPS (  UNIT )
      use pepper_legacy

! $ Abstract
!
!     Open a scratch text file for subsequent write access.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FILES
!     TEXT
!
! $ Declarations

      INTEGER               UNIT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     UNIT       O   Logical unit.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     UNIT           is the logical unit connected to the opened
!                    scratch file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the file cannot be opened, the error SPICE(FILEOPENFAILED)
!        is signaled.
!
! $ Files
!
!     See UNIT above.
!
! $ Particulars
!
!     In SPICELIB, a text file is formatted and sequential and may
!     contain only printable ASCII characters and blanks (ASCII 32-127).
!     When printing a text file, records are single spaced; the first
!     character will not be interpreted as a carriage control character.
!
!     TXTOPS opens a scratch text file and makes use of the SPICELIB
!     mechanism for coordinating the use of logical units.
!
!     System Dependencies
!     ===================
!
!     The open statement will include the following keyword = value
!     pairs:
!
!            UNIT   =  UNIT
!            FILE   =  FNAME
!            FORM   = 'FORMATTED'
!            ACCESS = 'SEQUENTIAL'
!            STATUS = 'SCRATCH'
!            IOSTAT =  IOSTAT
!
!     In addition, the statement will include
!
!            CARRIAGECONTROL = 'LIST'
!
!     for the Vax and Macintosh.
!
! $ Examples
!
!     The following example reads a line from an input file,
!     'INPUT.TXT', and writes it to an output file, 'OUTPUT.TXT'.
!
!        CALL TXTOPR ( 'INPUT.TXT',  IN  )
!        CALL TXTOPS ( OUT )
!
!        READ  ( IN,  FMT='(A)' ) LINE
!        WRITE ( OUT, FMT='(A)' ) LINE
!
!        CLOSE ( IN  )
!        CLOSE ( OUT )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     1. "Absoft FORTRAN 77 Language Reference Manual", page 7-12 for
!        the NeXT.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.18.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 1.17.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 1.16.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 1.15.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 1.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.13.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 1.10.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 1.9.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 1.8.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.7.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.6.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 1.5.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 1.4.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.3.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.2.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 1.1.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 1.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are WIN-NT
!
!-    SPICELIB Version 1.0.3, 21-SEP-1999 (NJB)
!
!        CSPICE and PC-LINUX environment lines were added.  Some
!        typos were corrected.
!
!-    SPICELIB Version 1.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 1.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.0.0, 20-FEB-1996 (WLT)
!
!
!-&

! $ Index_Entries
!
!     text file open scratch
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.0.0, 20-FEB-1996 (WLT)
!
!        This routine is basically a simple tweak of TXTOPN.
!        It replaces txtopn that Mike Spencer wrote because
!        the master file could not be located.
!
!-&


!
!     SPICELIB functions
!

!
!     Local variables
!
      INTEGER               IOSTAT



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TXTOPS' )
      END IF


      CALL GETLUN ( UNIT )

      OPEN ( UNIT            =  UNIT, &
             FORM            = 'FORMATTED', &
             ACCESS          = 'SEQUENTIAL', &
             STATUS          = 'SCRATCH', &
             IOSTAT          =  IOSTAT      )

      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Could not scratch file. IOSTAT was #. ' )
         CALL ERRINT ( '#', IOSTAT                             )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                 )
         CALL CHKOUT ( 'TXTOPS'                                )
         RETURN
      END IF

      CALL CHKOUT ( 'TXTOPS' )
      RETURN
      END

