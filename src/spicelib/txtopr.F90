!$Procedure TXTOPR ( Text file, open for read )

      SUBROUTINE TXTOPR ( FNAME, UNIT )

! $ Abstract
!
!     Open a text file for read access.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     TEXT
!
! $ Keywords
!
!     FILES
!     TEXT
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FNAME
      INTEGER               UNIT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of file.
!     UNIT       O   Logical unit.
!
! $ Detailed_Input
!
!     FNAME    is the name of the text file to be opened.
!
! $ Detailed_Output
!
!     UNIT     is the logical unit connected to the opened file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If FNAME is a blank string, the error SPICE(BLANKFILENAME) is
!         signaled.
!
!     2)  If the file cannot be opened, the error SPICE(FILEOPENFAILED)
!         is signaled.
!
! $ Files
!
!     See FNAME and UNIT above.
!
! $ Particulars
!
!     In SPICELIB, a text file is formatted and sequential and may
!     contain only printable ASCII characters and blanks (ASCII 32-127).
!     When printing a text file, records are single spaced; the first
!     character will not be interpreted as a carriage control character.
!
!     TXTOPR opens an existing text file for read access and makes use
!     of the SPICELIB mechanism for coordinating use of logical units.
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
!            STATUS = 'OLD'
!            IOSTAT =  IOSTAT
!
!     In addition, the statement will include
!
!            CARRIAGECONTROL = 'LIST'
!            READONLY
!
!     for the Vax and the OS X Absoft compiler, or
!
!            MODE            = 'READ'
!
!     for the IBM pc.
!
! $ Examples
!
!     The following example reads the first line from an input file,
!     'INPUT.TXT', and writes it to an output file, 'OUTPUT.TXT'.
!
!        CALL TXTOPR ( 'INPUT.TXT',  IN  )
!        CALL TXTOPN ( 'OUTPUT.TXT', OUT )
!
!        READ  ( IN,  FMT='(A)' ) LINE
!        WRITE ( OUT, FMT='(A)' ) LINE
!
!        CLOSE ( IN  )
!        CLOSE ( OUT )
!
! $ Restrictions
!
!     1)  The file, FNAME, must exist prior to calling TXTOPR.
!
! $ Literature_References
!
!     [1]  "Lahey F77L EM/32 FORTRAN Language Reference Manual", p.145.
!
!     [2]  "Language Reference Manual", Absoft Fortran V3.2, page 7-12
!          (for the NeXT), 1993.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     J.E. McLean        (JPL)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.27.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 2.26.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
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
!-    SPICELIB Version 2.0.6, 24-APR-2003 (EDW)
!
!        Added MAC-OSX-F77 to the list of platforms
!        that require READONLY to read write protected
!        kernels.
!
!-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 2.0.3, 16-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        References to the PC-LINUX environment were added.
!
!-    SPICELIB Version 1.3.0, 11-NOV-1993 (HAN)
!
!        Module was updated for the Silicon Graphics, DEC Alpha-OSF/1,
!        and NeXT platforms.
!
!-    SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 13-NOV-1991 (MJS)
!
!        Module updated to allow portability to the Lahey F77L EM/32
!        FORTRAN V 4.0 environment.
!
!-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
!
!-&


! $ Index_Entries
!
!     text file open for read
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        References to the PC-LINUX environment were added.
!
!-    SPICELIB Version 1.3.0, 11-NOV-1993 (HAN)
!
!        Module was updated for the Silicon Graphics, DEC Alpha-OSF/1,
!        and NeXT platforms.
!
!-    SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 1.1.0, 13-NOV-1991 (MJS)
!
!        Module updated to allow portability to the Lahey F77L EM/32
!        FORTRAN V 4.0 environment.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

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
         CALL CHKIN ( 'TXTOPR' )
      END IF

      IF ( FNAME .EQ. ' ' ) THEN
         CALL SETMSG ( 'A blank string is unacceptable as a file name' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'                          )
         CALL CHKOUT ( 'TXTOPR'                                        )
         RETURN
      END IF

      CALL GETLUN ( UNIT )

      OPEN ( UNIT            =  UNIT, &
             FILE            =  FNAME, &
             FORM            = 'FORMATTED', &
             ACCESS          = 'SEQUENTIAL', &
             STATUS          = 'OLD', &
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
             MODE            = 'READ', &
#else
#endif
             IOSTAT          =  IOSTAT      )

      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Could not open file #. IOSTAT was #. ' )
         CALL ERRCH  ( '#', FNAME                              )
         CALL ERRINT ( '#', IOSTAT                             )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                 )
         CALL CHKOUT ( 'TXTOPR'                                )
         RETURN
      END IF

      CALL CHKOUT ( 'TXTOPR' )
      RETURN
      END
