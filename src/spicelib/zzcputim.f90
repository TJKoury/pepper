!$Procedure ZZCPUTIM ( CPU Time )

      SUBROUTINE ZZCPUTIM ( TVEC )

! $ Abstract
!
!     Fetch the current CPU date and time and store the result
!     as a double precision 6-vector.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TIME
!     UTILITY
!
! $ Declarations

      DOUBLE PRECISION      TVEC ( 6 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TVEC       O   contains year, month, day, hours, minutes, seconds
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     TVEC       is a 6-vector containing the current system time.
!                The various components have the following meaning
!
!                   TVEC(1)  --- current calendar year
!                   TVEC(2)  --- current month
!                   TVEC(3)  --- current day of month
!                   TVEC(4)  --- current hour. Hours have a range from
!                                0 to 23.  0 corresponds to system
!                                midnight.
!                   TVEC(5)  --- current minutes
!                   TVEC(6)  --- current seconds and fraction of a
!                                second (provided the system clock
!                                has sufficiently fine granularity
!                                to provide greater precision).
!
!                The first 5 components will be double precision
!                integers.  (They truncate without change.)
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine returns the components of the current date and
!     time as determined by the system clock.
!
!
! $ Examples
!
!     Example 1.
!
!     The following routine illustrates how you might use ZZCPUTIM
!     to generate a "time stamp" that might be used to tag data
!     you plan to write to a file.
!
!           SUBROUTINE TSTAMP ( STAMP )
!
!           CHARACTER*(15)        STAMP
!
!           DOUBLE PRECISION      TVEC ( 6 )
!
!     C
!     C     First fetch the current system time.
!     C
!           CALL ZZCPUTIM ( TVEC )
!
!     C
!     C     now form a time stamp of the form YYYYYMMDDhhmmss
!     C
!           CALL DPFMT ( TVEC(1), '0YYYY', STAMP(1:5)   )
!           CALL DPFMT ( TVEC(2), '0M',    STAMP(6:7)   )
!           CALL DPFMT ( TVEC(3), '0D',    STAMP(8:9)   )
!           CALL DPFMT ( TVEC(4), '0h',    STAMP(10:11) )
!           CALL DPFMT ( TVEC(5), '0m',    STAMP(12:13) )
!           CALL DPFMT ( TVEC(6), '0s',    STAMP(14:15) )
!
!           RETURN
!
!     Example 2.
!
!     The following code illustrates how you might use this routine
!     to perform a crude estimate of the running time of the
!     SPICELIB routine VDOT. (This assumes that the program runs
!     during a single calendar day and that there is no competition
!     between users for system resources.)
!
!           DOUBLE PRECISION      VDOT
!
!           DOUBLE PRECISION      AVE
!           DOUBLE PRECISION      SINCE0( 2    )
!           DOUBLE PRECISION      TVEC  ( 6, 3 )
!           DOUBLE PRECISION      V1    ( 3    )
!           DOUBLE PRECISION      V2    ( 3    )
!           DOUBLE PRECISION      X
!
!           INTEGER               I
!           INTEGER               TRIALS
!           PARAMETER           ( TRIALS = 100000 )
!
!     C
!     C     Give the vectors some values (these seem as good as
!     C     anything else that comes to mind).
!     C
!           V1(1) = 1.0D0
!           V1(2) = 2.0D0
!           V1(3) = 3.0D0
!
!           V2(1) = 10.0D0
!           V2(2) = 20.0D0
!           V3(3) = 30.0D0
!
!     C
!     C     Perform the loop twice, once with one call to VDOT, the
!     C     second with two calls to VDOT.
!     C     The first will require
!     C
!     C        LOOP_OVERHEAD + TRIALS*TIME_FOR_VDOT
!     C
!     C     The second will require
!     C
!     C        LOOP_OVERHEAD + 2*TRIALS*TIME_FOR_VDOT
!     C
!     C     The difference of the two, will give us
!     C
!     C        TRIALS*TIME_FOR_VDOT
!     C
!
!     C
!     C     get the current system time.
!     C
!           CALL ZZCPUTIM ( TVEC(1,1) )
!
!           DO I = 1, TRIALS
!              X = VDOT( V1, V2 )
!           END DO
!
!     C
!     C     Get the time after the first pass.
!     C
!           CALL ZZCPUTIM ( TVEC(1,2)
!
!           DO I = 1, TRIALS
!              X = VDOT( V1, V2 )
!              X = VDOT( V1, V2 )
!           END DO
!
!      C
!      C    Get the time after the second pass.
!      C
!           CALL ZZCPUTIM ( TVEC(1,3)
!
!
!      C
!      C    Now compute seconds past midnight at each clock reading.
!      C
!           DO I = 1, 3
!
!              SINCE0(I) = TVEC(4,I) * 3600.0D0
!          .             + TVEC(5,I) *   60.0D0
!          .             + TVEC(6,I)
!           END DO
!
!      C
!      C    The time for the first  pass is SINCE0(2) - SINCE0(1)
!      C    The time for the second pass is SINCE0(3) - SINCE0(2)
!      C
!      C    The difference between these divided by the number of
!      C    trials is the average running time.
!      C
!           AVE = (SINCE0(3) - 2*SINCE0(2) - SINCE0(1)) / DBLE(TRIALS)
!
!           WRITE (*,*) 'The average running time for VDOT is ', AVE
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     1) "Absoft FORTRAN 77 Compatibility Libraries", 1992,
!        Chapter 2, Chapter 3, pages 3-14 and 3-16.
!
!     2) "Language Systems FORTRAN 3.0 Reference Manual", October 1991
!        Chapter 12, pages 214 and 218.
!
!     3) Other UNIX boxes, we used the commands "man idate" and
!        "man itime".
!
!     4) "Programming VAX FORTRAN", "Fortran Language Summary",
!        Appendix D, September 1984, D-39 and D-41.
!
!     5) For the SGI, we use a small C language program to obtain the
!        date and time. This avoids ambiguities with the two digit year
!        returned by idate and avoids a name collision with ltime, the
!        SPICELIB light time subroutine. The C language program is in
!        the file loctim.c and is given the symbol loctim_ as its name.
!
!     6) "LF 95 Language Reference", Lahey Computer Systems Inc.
!
!     7) "The Intel Fortran Reference, Document Number: 253261-003"
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     K.R. Gehringer (JPL)
!     H.A. Neilan    (JPL)
!     B.V. Semenov   (JPL)
!     M.J. Spencer   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 7.14.0, 17-OCT-2021 (NJB) (BVS)
!
!        Added C* prefix to all executables lines in the MACPPC
!        block to silence unbalanced CHKIN/CHKOUT complaints.
!
!        Moved declaration of RETURN to each of the environment
!        blocks to silence declared-not-used complaints from f2c.
!
!        Corrected typos in comments.
!
!-    Beta Version 7.13.0, 02-APR-2014 (BVS)
!
!        Changed PC-CYGWIN-GFORTRAN and PC-CYGWIN-64BIT-GFORTRAN
!        to be like other GFORTRAN environments.
!
!-    Beta Version 7.12.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    Beta Version 7.11.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    Beta Version 7.10.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    Beta Version 7.9.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    Beta Version 7.8.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    Beta Version 7.7.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    Beta Version 7.6.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    Beta Version 7.5.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    Beta Version 7.4.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    Beta Version 7.3.0, 03-DEC-2008 (EDW)
!
!        Updated for PC-LINUX-GFORTRAN MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version  7.2.1, 07-SEP-2007 (EDW)
!
!        Renamed CPUTIM to ZZCPUTIM; moved routine into
!        SPICELIB from SUPPORT.
!
!-    SPICELIB Version  7.2.0, 26-NOV-2006 (EDW)
!
!        Added support for ifort compilers, OS X and Windows.
!
!-    SPICELIB Version  7.1.0, 10-NOV-2005 (BVS)
!
!        Fixed duplicate arguments in RMAIND calls.
!
!-    SPICELIB Version  7.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version  7.0.4, 27-DEC-2001 (EDW) (FST)
!
!        MACPPC environment updated to reflect the use of Absoft
!        compiler. It now uses the GetTime call to return
!        a DateTimeRec record (structure). The CPU time returns as
!        components of the DateTimeRec record.
!
!        PC-LAHEY environment updated to reflect the use of the new
!        LF95 compiler.
!
!-    SPICELIB Version  7.0.3, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are WIN-NT
!
!-    SPICELIB Version  7.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version  7.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version  7.0.0, 10-JUN-1998 (WLT)
!
!        Both the MAC and VAX versions included code to deal
!        with 2-digit years.  This code was replaced with a
!        call to TEXPYR so that two-digit years are handled
!        uniformly.
!
!        Also when the year 2068 rolls around, future NAIF
!        programmers can simply modify TEXPYR to change the default
!        century and code will continue to work.
!
!-    SPICELIB Version  6.0.0, 05-APR-1998 (NJB)
!
!        Added support for the PC-Linux environment.  The extant
!        SGI code sufficed.
!
!        Added SAVE statement for Mac/PPC environment.  Removed
!        non-printing characters.  Shortened one long line.
!
!-    SPICELIB Version  5.0.0, 16-FEB-1996 (KRG)
!
!        Generally just cleaned things up and rearranged the code for
!        each environment so that it was contiguous, to make it easier
!        to add new environments if necessary.
!
!        Removed VMS library dependencies from the NeXT code.
!
!        Updated and simplified the SGI code. THis now uses a small C
!        language program to obtain the system time and date.
!
!-    SPICELIB Version  4.0.0, 28-SEP-1995 (HAN)
!
!       Added EXTERNAL declarations required for the DEC-OSF1
!       platform.
!
!-    SPICELIB Version 3.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 3.0.0, 16-MAY-1995 (HAN)
!
!       Added the NeXT environment to the master source file. The
!       Absoft compiler requires system functions to be in lower case
!       and they must have a postpended underscore.
!
!       Added the Silicon Graphics and Macintosh environments to the
!       master source file. The DEC Alpha running OSF/1 is the same
!       as the Sun and HP.
!
!-    SPICELIB Version 2.0.0, 7-APR-1994 (HAN)
!
!       Created a master source file and added information about the
!       HP-UX/HP Fortran environment.
!
!-    SPICELIB Version 1.0.0, 1-MAY-1991 (NJB) (HAN) (MJS) (WLT)
!
!-&

! $ Index_Entries
!
!     get system date and time
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               DTIME (8)
      CHARACTER*(12)        RCLOCK(3)

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCPUTIM' )
      END IF

      CALL DATE_AND_TIME( RCLOCK(1), RCLOCK(2), RCLOCK(3), DTIME )
!
!     Let's pack all this information into our double precision TVEC
!     array.
!
      TVEC ( 1 ) = DBLE( DTIME(1) )
      TVEC ( 2 ) = DBLE( DTIME(2) )
      TVEC ( 3 ) = DBLE( DTIME(3) )
      TVEC ( 4 ) = DBLE( DTIME(5) )
      TVEC ( 5 ) = DBLE( DTIME(6) )
      TVEC ( 6 ) = DBLE( DTIME(7) ) + DBLE( DTIME(8) )/1000.D0

!
!     That's it.
!
      CALL CHKOUT ( 'ZZCPUTIM' )
      RETURN

      END
