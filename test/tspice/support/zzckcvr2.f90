!$Procedure ZZCKCVR2 ( Private --- C-kernel segment coverage, type 02 )

      SUBROUTINE ZZCKCVR2 ( HANDLE, ARRBEG, ARREND, SCHEDL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 02 C-kernel segment.
!
! $ Required_Reading
!
!     CK
!     DAF
!
! $ Keywords
!
!     CK
!     UTILITY
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               HANDLE
      INTEGER               ARRBEG
      INTEGER               ARREND
      DOUBLE PRECISION      SCHEDL ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a C-kernel open for read access
!     ARRBEG     I   Beginning DAF address
!     ARREND     I   Ending DAF address
!     SCHEDL    I/O  An initialized window/schedule of interval
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     ARRBEG     is the beginning address of a type 02 segment
!
!     ARREND     is the ending address of a type 02 segment.
!
!     SCHEDL     is a schedule (window) of intervals, to which the
!                intervals of coverage for this segment will be added.
!
! $ Detailed_Output
!
!     SCHEDL     the input schedule updated to include the intervals
!                of coverage for this segment.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This routine reads the contents of the file associated with
!     HANDLE to locate coverage intervals.
!
! $ Exceptions
!
!     Routines in the call tree of this routine may signal errors
!     if insufficient room in SCHEDL exists or other error
!     conditions relating to file access arise.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 02 C-kernel segment.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SUPPORT Version 2.1.0, 13-FEB-2003 (BVS)
!
!        Replaced MAX with MIN in the assignment of GET. This bug
!        caused the routine either to look beyond the end of the
!        start/stop time blocks of the segment (for NREC < BSIZE) or to
!        attempt to fill in internal buffers with more data than they
!        were declared to hold (for NREC > BSIZE.)
!
!-    SUPPORT Version 2.0.0, 27-AUG-2002 (FST)
!
!        Updated this routine to use DAFGDA instead of DAFRDA.
!        This allows the module to process non-native kernels.
!
!        Header and code clean up for delivery to SUPPORT.
!
!-    SUPPORT Version 1.0.0, 14-Feb-2000 (WLT)
!
!        Happy Valentine's Day.
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               PSIZE
      PARAMETER           ( PSIZE = 8 )

      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 100 )

!
!     Local Variables
!
      DOUBLE PRECISION      FIRST ( BSIZE )
      DOUBLE PRECISION      LAST  ( BSIZE )

      INTEGER               ARRSIZ
      INTEGER               BEGAT
      INTEGER               ENDAT
      INTEGER               GET
      INTEGER               GOT
      INTEGER               I
      INTEGER               NREC

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCVR2' )
      END IF

!
!     Determine the size of the array and the number of records
!     in it.
!
      ARRSIZ = ARREND - ARRBEG + 1
      NREC   = NINT (  ( 100.D0 * (DBLE(ARRSIZ)) + 1.D0 ) / 1001.D0  )

!
!     The variable GOT tells us how many time endpoints we've
!     gotten so far.
!
      GOT = 0

      DO WHILE ( GOT .LT. NREC )

         GET = MIN ( BSIZE, NREC - GOT )

         BEGAT = ARRBEG + NREC*PSIZE + GOT
         ENDAT = ARRBEG + NREC*PSIZE + NREC + GOT

!
!        Retrieve the list next list of windows.
!
         CALL DAFGDA ( HANDLE, BEGAT, BEGAT + GET - 1, FIRST )
         CALL DAFGDA ( HANDLE, ENDAT, ENDAT + GET - 1, LAST  )

!
!        Insert the coverage intervals into the schedule.
!
         DO I = 1, GET
            CALL WNINSD ( FIRST(I), LAST(I), SCHEDL )
         END DO

         GOT = GOT + GET

      END DO

      CALL CHKOUT ( 'ZZCKCVR2' )
      RETURN
      END
