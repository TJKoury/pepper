!$Procedure ZZCKCVR3 ( Private --- C-kernel segment coverage, type 03 )

      SUBROUTINE ZZCKCVR3 ( HANDLE, ARRBEG, ARREND, SCHEDL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 03 C-kernel segment.
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
!     ARRBEG     is the beginning address of a type 03 segment
!
!     ARREND     is the ending address of a type 03 segment.
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
!     1) The error SPICE(BADCK3SEGMENT) is signaled if the derived
!        segment length from ARRBEG and ARREND does not match
!        the possible lengths computed from the segment metadata.
!
!     2) Routines in the call tree of this routine may signal errors
!        if insufficient room in SCHEDL exists or other error
!        conditions relating to file access arise.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 03 C-kernel segment.
!
! $ Examples
!
!     See CKBRIEF's main driver.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 27-AUG-2002 (FST)
!
!        Updated this routine to use DAFGDA instead of DAFRDA.
!        This allows the module to process non-native kernels.
!
!-    SPICELIB Version 1.0.0, 14-Feb-2000 (WLT)
!
!        Happy Valentine's Day.
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               RETURN

!
!     Local Variables
!
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      BUFFER ( 2 )
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      START
      DOUBLE PRECISION      TICK

      INTEGER               INTAT
      INTEGER               INTBEG
      INTEGER               INVLS
      INTEGER               LSTINT
      INTEGER               LSTTIK
      INTEGER               NDIR
      INTEGER               NREC
      INTEGER               RSIZE
      INTEGER               TICKAT

      INTEGER               NAVSLN
      INTEGER               AVSLN
      INTEGER               SEGLEN

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCVR3' )
      END IF

!
!     Get the number of intervals and pointing instances ( records )
!     in this segment, and from that determine the number of respective
!     directory epochs.
!
      CALL DAFGDA ( HANDLE, ARREND - 1, ARREND, BUFFER )

      INVLS  = NINT ( BUFFER ( 1 ) )
      NREC   = NINT ( BUFFER ( 2 ) )
      NDIR   = (NREC-1)/ 100

!
!     Determine the size of the pointing packets.  This is dependent
!     on whether angular rate data is present in the segment or not.
!     We can determine this with the following computation:
!
!     Assume a record size of 4, i.e. no angular rate data.
!
      NAVSLN = 5*NREC + NDIR + INVLS + (INVLS-1)/100 + 2

!
!     Assume a record size of 7, i.e. angular rate data.
!
      AVSLN = 8*NREC + NDIR + INVLS + (INVLS-1)/100 + 2

!
!     Compute the actual length of the segment.
!
      SEGLEN = ARREND - ARRBEG + 1

      IF ( SEGLEN .EQ. NAVSLN ) THEN
         RSIZE = 4
      ELSE IF ( SEGLEN .EQ. AVSLN ) THEN
         RSIZE = 7
      ELSE
         CALL SETMSG ( 'The requested segment in file # reports a ' &
         //            'length of # d.p. numbers, but the metadata ' &
         //            'in the segment indicates the length must ' &
         //            'either be # (no angular rate data) or # ' &
         //            '(angular rate data). Perhaps the segment ' &
         //            'is not type 3?'                              )
         CALL ERRHAN ( '#', HANDLE                                   )
         CALL ERRINT ( '#', SEGLEN                                   )
         CALL ERRINT ( '#', NAVSLN                                   )
         CALL ERRINT ( '#', AVSLN                                    )
         CALL SIGERR ( 'SPICE(BADCK3SEGMENT)'                        )
         CALL CHKOUT ( 'ZZCKCVR3'                                    )
         RETURN
      END IF

!
!     Recall that the segment is layed out as:
!
!       +------------------------------+
!       |                              |
!       |  Pointing                    |
!       |                              |
!       +------------------------------+
!       |                        |
!       |  SCLK times            |
!       |                        |
!       +------------------------+
!       |                        |
!       |  SCLK directory        |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Interval start times  |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Start times directory |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Number of intervals   |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Number of pointing    |
!       |      instances         |
!       |                        |
!       +------------------------+
!

      TICKAT = ARRBEG + RSIZE*NREC
      LSTTIK = TICKAT + NREC - 1
      INTBEG = ARRBEG + RSIZE*NREC + NREC + NDIR
      INTAT  = INTBEG
      LSTINT = INTBEG + INVLS - 1

      CALL DAFGDA ( HANDLE, INTAT,  INTAT,   START )
      CALL DAFGDA ( HANDLE, TICKAT, TICKAT,  TICK  )

      DO WHILE ( TICK .LT. START .AND. TICKAT .LT. LSTTIK )
         TICKAT = TICKAT + 1
         CALL DAFGDA ( HANDLE, TICKAT, TICKAT, TICK )
      END DO

!
!     If we did not find a TICK at least as big as START, we can
!     just return now.
!
      IF ( TICK .LT. START ) THEN
         CALL CHKOUT ( 'ZZCKCVR3' )
         RETURN
      END IF

      DO WHILE (      INTAT  .LE. LSTINT &
                .AND. TICKAT .LE. LSTTIK )

!
!        At this point, we have an interval that begins at START
!        and ends at FINISH (unless of course we never found a "good"
!        TICK to start with.)
!
         BEGIN  = START

!
!        If the the start of the interval was the start of the LAST
!        interval available, we can short cut the remainder of the
!        reads.
!
         IF ( INTAT .EQ. LSTINT ) THEN
            CALL DAFGDA ( HANDLE, LSTTIK, LSTTIK, FINISH )
            CALL WNINSD ( START, FINISH, SCHEDL )
            CALL CHKOUT ( 'ZZCKCVR3' )
            RETURN
         END IF

!
!        This is the expected case.  Get the start of the next
!        interval.
!
         INTAT  = INTAT  + 1
         CALL DAFGDA ( HANDLE, INTAT, INTAT, START )

!
!        Read forward from the last tick until we reach the
!        START of the next interval or until we run out of TICKS.
!
         DO WHILE ( TICK .LT. START .AND. TICKAT .LT. LSTTIK )
            FINISH = TICK
            TICKAT = TICKAT + 1
            CALL DAFGDA ( HANDLE, TICKAT, TICKAT, TICK  )
         END DO

!
!        A structurally correct CK-3 segment should never allow
!        the next test to pass, but it's just easier to check than
!        police the writers of C-kernels.  The only way to get into
!        the block below is if TICKAT .EQ. LSTTIK
!
         IF ( TICK .LT. START ) THEN
            FINISH = TICK
            TICKAT = TICKAT + 1
         END IF

!
!        Insert the interval into the window.
!
         CALL WNINSD ( BEGIN, FINISH, SCHEDL )

      END DO

      CALL CHKOUT ( 'ZZCKCVR3' )
      RETURN
      END
