!$Procedure ZZCKCVR5 ( Private --- C-kernel segment coverage, type 05 )

      SUBROUTINE ZZCKCVR5 ( HANDLE, ARRBEG, ARREND, SCHEDL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 05 C-kernel segment.
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

      INCLUDE              'ck05.inc'

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
!     ARRBEG     is the beginning address of a type 05 segment
!
!     ARREND     is the ending address of a type 05 segment.
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
!     Several parameters associated with the type 05 C-kernel
!     are utilized to compute the packet size of each subtype.
!     See the include file 'ck05.inc' for details.
!
! $ Files
!
!     This routine reads the contents of the file associated with
!     HANDLE to locate coverage intervals.
!
! $ Exceptions
!
!     1) The error SPICE(NOTSUPPORTED) is signaled if the subtype
!        of the CK type 05 segment is not recognized.
!
!     2) Routines in the call tree of this routine may signal errors
!        if insufficient room in SCHEDL exists or other error
!        conditions relating to file access arise.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 05 C-kernel segment.
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
!-    SPICELIB Version 1.0.0, 28-AUG-2002 (FST)
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
      DOUBLE PRECISION      BUFFER ( 4 )
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
      INTEGER               SUBTYP
      INTEGER               TICKAT

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCVR5' )
      END IF

!
!     Get the meta-data associated with this segment that we
!     require to produce the schedule.
!
!     BUFFER(1) = Subtype Code
!     BUFFER(2) = Window Size
!     BUFFER(3) = Number of Interpolation Intervals
!     BUFFER(4) = Number of Packets
!
      CALL DAFGDA ( HANDLE, ARREND - 3, ARREND, BUFFER )

      SUBTYP = NINT ( BUFFER ( 1 ) )
      INVLS  = NINT ( BUFFER ( 3 ) )
      NREC   = NINT ( BUFFER ( 4 ) )
      NDIR   = (NREC-1)/ 100

!
!     Compute the packet size.  This requires parameters listed
!     in the include file 'ck05.inc' and is based on the subtype.
!
      IF      ( SUBTYP .EQ. C05TP0 ) THEN
         RSIZE = C05PS0
      ELSE IF ( SUBTYP .EQ. C05TP1 ) THEN
         RSIZE = C05PS1
      ELSE IF ( SUBTYP .EQ. C05TP2 ) THEN
         RSIZE = C05PS2
      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN
         RSIZE = C05PS3
      ELSE
         CALL SETMSG ( 'CK type 5 subtype <#> is not supported.' )
         CALL ERRINT ( '#', BUFFER(1)                            )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
         CALL CHKOUT ( 'ZZCKCVR5'                                )
         RETURN
      END IF

!
!     Recall that the segment is layed out as:
!
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
!       |    Seconds per tick    |
!       +------------------------+
!       |      Subtype code      |
!       +------------------------+
!       |      Window size       |
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
         CALL CHKOUT ( 'ZZCKCVR5' )
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
            CALL CHKOUT ( 'ZZCKCVR5' )
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
!        A structurally correct CK-5 segment should never allow
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


      CALL CHKOUT ( 'ZZCKCVR5' )
      RETURN
      END
