!$Procedure ZZCKCV05 ( Private --- C-kernel segment coverage, type 05 )

      SUBROUTINE ZZCKCV05 ( HANDLE,  ARRBEG,  ARREND,  SCLKID, &
                            DC,      TOL,     TIMSYS,  SCHEDL  )

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
      INTEGER               SCLKID
      DOUBLE PRECISION      DC     ( 2 )
      DOUBLE PRECISION      TOL
      CHARACTER*(*)         TIMSYS
      DOUBLE PRECISION      SCHEDL ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a C-kernel open for read access
!     ARRBEG     I   Beginning DAF address
!     ARREND     I   Ending DAF address
!     SCLKID     I   ID of SCLK associated with segment.
!     DC         I   D.p. component of CK segment descriptor.
!     TOL        I   Tolerance in ticks.
!     TIMSYS     I   Time system used to represent coverage.
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
!     SCLKID     is the ID code of the spacecraft clock associated with
!                the object for which the segment contains pointing.
!                This is the ID code used by the SCLK conversion
!                routines.
!
!     DC         is the double precision component of the descriptor of
!                the CK segment.  The components are the segment start
!                and stop times.
!
!                Each interpolation interval is replaced with its
!                intersection with the segment coverage interval
!
!                   [ DC(1), DC(2) ]
!
!                before being expanded by TOL. Interpolation intervals
!                that don't intersect the segment coverage interval are
!                discarded, even if after expansion by TOL they would
!                have non-empty intersection with the segment coverage
!                interval.
!
!     TOL        is a tolerance value expressed in ticks of the
!                spacecraft clock associated with the segment. After
!                truncation by the segment coverage interval, and
!                before insertion into the coverage window, each
!                non-empty truncated interpolation interval is expanded
!                by TOL:  the left endpoint of each interval is reduced
!                by TOL and the right endpoint is increased by TOL.
!                Any intervals that overlap as a result of the
!                expansion are merged.
!
!                The coverage window returned when TOL > 0 indicates
!                the coverage provided by the file to the CK readers
!                CKGPAV and CKGP when that value of TOL is passed to
!                them as an input.
!
!
!     TIMSYS     is a string indicating the time system used in the
!                output coverage window.  TIMSYS may have the values:
!
!                   'SCLK'    Elements of SCHEDL are expressed in
!                             encoded SCLK ("ticks"), where the clock
!                             is associated with the object designated
!                             by IDCODE.
!
!                   'TDB'     Elements of SCHEDL are expressed as
!                             seconds past J2000 TDB.
!
!                TIMSYS must be consistent with the system used for
!                the contents of SCHEDL on input, if any.
!
!
!     SCHEDL     is a schedule (window) of intervals, to which the
!                intervals of coverage for this segment will be added.
!
! $ Detailed_Output
!
!     SCHEDL     the input schedule updated to include the intervals
!                of coverage for this segment.  The schedule has
!                been adjusted to account for the provided tolerance
!                value.  Coverage lying outside the interval
!
!                   DC(1) - TOL : DC(2) + TOL
!
!                is excluded.
!
!                The elements of SCHEDL are given in the time system
!                indicated by TIMSYS.
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
!     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
!         the CK type 05 segment is not recognized.
!
!     2)  Routines in the call tree of this routine may signal errors
!         if insufficient room in SCHEDL exists or other error
!         conditions relating to file access arise.
!
!     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
!     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
!         is signaled.
!
!     5)  If a time conversion error occurs, the error will be
!         diagnosed by a routine in the call tree of this routine.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 05 C-kernel segment.
!
! $ Examples
!
!     See CKCOV.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB) (FST) (WLT)
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               EQSTR
      LOGICAL               RETURN

!
!     Local Variables
!
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      BUFFER ( 4 )
      DOUBLE PRECISION      ET
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

      LOGICAL               BAIL
      LOGICAL               ISTDB

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCV05' )
      END IF

!
!     Check tolerance value.
!
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZCKCV05'                                     )
         RETURN

      END IF

!
!     Set a logical flag indicating whether the time systm is SCLK.
!
      ISTDB  = EQSTR ( TIMSYS, 'TDB' )

!
!     Check time system.
!
      IF ( .NOT. ISTDB ) THEN

         IF (  .NOT.  EQSTR ( TIMSYS, 'SCLK' )  )THEN

            CALL SETMSG ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            CALL ERRCH  ( '#',  TIMSYS                              )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                    )
            CALL CHKOUT ( 'ZZCKCV05'                                )
            RETURN

         END IF

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
         CALL CHKOUT ( 'ZZCKCV05'                                )
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
         CALL CHKOUT ( 'ZZCKCV05' )
         RETURN
      END IF


      BAIL = .FALSE.

      DO WHILE (       ( INTAT  .LE.  LSTINT ) &
                 .AND. ( TICKAT .LE.  LSTTIK ) &
                 .AND. (        .NOT. BAIL   )  )
!
!        At this point, we have an interval that begins at START
!        and ends at FINISH (unless of course we never found a "good"
!        TICK to start with.)
!
         BEGIN = START

!
!        If the start of the interval was the start of the LAST
!        interval available, we can short cut the remainder of the
!        reads.
!
         IF ( INTAT .EQ. LSTINT ) THEN

            CALL DAFGDA ( HANDLE, LSTTIK, LSTTIK, FINISH )

            BAIL = .TRUE.
!
!           The routine will return at the end of this loop
!           iteration.  But first, we may have to update BEGIN
!           and FINISH, depending on the values of TOL and TIMSYS,
!           and we have to insert these values into SCHEDL.
!           We'll carry out these tasks at the end of this IF block.


         ELSE
!
!           This is the expected case.  Get the start of the next
!           interval.
!
            INTAT  = INTAT  + 1
            CALL DAFGDA ( HANDLE, INTAT, INTAT, START )

!
!           Read forward from the last tick until we reach the
!           START of the next interval or until we run out of TICKS.
!
            DO WHILE ( TICK .LT. START .AND. TICKAT .LT. LSTTIK )
               FINISH = TICK
               TICKAT = TICKAT + 1
               CALL DAFGDA ( HANDLE, TICKAT, TICKAT, TICK  )
            END DO

!
!           A structurally correct CK-5 segment should never allow the
!           next test to pass, but it's just easier to check than
!           police the writers of C-kernels.  The only way to get into
!           the block below is if TICKAT .EQ. LSTTIK
!
            IF ( TICK .LT. START ) THEN
               FINISH = TICK
               TICKAT = TICKAT + 1
            END IF

         END IF

!
!        Truncate the interval using the segment bounds.
!
         BEGIN   =  MAX ( BEGIN,  DC(1) )
         FINISH  =  MIN ( FINISH, DC(2) )

!
!        Adjust the interval using the tolerance.  Empty
!        intervals *do not get expanded*; this choice is
!        consistent with the type 5 reading algorithm.
!
         IF ( BEGIN .LE. FINISH ) THEN

            IF ( TOL .GT. 0.D0 ) THEN

               BEGIN   =  MAX( BEGIN  - TOL, 0.D0 )
               FINISH  =       FINISH + TOL

            END IF

         END IF

!
!        Convert the time to TDB if necessary.
!
         IF ( ISTDB ) THEN

            CALL SCT2E ( SCLKID, BEGIN,  ET )
            BEGIN  = ET

            CALL SCT2E ( SCLKID, FINISH, ET )
            FINISH = ET

         END IF

!
!        Insert the interval into the window.
!
         IF ( BEGIN .LE. FINISH ) THEN
            CALL WNINSD ( BEGIN, FINISH, SCHEDL )
         END IF

      END DO


      CALL CHKOUT ( 'ZZCKCV05' )
      RETURN
      END
