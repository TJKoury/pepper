!$Procedure ZZCKCV02 ( Private --- C-kernel segment coverage, type 02 )

      SUBROUTINE ZZCKCV02 ( HANDLE,  ARRBEG,  ARREND, &
                            SCLKID,  TOL,     TIMSYS,  SCHEDL  )

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
      INTEGER               SCLKID
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
!     TOL        I   Tolerance in ticks.
!     TIMSYS     I   Time system used to represent coverage.
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
!
!     SCLKID     is the ID code of the spacecraft clock associated with
!                the object for which the segment contains pointing.
!                This is the ID code used by the SCLK conversion
!                routines.
!
!     TOL        is a tolerance value expressed in ticks of the
!                spacecraft clock associated with the segment. Before
!                each interval is inserted into the coverage window,
!                the intervals are expanded by TOL:  the left endpoint
!                of each interval is reduced by TOL and the right
!                endpoint is increased by TOL.  Any intervals that
!                overlap as a result of the expansion are merged.
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
!     1) Routines in the call tree of this routine may signal errors
!        if insufficient room in SCHEDL exists or other error
!        conditions relating to file access arise.
!
!     2) If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
!        signaled.
!
!     3) If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
!        is signaled.
!
!     4) If a time conversion error occurs, the error will be
!        diagnosed by a routine in the call tree of this routine
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
!     N.J. Bachman    (JPL)
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
!-    SPICELIB Version 1.0.0, 03-JAN-2005 (NJB) (FST) (WLT) (BVS)
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               EQSTR
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
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      FIRST ( BSIZE )
      DOUBLE PRECISION      LAST  ( BSIZE )

      INTEGER               ARRSIZ
      INTEGER               BEGAT
      INTEGER               ENDAT
      INTEGER               GET
      INTEGER               GOT
      INTEGER               I
      INTEGER               NREC

      LOGICAL               ISTDB

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCV02' )
      END IF

!
!     Check tolerance value.
!
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZCKCV02'                                     )
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
            CALL CHKOUT ( 'ZZCKCV02'                                )
            RETURN

         END IF

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
!
!           Adjust the interval using the tolerance.
!
            BEGIN  = FIRST(I)
            FINISH = LAST(I)

            IF ( TOL .GT. 0.D0 ) THEN

               BEGIN   =  MAX( BEGIN  - TOL, 0.D0 )
               FINISH  =       FINISH + TOL

            END IF

!
!           Convert the time to TDB if necessary.
!
            IF ( ISTDB ) THEN

               CALL SCT2E ( SCLKID, BEGIN,  ET )
               BEGIN  = ET

               CALL SCT2E ( SCLKID, FINISH, ET )
               FINISH = ET

            END IF

            CALL WNINSD ( BEGIN, FINISH, SCHEDL )

         END DO

         GOT = GOT + GET

      END DO

      CALL CHKOUT ( 'ZZCKCV02' )
      RETURN
      END
