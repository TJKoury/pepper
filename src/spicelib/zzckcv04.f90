!$Procedure ZZCKCV04 ( Private --- C-kernel segment coverage, type 04 )

      SUBROUTINE ZZCKCV04 ( HANDLE,  ARRBEG,  ARREND, &
                            SCLKID,  TOL,     TIMSYS,  SCHEDL  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 04 C-kernel segment.
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

      INCLUDE              'ckparam.inc'

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
!     CK4RSZ     P   C-kernel Type 04 Maximum Record Size
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     ARRBEG     is the beginning address of a type 04 segment
!
!     ARREND     is the ending address of a type 04 segment.
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
!     CK4RSZ     is the maximum length of a CK4 record (with angular
!                velocity). Defined in the include file 'ckparam.inc'.
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
!        diagnosed by a routine in the call tree of this routine.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 04 C-kernel segment.
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
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB) (FST)
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               INTMAX
      LOGICAL               EQSTR
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )

!
!     Local Variables
!
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      VALUES ( CK4RSZ )

      INTEGER               ENDS   ( 2 )
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               NREC

      LOGICAL               ISTDB

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZCKCV04' )

!
!     Check tolerance value.
!
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZCKCV04'                                     )
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
            CALL CHKOUT ( 'ZZCKCV04'                                )
            RETURN

         END IF

      END IF

!
!     Build a descriptor record that satisfies the requirements
!     of CKNR04 and SGFPKT.
!
!     Note: This is a hack dependent on the implementation of
!     the generic segments routines.  But for C-kernels it
!     should always work, as ND and NI aren't changing any
!     time soon.
!
      IC ( 1 ) = INTMAX()
      IC ( 2 ) = INTMAX()
      IC ( 3 ) = 4
      IC ( 4 ) = INTMAX()
      IC ( 5 ) = ARRBEG
      IC ( 6 ) = ARREND

      DC ( 1 ) = 0.0D0
      DC ( 2 ) = 0.0D0

      CALL DAFPS ( ND, NI, DC, IC, DESCR )

!
!     Determine the number of records in the array.
!
      CALL CKNR04 ( HANDLE, DESCR, NREC )

      DO I = 1, NREC

!
!        Extract each packet of pointing coefficients.
!
         CALL SGFPKT ( HANDLE, DESCR, I, I, VALUES, ENDS )

!
!        Compute the left and right end points of the interval
!        of coverage related to this packet.
!
         LEFT  = VALUES ( 1 ) - VALUES ( 2 )
         RIGHT = VALUES ( 1 ) + VALUES ( 2 )

!
!        Adjust the interval using the tolerance.
!
         IF ( TOL .GT. 0.D0 ) THEN

            LEFT   =  MAX( LEFT  - TOL, 0.D0 )
            RIGHT  =       RIGHT + TOL

         END IF

!
!        Convert the time to TDB if necessary.
!
         IF ( ISTDB ) THEN

            CALL SCT2E ( SCLKID, LEFT,  ET )
            LEFT  = ET

            CALL SCT2E ( SCLKID, RIGHT, ET )
            RIGHT = ET

         END IF

!
!        Store the results in the schedule.
!
         CALL WNINSD ( LEFT, RIGHT, SCHEDL )

      END DO

      CALL CHKOUT ( 'ZZCKCV04' )
      RETURN
      END
