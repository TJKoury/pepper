!$Procedure  CK3SDN ( Down sample type 3 CK data prepared for writing )

      SUBROUTINE CK3SDN ( SDNTOL, AVFLAG, &
                         NREC, SCLKDP, QUATS, AVVS, &
                         NINTS, STARTS, DPARR, INTARR )
      use pepper_legacy

! $ Abstract
!
!     Down sample type 3 CK data prepared for writing.
!
! $ Required_Reading
!
!     CK
!     DAF
!     ROTATIONS
!     SCLK
!
! $ Keywords
!
!     POINTING
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      SDNTOL
      LOGICAL               AVFLAG
      INTEGER               NREC
      DOUBLE PRECISION      SCLKDP (      * )
      DOUBLE PRECISION      QUATS  ( 0:3, * )
      DOUBLE PRECISION      AVVS   (   3, * )
      INTEGER               NINTS
      DOUBLE PRECISION      STARTS (      * )
      DOUBLE PRECISION      DPARR  (      * )
      INTEGER               INTARR (      * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     SDNTOL     I   Tolerance used for sampling down.
!     AVFLAG     I   True if angular velocity data is set.
!     NREC      I/O  Number of pointing records.
!     SCLKDP    I/O  Encoded SCLK times.
!     QUATS     I/O  Quaternions representing instrument pointing.
!     AVVS      I/O  Angular velocity vectors.
!     NINTS      I   Number of intervals.
!     STARTS     I   Encoded SCLK interval start times.
!     DPARR      I   Double precision work array.
!     INTARR     I   Integer work array.
!
! $ Detailed_Input
!
!     SDNTOL     is the angular tolerance, in radians, to be used to
!                down sample the input CK type 3 pointing data.
!                SDNTOL must be a non-negative number.
!
!     AVFLAG     is a logical flag indicating whether or not
!                the angular velocity data should be processed.
!
!     NREC       is the number of pointing instances in the input
!                buffer.
!
!     SCLKDP     are the encoded spacecraft clock times associated with
!                each pointing instance. These times must be strictly
!                increasing.
!
!     QUATS      is the quaternion buffer.
!
!     AVVS       is the angular velocity vector buffer.
!
!                If AVFLAG is FALSE then this array is ignored by the
!                routine; however it still must be supplied as part of
!                the calling sequence.
!
!     NINTS      is the number of intervals that the pointing instances
!                are partitioned into.
!
!     STARTS     are the start times of each of the interpolation
!                intervals. These times must be strictly increasing
!                and must coincide with times for which the input
!                quaternion buffer contains pointing.
!
!     DPARR      is a double precision work array.
!
!     INTARR     is an integer work array.
!
! $ Detailed_Output
!
!     NREC       is the number of pointing instances in the buffer
!                after down sampling.
!
!     SCLKDP     is the encoded spacecraft clock time buffer after
!                down sampling.
!
!     QUATS      is the quaternion buffer after down sampling.
!
!     AVVS       is the angular velocity vector buffer after down
!                sampling.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!      1) If the number of pointing records is not greater than zero,
!         the error SPICE(INVALIDNUMBEROFRECORDS) is signaled.
!
!      2) If the number of interval starts is not greater than zero,
!         the error SPICE(INVALIDNUMBEROFINTERVALS) is signaled.
!
!      3) If the number of interval starts is not is not less than
!         or equal to the number of records, the error
!         SPICE(BUFFERSIZESMISMATCH) is signaled.
!
!      4) If the first interval start time is not the same as the
!         first record time, the error SPICE(FIRSTRECORDMISMATCH)
!         is signaled.
!
!      5) If the down sampling tolerance is not a non-negative number,
!         the error SPICE(BADDOWNSAMPLINGTOL) is signaled.
!
!      6) If record times buffer does not contain any of the times
!         from interval start times buffers, the error
!         SPICE(INTERVALSTARTNOTFOUND) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine eliminates from the input quaternion and angular
!     rate buffers all data points for which type 3 CK interpolation
!     between bounding points that are not eliminated would produce
!     result that is within specified tolerance of the input attitude.
!     The elimination, referred to in these comments as "down sampling",
!     is done within each individual interpolation interval (as
!     specified in the input interval starts buffer), with intervals
!     boundaries unchanged.
!
! $ Examples
!
!     Normally this routine would be called immediately before the
!     CKW03 is called and be supplied with the input time, quaternion,
!     angular rate, and interval start buffers that were fully and
!     properly prepared for the CKW03 input, like this:
!
!         CALL CK3SDN ( SDNTOL, ARFLAG,
!        .              NREC, SCLKDP, QUATS, AVVS, NINTS, STARTS,
!        .              DPARR, INTARR )
!
!         CALL CKW03  ( HANDLE, SCLKDP(1), SCLKDP(NREC),
!        .              INSTID, FRMNAM, ARFLAG, SEGID,
!        .              NREC, SCLKDP, QUATS, AVVS, NINTS, STARTS )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!
!-    Beta Version 1.3.0, 29-AUG-2019 (BVS)
!
!        BUG FIX: changed to declare fit to be OK for cases when KEEPF
!        and KEEPL points are next to each other but do not pass the
!        too close to 180 degrees apart check. In such cases the
!        algorithm went into indefinite loop.
!
!-    Beta Version 1.2.0, 04-JUN-2012 (BVS)
!
!        BUG FIX: changed the end-point selection algorithm to not
!        consider end-point quaternions that are close to 180 degrees
!        apart to prevent cases in which the quaternion (QMINI) and
!        matrix (LINROT_M-like) interpolation algorithms produce
!        rotations in the opposite directions due to numerics.
!
!-    Beta Version 1.1.0, 19-SEP-2005 (BVS)(FST)
!
!        Incorporated Scott's shrinking window search algorithm to
!        speed up down sampling.
!
!-    Beta Version 1.0.0, 29-JUL-2005 (BVS)(NJB)
!
!-&

! $ Index_Entries
!
!     sample down ck type_3 pointing data prepared for writing
!
!-&

!
!     SPICELIB functions.
!

!
!     Local parameters.
!
!     Tolerance for 180 separation check, in radians.
!
      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 0.001D0 )

!
!     Local variables.
!
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      QINTRP ( 0 : 3 )
      DOUBLE PRECISION      QKEEPF ( 0 : 3 )
      DOUBLE PRECISION      QKEEPL ( 0 : 3 )
      DOUBLE PRECISION      QLNEG  ( 0 : 3 )
      DOUBLE PRECISION      QLINPT ( 0 : 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      DIST2
      DOUBLE PRECISION      DIST2A
      DOUBLE PRECISION      DIST2B
      DOUBLE PRECISION      DPOS
      DOUBLE PRECISION      DNEG
      DOUBLE PRECISION      COSVAL

      INTEGER               I
      INTEGER               J
      INTEGER               INTNRF
      INTEGER               INTCRF
      INTEGER               INTCRL
      INTEGER               KEEPF
      INTEGER               KEEPL
      INTEGER               NDROPD

      INTEGER               LEFT
      INTEGER               RIGHT

      LOGICAL               SKIPIT
      LOGICAL               FITOK

!
!     SPICELIB functions.
!

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CK3SDN' )
      END IF

!
!     Let's do some sanity checks that needed to make sure that future
!     loops and comparisons don't blow up. First, verify that the
!     number pointing records is greater that zero.
!
      IF ( NREC .LE. 0 ) THEN
         CALL SETMSG ( 'The number of pointing records must be ' // &
                       'greater than zero. It was #.'             )
         CALL ERRINT ( '#', NREC                                  )
         CALL SIGERR ( 'SPICE(INVALIDNUMBEROFRECORDS)'            )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN
      END IF

!
!     Then, verify that the number intervals is greater that zero.
!
      IF ( NINTS .LE. 0 ) THEN
         CALL SETMSG ( 'The number of interval starts must be '  // &
                       'greater than zero. It was #.'             )
         CALL ERRINT ( '#', NINTS                                 )
         CALL SIGERR ( 'SPICE(INVALIDNUMBEROFINTERVALS)'          )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN
      END IF

!
!     Then, verify that the number intervals is less than or equal to
!     the number of records.
!
      IF ( NINTS .GT. NREC ) THEN
         CALL SETMSG ( 'The number of interval starts, #, '      // &
                       'is not less than or equal to the '       // &
                       'number of records, #.'                    )
         CALL ERRINT ( '#', NINTS                                 )
         CALL ERRINT ( '#', NREC                                  )
         CALL SIGERR ( 'SPICE(BUFFERSIZESMISMATCH)'               )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN
      END IF

!
!     Then verify that the first time in the intervals array is the same
!     as the first time in the records array.
!
      IF ( SCLKDP(1) .NE. STARTS(1) ) THEN
         CALL SETMSG ( 'The first interval start time, #, '      // &
                       'is not the same as the first record '    // &
                       'time, #.'                                 )
         CALL ERRDP  ( '#', SCLKDP(1)                             )
         CALL ERRDP  ( '#', STARTS(1)                             )
         CALL SIGERR ( 'SPICE(FIRSTRECORDMISMATCH)'               )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN
      END IF

!
!     Finally verify that input down sampling tolerance is not positive
!     number.
!
      IF ( SDNTOL .LT. 0.D0 ) THEN
         CALL SETMSG ( 'The down sampling tolerance must '        // &
                       'be a non-negative number. It was #.'      )
         CALL ERRDP  ( '#', SDNTOL                                )
         CALL SIGERR ( 'SPICE(BADDOWNSAMPLINGTOL)'                )
         CALL CHKOUT ( 'CK3SDN'                                   )
         RETURN
      END IF

!
!     This variable will hold to the index of the pointing record that
!     matches the start of the next interval. For the first interval
!     it is set to one.
!
      INTNRF = 1

!
!     We will count the number of points that were dropped.
!
      NDROPD = 0

!
!     Loop through interpolation intervals.
!
      DO I = 1, NINTS

!
!        Assign the index of the pointing record that matches the
!        begin time of this interval.
!
         INTCRF = INTNRF

!
!        Find the index of the pointing record that ends this interval.
!        If this the last interval, it is the last pointing record in
!        pointing buffer.
!
         IF ( I .EQ. NINTS ) THEN
            INTCRL = NREC
         ELSE

!
!           This is not the last interval. To get its end time we need
!           to find the pointing record that matches the start of the
!           next interval and pick the record before it.
!
!           First we find index of the pointing record that corresponds
!           to the start of the next interval.
!
            INTNRF = BSRCHD ( STARTS( I + 1 ), NREC - INTCRF + 1, &
                              SCLKDP( INTCRF ) )


            IF ( INTNRF .NE. 0 ) THEN

!
!              Found index must be adjusted to be relative to the
!              beginning of the buffer. Currently it is relative to the
!              start of the current interval.
!
               INTNRF = INTNRF + INTCRF - 1
!
!              The index of the last record belonging to this interval
!              in the found index minus 1.
!
               INTCRL = INTNRF - 1

            ELSE

!
!              We did not find such record. The input buffer must have
!              been formed improperly for this to happen. Signal an
!              error.
!
               CALL SETMSG ( 'Cannot find pointing record with '  // &
                             'time that matches the start time '  // &
                             '# (encoded SCLK ticks) of the '     // &
                             'interpolation interval number #.'    )
               CALL ERRDP  ( '#', STARTS( I + 1 )                  )
               CALL ERRINT ( '#', I + 1                            )
               CALL SIGERR ( 'SPICE(INTERVALSTARTNOTFOUND)'        )
               CALL CHKOUT ( 'CK3SDN'                              )
               RETURN

            END IF

         END IF

!
!        Let's look at the indexes of the pointing records
!        corresponding to the begin and end of this interval. If they
!        are the same (meaning it's a singleton interval) or if they
!        are next to each other (meaning that the whole set of
!        interval's pointing data is comprised of only its begin
!        and end points) there is no down sampling to do.
!
         SKIPIT = ( INTCRF .EQ. INTCRL     ) .OR. &
                  ( INTCRF .EQ. INTCRL - 1 )

!
!        Set initial values for a binary search.
!
         KEEPF = INTCRF
         LEFT  = INTCRF
         RIGHT = INTCRL

         DO WHILE ( .NOT. SKIPIT .AND. KEEPF .LT. INTCRL )

!
!           Set the right endpoint of the interval by dividing the
!           binary search region in half.
!
            KEEPL = ( LEFT + RIGHT ) / 2

!
!           Unitize bracketing quaternions as QMINI seems to be
!           very sensitive to that. :)
!
            CALL VHATG ( QUATS( 0, KEEPF ), 4, QKEEPF )
            CALL VHATG ( QUATS( 0, KEEPL ), 4, QKEEPL )

!
!           Pick the closer of the right quaternion or its negative to
!           QKEEPF for input into QMINI to ensure that QMINI does
!           interpolation in the "shortest arc" direction.
!
            CALL VMINUG( QKEEPL, 4, QLNEG )

            DPOS = VDISTG( QKEEPL, QKEEPF, 4 )
            DNEG = VDISTG( QLNEG,  QKEEPF, 4 )

            IF ( DNEG .LT. DPOS ) THEN
               CALL MOVED( QLNEG,  4, QLINPT )
            ELSE
               CALL MOVED( QKEEPL, 4, QLINPT )
            END IF

!
!           If the currently picked window ends are not 180 degrees
!           apart, check all records between them to see if
!           interpolated pointing is within tolerance of the actual
!           pointing. If the currently picked window ends are close to
!           180 degrees apart, don't consider them as a possibility.
!
            COSVAL = QKEEPF(0)*QLINPT(0) + VDOT(QKEEPF(1),QLINPT(1))

            ANGLE = DACOS ( BRCKTD ( COSVAL, -1.D0, 1.D0 ) ) * 2.D0

            FITOK = ABS ( PI() - ANGLE ) .GT. ANGTOL

!
!           If KEEPF and KEEPL points are next to each other, we will
!           declare the fit to be OK even if it does not pass the ``too
!           close to 180 degrees apart'' test and set LEFT equal to
!           RIGHT to move KEEPF forward. If we don't do this, the
!           algorithm goes into indefinite loop.
!
            IF ( KEEPF + 1 .EQ. KEEPL .AND. .NOT. FITOK ) THEN
               FITOK = .TRUE.
               LEFT  = RIGHT
            END IF

            J = KEEPF + 1

            DO WHILE ( J .LE. ( KEEPL - 1 ) .AND. FITOK )

!
!              Compute interpolation fraction for this pointing record.
!
               IF ( SCLKDP( KEEPL ) - SCLKDP( KEEPF ) .NE. 0.D0 ) THEN
                  FRAC = ( SCLKDP( J )     - SCLKDP( KEEPF ) ) / &
                         ( SCLKDP( KEEPL ) - SCLKDP( KEEPF ) )
               ELSE
                  CALL SIGERR ( 'SPICE(CK3SDNBUG)'                 )
                  CALL CHKOUT ( 'CK3SDN'                           )
                  RETURN
               END IF

!
!              Call Nat's fast quaternion interpolation routine to
!              compute interpolated rotation for this point.
!
               CALL QMINI ( QKEEPF, QLINPT, FRAC, QINTRP )

!
!              Find the squared distance between the interpolated
!              and input quaternions.
!
               DIST2A = (QUATS(0,J)-QINTRP(0)) * (QUATS(0,J)-QINTRP(0)) &
                      + (QUATS(1,J)-QINTRP(1)) * (QUATS(1,J)-QINTRP(1)) &
                      + (QUATS(2,J)-QINTRP(2)) * (QUATS(2,J)-QINTRP(2)) &
                      + (QUATS(3,J)-QINTRP(3)) * (QUATS(3,J)-QINTRP(3))

               DIST2B = (QUATS(0,J)+QINTRP(0)) * (QUATS(0,J)+QINTRP(0)) &
                      + (QUATS(1,J)+QINTRP(1)) * (QUATS(1,J)+QINTRP(1)) &
                      + (QUATS(2,J)+QINTRP(2)) * (QUATS(2,J)+QINTRP(2)) &
                      + (QUATS(3,J)+QINTRP(3)) * (QUATS(3,J)+QINTRP(3))

               DIST2 = MIN( DIST2A, DIST2B )

!
!              The rotation angle theta is related to the distance by
!              the formula
!
!                 || Q1 - Q2 ||     =  2 * | sin(theta/4) |
!
               ANGLE = 4.D0 * ASIN( MIN( SQRT(DIST2)/2.D0, 1.D0 ) )

!
!              Compare the angle with specified threshold.
!
               FITOK = FITOK .AND. ( ABS( ANGLE ) .LE. SDNTOL )

!
!              Increment index to move to the next record.
!
               J = J + 1

            END DO

!
!           Was the fit OK?
!
            IF ( FITOK ) THEN

!
!              Fit was OK. Check if left and right are equal; if so we
!              found the point that were were looking for.
!
               IF ( LEFT .EQ. RIGHT ) THEN

!
!                 Mark all records between fist and last with DPMAX.
!
                  DO J = KEEPF+1, KEEPL-1
                     SCLKDP(J) = DPMAX()
                     NDROPD = NDROPD + 1
                  END DO

!
!                 Set first point for the next search to be equal to
!                 the to the found point.
!
                  KEEPF = KEEPL

!
!                 Reset window boundaries for binary search.
!
                  LEFT  = KEEPL
                  RIGHT = INTCRL

               ELSE

!
!                 Left and right sides haven't converged yet; shift
!                 left side of the binary search window forward.
!
                  LEFT = KEEPL + 1

               END IF

            ELSE

!
!              No fit; shift right side of the binary search window
!              backwards.
!
               RIGHT = KEEPL - 1

!
!              If right side went "over" the left side, set left side
!              to be equal to the right side.
!
               IF ( RIGHT .LT. LEFT ) THEN
                  LEFT = RIGHT
               END IF

            END IF

         END DO

      END DO

!
!     At this point all records that are to be removed, if any, have
!     been "tagged" with DPMAX in the times buffer. We need to re-sort
!     the buffers to push these records to the bottom and re-set the
!     number of records to indicate that only the top portion should be
!     used.
!
      IF ( NDROPD .NE. 0 ) THEN

!
!        Since SCLKs were the ones "marked" by DPMAX, we will use them
!        to get the order vector.
!
         CALL ORDERD ( SCLKDP, NREC, INTARR )

!
!        Now, with the order vector in hand, sort the SCLKs ...
!
         CALL REORDD ( INTARR, NREC, SCLKDP )

!
!        ... then sort quaternions (element by element) ...
!
         DO I = 0, 3
            DO J = 1, NREC
               DPARR( J ) = QUATS ( I, J )
            END DO
            CALL REORDD ( INTARR, NREC, DPARR )
            DO J = 1, NREC
               QUATS ( I, J ) = DPARR( J )
            END DO
         END DO

!
!        ... and, finally, if requested, sort AVs (also element by
!        element) ...
!
         IF ( AVFLAG ) THEN
            DO I = 1, 3
               DO J = 1, NREC
                  DPARR( J ) = AVVS ( I, J )
               END DO
               CALL REORDD ( INTARR, NREC, DPARR )
               DO J = 1, NREC
                  AVVS ( I, J ) = DPARR( J )
               END DO
            END DO
         END IF

!
!        Reset the number of points.
!
         NREC = NREC - NDROPD

      END IF

!
!     All done. Check out.
!
      CALL CHKOUT ( 'CK3SDN' )

      RETURN

      END
