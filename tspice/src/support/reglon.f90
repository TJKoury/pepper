!$Procedure REGLON ( Regularize longitude intervals )

      SUBROUTINE REGLON ( NIVALS, BOUNDS, MAXN,   NOUT, &
                          MINLON, MAXLON, OUTBDS, SRCS )
      use pepper_inc_dsktol

! $ Abstract
!
!     Regularize a set of longitude intervals. The output
!     intervals have their endpoints in ascending order,
!     and all output bounds are in the range MINLON:MAXLON.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DSKBRIEF
!
! $ Declarations

      IMPLICIT NONE


      INTEGER               NIVALS
      DOUBLE PRECISION      BOUNDS ( 2, NIVALS )
      INTEGER               MAXN
      INTEGER               NOUT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      OUTBDS ( 2, * )
      INTEGER               SRCS   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NIVALS     I   is the number of input longitude intervals.
!     BOUNDS     I   is an array of bounds of the input intervals.
!     MAXN       I   is the maximum number of output intervals.
!     NOUT       O   is the number of output intervals.
!     MINLON     O   is the minimum longitude of all output intervals.
!     MAXLON     O   is the maximum longitude of all output intervals.
!     OUTBDS     O   is an array of output longitude intervals.
!     SRCS       O   is an array mapping output to input intervals.
!
! $ Detailed_Input
!
!     NIVALS     is the number of input longitude intervals.
!
!     BOUNDS     is an array of upper and lower interval bounds.
!                Units are radians.
!
!                The elements
!
!                   BOUNDS(1,I)
!                   BOUNDS(2,I)
!
!                are, respectively, the lower and upper bounds of
!                the Ith interval. The upper bound may be less
!                than the lower bound. If the bounds are equal, they
!                are treated as though the upper bound exceeds the
!                lower bound by 2*pi.
!
!                 Bounds must be in the range
!
!                   [ -2*pi,  2*pi ]
!
!     MAXN       is the maximum number of output intervals that
!                can be returned.
!
! $ Detailed_Output
!
!     NOUT       is the number of output intervals. NOUT is greater
!                than or equal to NIVALS.
!
!     MINLON,
!     MAXLON     are, respectively, lower and upper bounds on
!                the range of the output longitudes.
!
!                   {MINLON, MAXLON}
!
!                is either
!
!                   {0, 2*pi}  or {-pi, pi}
!
!     OUTBDS     is an array of output longitude bounds.
!                Units are radians.
!
!                Each output interval represents a subset of some input
!                interval; the endpoints of an output interval may be
!                shifted by 2*pi relative to the endpoints of the
!                subset.
!
!                Input intervals for which the lower bound is greater
!                than the upper bound are broken up into two output
!                intervals.
!
!                The elements
!
!                   OUTBDS(1,I)
!                   OUTBDS(2,I)
!
!                are, respectively, the lower and upper bounds of
!                the Ith interval. The upper bound is always
!                greater than or equal to the lower bound.
!                Bounds are in the range
!
!                   [MINLON, MAXLON]
!
!
!     SRCS       is an array of indices that map the output intervals
!                to the source input intervals from which they were
!                derived.
!
! $ Parameters
!
!     ANGMRG     See the description in dsktol.inc.
!
! $ Exceptions
!
!     1)  If the output array doesn't have enough room to store
!         the output intervals, the error SPICE(ARRAYTOOSMALL)
!         is signaled.
!
!     2)  Longitudes outside the range
!
!            -2*pi - ANGMRG : 2*pi + ANGMRG
!
!         are not accepted: if such a value is encountered, the
!         error will be diagnosed by a routine in the call tree
!         of this routine.
!
!     3)  If the lower bound of a longitude interval matches the
!         upper bound, the error SPICE(ZEROBOUNDSEXTENT) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine "regularizes" a set of longitude intervals: it maps
!     them to a set of output longitude intervals that has the same
!     coverage on the unit circle as the input set, such that each of
!     the output intervals has its endpoints in increasing order, and
!     all of the output intervals have their endpoints in a common
!     interval of length 2*pi.
!
!     The set of output intervals has the property that order
!     relationships between endpoints are valid.
!
!     This routine supports coverage gap determination.
!
! $ Examples
!
!     See usage in DSKBRIEF.
!
! $ Restrictions
!
!     1) For use only within program DSKBRIEF.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!     DSKBRIEF Version 1.0.0, 21-FEB-2017 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      LB
      DOUBLE PRECISION      LOCLB
      DOUBLE PRECISION      LOCUB
      DOUBLE PRECISION      UB

      INTEGER               I
      INTEGER               NREQ


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'REGLON' )

!
!     No output intervals have been found yet.
!
      NOUT = 0

      IF ( NIVALS .EQ. 0 ) THEN

         CALL CHKOUT ( 'REGLON')
         RETURN

      END IF

!
!     Get lower and upper bounds of input values.
!
      MINLON = DPMAX()
      MAXLON = DPMIN()

      DO I = 1, NIVALS

         LB = BOUNDS(1,I)
         UB = BOUNDS(2,I)
!
!        Rectangles of zero longitude extent not allowed.
!
         IF ( LB .EQ. UB ) THEN

            CALL SETMSG ( 'Longitude lower bound # (# degrees) ' &
            //            'equals upper bound.'                  )
            CALL ERRDP  ( '#', LB                                )
            CALL ERRDP  ( '#', LB * DPR()                        )
            CALL SIGERR ( 'SPICE(ZEROBOUNDSEXTENT)'              )
            CALL CHKOUT ( 'REGLON'                               )
            RETURN

         END IF

!
!        Adjust UB if necessary before deciding on the output
!        range.
!
         IF ( UB .LT. LB ) THEN
            UB = UB + TWOPI()
         END IF

         MINLON = MIN( LB, UB, MINLON )
         MAXLON = MAX( LB, UB, MAXLON )

      END DO

!
!     If MAXLON and MINLON lie within the range
!
!        0 - ANGMRG : 2*pi + ANGMRG
!
!     we'll set the output longitudes to lie in the range
!
!        0 : 2*pi
!
!
!     If MAXLON and MINLON lie within the range
!
!        -pi - ANGMRG : pi + ANGMRG
!
!     we'll set the output longitudes to lie in the range
!
!        -pi : pi
!
!
!     We use the latter range if neither of the first two
!     conditions are met.
!
!
      IF (      ( MINLON .GT.  -ANGMRG              ) &
          .AND. ( MAXLON .LT. ( TWOPI() + ANGMRG )  )  ) THEN

         A = 0.D0
         B = TWOPI()

      ELSE
!
!        We arbitrarily pick the output longitude range
!
!           -pi : pi
!
         A = -PI()
         B =  PI()

      END IF

!
!     Set the output values of MINLON and MAXLON.
!
      MINLON = A
      MAXLON = B

!
!     Process each input interval.
!
      DO I = 1, NIVALS

         LB = BOUNDS(1,I)
         UB = BOUNDS(2,I)
!
!        We'll adjust the inputs to ensure they're in range.
!
!        First, make sure we're starting with values in
!        the range [-2*pi, 2*pi].
!
         CALL ZZNRMLON ( LB, UB, ANGMRG, LOCLB, LOCUB )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'REGLON' )
            RETURN
         END IF

!
!        Move each output into the range [A, B].
!
         IF ( LOCLB .LT. A ) THEN

            LOCLB = LOCLB + TWOPI()

         ELSE IF ( LOCLB .GT. B ) THEN

            LOCLB = LOCLB - TWOPI()

         END IF


         IF ( LOCUB .LT. A ) THEN

            LOCUB = LOCUB + TWOPI()

         ELSE IF ( LOCUB .GT. B ) THEN

            LOCUB = LOCUB - TWOPI()

         END IF

!
!        Now the bounds are in range, but they may be
!        out of order.
!
         IF ( LOCLB .LT. LOCUB ) THEN
!
!           The bounds are in order. Add the interval to
!           the list of output intervals.
!
            NOUT = NOUT + 1

            IF ( NOUT .GT. MAXN ) THEN
!
!              We're out of room.
!
               CALL SETMSG ( 'Output arrays have room for # ' &
               //            'intervals we have found # ' &
               //            'output intervals so far.'      )
               CALL ERRINT ( '#', MAXN                       )
               CALL ERRINT ( '#', NOUT                       )
               CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'          )
               CALL CHKOUT ( 'REGLON'                        )
               RETURN

            END IF

            OUTBDS(1,NOUT) = LOCLB
            OUTBDS(2,NOUT) = LOCUB
            SRCS  (  NOUT) = I


         ELSE
!
!           The bounds are in range but out of order.
!           We'll split the input interval into two
!           output intervals.
!
            NREQ = 0

            IF ( A .LT. LOCUB ) THEN
               NREQ = NREQ + 1
            END IF

            IF ( LOCLB .LT. B ) THEN
               NREQ = NREQ + 1
            END IF


            IF ( NOUT+NREQ .GT. MAXN ) THEN
!
!              We're out of room.
!
               CALL SETMSG ( 'Output arrays have room for # ' &
               //            'intervals we have found # ' &
               //            'output intervals so far.'      )
               CALL ERRINT ( '#', MAXN                       )
               CALL ERRINT ( '#', NOUT+NREQ                  )
               CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'          )
               CALL CHKOUT ( 'REGLON'                        )
               RETURN

            END IF
!
!           The input interval "wraps around" the output boundaries.
!
!           The output intervals extend from A to the upper bound and
!           from the lower bound to B.
!
            IF ( A .LT. LOCUB ) THEN

               NOUT = NOUT + 1

               OUTBDS(1,NOUT) = A
               OUTBDS(2,NOUT) = LOCUB
               SRCS  (  NOUT) = I

            END IF

            IF ( LOCLB .LT. B ) THEN

               NOUT = NOUT + 1

               OUTBDS(1,NOUT) = LOCLB
               OUTBDS(2,NOUT) = B
               SRCS  (  NOUT) = I

            END IF

         END IF

      END DO

      CALL CHKOUT ( 'REGLON' )
      RETURN
      END



