!$Procedure ZZNRMLON ( Normalize longitude bounds )

      SUBROUTINE ZZNRMLON ( INMIN, INMAX, TOL, OUTMIN, OUTMAX )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Normalize longitude bounds: map bounds into the interval
!
!        [ -2*pi, 2*pi ]
!
!     Put the bounds in order and ensure that the bounds differ by no
!     more than 2*pi.
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
!     None.
!
! $ Keywords
!
!     GEOMETRY
!     LONGITUDE
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      INMIN
      DOUBLE PRECISION      INMAX
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      OUTMIN
      DOUBLE PRECISION      OUTMAX

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INMIN      I   Longitude interval lower bound.
!     INMAX      I   Longitude interval upper bound.
!     TOL        I   Round-off tolerance.
!     OUTMIN     O   Adjusted longitude interval lower bound.
!     OUTMAX     O   Adjusted longitude interval upper bound.
!
! $ Detailed_Input
!
!     INMIN,
!     INMAX      are, respectively, the lower and upper
!                bounds of a longitude interval. Units are
!                radians. INMIN and INMAX must lie in the range
!
!                   [-2*pi, 2pi]
!
!                INMAX is allowed to be less than INMIN, but must
!                not be equal to it.
!
!
!     TOL        is a non-negative tolerance value. If an input bound
!                lies outside of the range
!
!                   [-2*pi, 2pi]
!
!                by less than TOL, it is interpreted as being equal
!                to the nearest interval endpoint.
!
!                If INMAX exceeds INMIN by less than TOL, the bounds
!                are interpreted as being 2*pi radians apart.
!
!
! $ Detailed_Output
!
!     OUTMIN,
!     OUTMAX     are, respectively, the normalized lower and upper
!                bounds of the longitude interval described by the
!                inputs. Units are radians.
!
!                "Normalization" means the bounds are modified if
!                necessary so that they represent the same interval
!                as [INMIN, INMAX], but also meet the following
!                criteria:
!
!                   1) Both OUTMIN and OUTMAX lie in the interval
!                      [-2*pi, 2*pi].
!
!                   2) OUTMIN is strictly less than OUTMAX.
!
!                   3) OUTMAX does not exceed OUTMIN by more than 2*pi.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     2)  If either input longitude is less than 2*pi-TOL or
!         greater than 2*pi + TOL, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     3)  If INMAX equals INMIN, or if INMAX is less than INMIN
!         by an integer multiple of 2*pi, the error
!         SPICE(ZEROBOUNDSEXTENT) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine centralizes an oft-repeated algorithm. It is
!     called by several DSK routines.
!
! $ Examples
!
!     See usage in ZZINLAT.
!
! $ Restrictions
!
!     This is a private routine. It is meant to be used only by SPICE
!     routines.
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
!-    SPICELIB Version 1.0.0, 11-OCT-2016 (NJB)
!
!        Original version 16-MAY-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     normalize longitude interval
!     normalize longitude boundaries
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      TWOPI

!
!     Local variables
!
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      PI2

      LOGICAL               FIRST

!
!     Saved values
!
      SAVE                  FIRST
      SAVE                  PI2

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /

!
!     Use discovery check-in. Don't check RETURN.
!
      IF ( FIRST ) THEN

         PI2   = TWOPI()
         FIRST = .FALSE.

      END IF

!
!     TOL cannot be negative.
!
      IF ( TOL .LT. 0.D0 ) THEN

         CALL CHKIN  ( 'ZZNRMLON'                                  )
         CALL SETMSG ( 'Tolerance must be non-negative but was #.' )
         CALL ERRDP  ( '#', TOL                                    )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                    )
         CALL CHKOUT ( 'ZZNRMLON'                                  )
         RETURN

      END IF

!
!     Reject inputs that lie outside of [-2*pi, 2*pi], accounting
!     for the tolerance.
!
      IF (      ( INMIN .LT. -PI2-TOL ) &
           .OR. ( INMIN .GT.  PI2+TOL )  ) THEN

         CALL CHKIN  ( 'ZZNRMLON'                                  )
         CALL SETMSG ( 'Longitude lower bound INMIN = # (radians), ' &
         //            ' = # (deg). The minimum allowed value is ' &
         //            ' -2*pi - TOL = # (radians), = # (deg).'    )
         CALL ERRDP  ( '#', INMIN                                  )
         CALL ERRDP  ( '#', INMIN * DPR()                          )
         CALL ERRDP  ( '#',   -PI2  - TOL                          )
         CALL ERRDP  ( '#', ( -PI2  - TOL ) * DPR()                )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                    )
         CALL CHKOUT ( 'ZZNRMLON'                                  )
         RETURN

      END IF

!
!     The input bounds may not be equal.
!
      IF ( INMIN .EQ. INMAX ) THEN

         CALL CHKIN  ( 'ZZNRMLON'                                  )
         CALL SETMSG ( 'Longitude lower bound INMIN = # (radians), ' &
         //            ' = # (deg), is equal to upper bound.'      )
         CALL ERRDP  ( '#', INMIN                                  )
         CALL ERRDP  ( '#', INMIN * DPR()                          )
         CALL SIGERR ( 'SPICE(ZEROBOUNDSEXTENT)'                   )
         CALL CHKOUT ( 'ZZNRMLON'                                  )
         RETURN

      END IF

!
!     The input longitude is within range or is out of range by at most
!     |TOL| radians. Bracket it.

      OUTMIN = MAX( -PI2,  MIN( INMIN, PI2 ) )

!
!     Same deal for the upper bound.
!
      IF (      ( INMAX .LT. -PI2-TOL ) &
           .OR. ( INMAX .GT.  PI2+TOL )  ) THEN

         CALL CHKIN  ( 'ZZNRMLON'                                  )
         CALL SETMSG ( 'Longitude upper bound INMAX = # (radians), ' &
         //            ' = # (deg). The minimum allowed value is ' &
         //            ' -2*pi - TOL = # (radians), = # (deg).'    )
         CALL ERRDP  ( '#', INMAX                                  )
         CALL ERRDP  ( '#', INMAX * DPR()                          )
         CALL ERRDP  ( '#',   -PI2  - TOL                          )
         CALL ERRDP  ( '#', ( -PI2  - TOL ) * DPR()                )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                    )
         CALL CHKOUT ( 'ZZNRMLON'                                  )
         RETURN

      END IF

      OUTMAX = MAX( -PI2,  MIN( INMAX, PI2 ) )

!
!     If the bounds are out of order, put them in order.
!     It is assumed that no interval has length zero.
!
!     If the upper bound is greater than the lower bound by
!     less than TOL, the bounds are considered to be "out of
!     order."
!
      IF ( OUTMAX .LE. TOUCHD(OUTMIN + TOL) ) THEN
!
!        Shift one of the bounds by 2*pi, while keeping
!        the bounds in the range [-2pi, 2pi].
!
         IF ( OUTMAX .LE. 0.D0 ) THEN
!
!           OUTMAX is non-positive. Shift it to the right.
!
            OUTMAX = MIN ( TOUCHD(OUTMAX + PI2),  PI2 )

            IF ( OUTMAX .LT. OUTMIN ) THEN
!
!              If the bounds are still out of order, shift the lower
!              bound left.
!
               OUTMIN = MAX ( TOUCHD(OUTMIN - PI2), -PI2 )

            END IF

         ELSE
!
!           OUTMAX is > 0. Shift the lower bound left.
!
            OUTMIN = MAX ( TOUCHD(OUTMIN - PI2), -PI2 )

         END IF

      END IF

!
!     If the bounds are too far apart, move them together. Note
!     that OUTMIN and OUTMAX are already set at this point.
!
      DELTA = TOUCHD( OUTMAX - OUTMIN )

      IF (  DELTA  .GT.  TOUCHD( PI2 + TOL )  ) THEN
!
!        Shift the upper bound lower by 2*pi.
!
         OUTMAX = TOUCHD( OUTMAX - PI2 )

      END IF


!
!     The output bounds must not be equal. We could end up with
!     equal output bounds if the input maximum is less than
!     the input minimum and the bounds differ by an integer
!     multiple of 2*pi.
!
      IF ( OUTMIN .EQ. OUTMAX ) THEN

         CALL CHKIN  ( 'ZZNRMLON'                                     )
         CALL SETMSG ( 'After adjustment, input longitude lower ' &
         //            'bound INMIN = # (radians),  = # (deg), ' &
         //            'is equal to adjusted longitude upper bound. ' &
         //            'Input upper bound = # (radians),  = # (deg). ' &
         //            'When the input upper bound is less than the ' &
         //            'input lower bound, the difference must not ' &
         //            'be an integer multiple of 2*pi.'              )
         CALL ERRDP  ( '#', INMIN                                     )
         CALL ERRDP  ( '#', INMIN * DPR()                             )
         CALL ERRDP  ( '#', INMAX                                     )
         CALL ERRDP  ( '#', INMAX * DPR()                             )
         CALL SIGERR ( 'SPICE(ZEROBOUNDSEXTENT)'                      )
         CALL CHKOUT ( 'ZZNRMLON'                                     )
         RETURN

      END IF

      END
