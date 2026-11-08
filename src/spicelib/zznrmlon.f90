!$Procedure ZZNRMLON ( Normalize longitude bounds )

      subroutine zznrmlon ( inmin, inmax, tol, outmin, outmax )
      use pepper_kinds, only: wp

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

      implicit none

      real(wp)              inmin
      real(wp)              inmax
      real(wp)              tol
      real(wp)              outmin
      real(wp)              outmax

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
      real(wp)              dpr
      real(wp)              touchd
      real(wp)              twopi

!
!     Local variables
!
      real(wp)              delta
      real(wp)              pi2

      logical               first

!
!     Saved values
!
      save                  first
      save                  pi2

!
!     Initial values
!
      data                  first / .true. /

!
!     Use discovery check-in. Don't check RETURN.
!
      if ( first ) then

         pi2   = twopi()
         first = .false.

      end if

!
!     TOL cannot be negative.
!
      if ( tol < 0.0_wp ) then

         call chkin  ( 'ZZNRMLON'                                  )
         call setmsg ( 'Tolerance must be non-negative but was #.' )
         call errdp  ( '#', tol                                    )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                    )
         call chkout ( 'ZZNRMLON'                                  )
         return

      end if

!
!     Reject inputs that lie outside of [-2*pi, 2*pi], accounting
!     for the tolerance.
!
      if (      ( inmin < -pi2-tol ) &
           .or. ( inmin >  pi2+tol )  ) then

         call chkin  ( 'ZZNRMLON'                                  )
         call setmsg ( 'Longitude lower bound INMIN = # (radians), ' &
         //            ' = # (deg). The minimum allowed value is ' &
         //            ' -2*pi - TOL = # (radians), = # (deg).'    )
         call errdp  ( '#', inmin                                  )
         call errdp  ( '#', inmin * dpr()                          )
         call errdp  ( '#',   -pi2  - tol                          )
         call errdp  ( '#', ( -pi2  - tol ) * dpr()                )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                    )
         call chkout ( 'ZZNRMLON'                                  )
         return

      end if

!
!     The input bounds may not be equal.
!
      if ( inmin == inmax ) then

         call chkin  ( 'ZZNRMLON'                                  )
         call setmsg ( 'Longitude lower bound INMIN = # (radians), ' &
         //            ' = # (deg), is equal to upper bound.'      )
         call errdp  ( '#', inmin                                  )
         call errdp  ( '#', inmin * dpr()                          )
         call sigerr ( 'SPICE(ZEROBOUNDSEXTENT)'                   )
         call chkout ( 'ZZNRMLON'                                  )
         return

      end if

!
!     The input longitude is within range or is out of range by at most
!     |TOL| radians. Bracket it.

      outmin = max( -pi2,  min( inmin, pi2 ) )

!
!     Same deal for the upper bound.
!
      if (      ( inmax < -pi2-tol ) &
           .or. ( inmax >  pi2+tol )  ) then

         call chkin  ( 'ZZNRMLON'                                  )
         call setmsg ( 'Longitude upper bound INMAX = # (radians), ' &
         //            ' = # (deg). The minimum allowed value is ' &
         //            ' -2*pi - TOL = # (radians), = # (deg).'    )
         call errdp  ( '#', inmax                                  )
         call errdp  ( '#', inmax * dpr()                          )
         call errdp  ( '#',   -pi2  - tol                          )
         call errdp  ( '#', ( -pi2  - tol ) * dpr()                )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                    )
         call chkout ( 'ZZNRMLON'                                  )
         return

      end if

      outmax = max( -pi2,  min( inmax, pi2 ) )

!
!     If the bounds are out of order, put them in order.
!     It is assumed that no interval has length zero.
!
!     If the upper bound is greater than the lower bound by
!     less than TOL, the bounds are considered to be "out of
!     order."
!
      if ( outmax <= touchd(outmin + tol) ) then
!
!        Shift one of the bounds by 2*pi, while keeping
!        the bounds in the range [-2pi, 2pi].
!
         if ( outmax <= 0.0_wp ) then
!
!           OUTMAX is non-positive. Shift it to the right.
!
            outmax = min ( touchd(outmax + pi2),  pi2 )

            if ( outmax < outmin ) then
!
!              If the bounds are still out of order, shift the lower
!              bound left.
!
               outmin = max ( touchd(outmin - pi2), -pi2 )

            end if

         else
!
!           OUTMAX is > 0. Shift the lower bound left.
!
            outmin = max ( touchd(outmin - pi2), -pi2 )

         end if

      end if

!
!     If the bounds are too far apart, move them together. Note
!     that OUTMIN and OUTMAX are already set at this point.
!
      delta = touchd( outmax - outmin )

      if (  delta  >  touchd( pi2 + tol )  ) then
!
!        Shift the upper bound lower by 2*pi.
!
         outmax = touchd( outmax - pi2 )

      end if


!
!     The output bounds must not be equal. We could end up with
!     equal output bounds if the input maximum is less than
!     the input minimum and the bounds differ by an integer
!     multiple of 2*pi.
!
      if ( outmin == outmax ) then

         call chkin  ( 'ZZNRMLON'                                     )
         call setmsg ( 'After adjustment, input longitude lower ' &
         //            'bound INMIN = # (radians),  = # (deg), ' &
         //            'is equal to adjusted longitude upper bound. ' &
         //            'Input upper bound = # (radians),  = # (deg). ' &
         //            'When the input upper bound is less than the ' &
         //            'input lower bound, the difference must not ' &
         //            'be an integer multiple of 2*pi.'              )
         call errdp  ( '#', inmin                                     )
         call errdp  ( '#', inmin * dpr()                             )
         call errdp  ( '#', inmax                                     )
         call errdp  ( '#', inmax * dpr()                             )
         call sigerr ( 'SPICE(ZEROBOUNDSEXTENT)'                      )
         call chkout ( 'ZZNRMLON'                                     )
         return

      end if

      end
