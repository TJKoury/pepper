!$Procedure  ZZCXBRUT ( Cone-segment intersection by brute force )

      subroutine zzcxbrut ( apex,   axis,   angle, &
                            endpt1, endpt2, xpt,   isbrck )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute a bracketed point of intersection of a specified nappe of
!     a cone and a line segment by "brute force"---specifically by
!     bisection.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONE
!     GEOMETRY
!     INTERSECTION
!     LINE
!     MATH
!     SEGMENT
!
! $ Declarations

      implicit none

      real(wp)              apex   ( 3 )
      real(wp)              axis   ( 3 )
      real(wp)              angle
      real(wp)              endpt1 ( 3 )
      real(wp)              endpt2 ( 3 )
      real(wp)              xpt    ( 3 )
      logical               isbrck

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     APEX       I   Apex of cone.
!     AXIS       I   Axis of cone.
!     ANGLE      I   Angle of cone.
!     ENDPT1,
!     ENDPT2     I   Endpoints of line segment.
!     XPT        O   Intersection point, if it exists.
!     ISBRCK     O   Logical flag indicating whether root is bracketed.
!
! $ Detailed_Input
!
!     APEX       is the apex (tip) of the cone. In this routine's
!                documentation, we'll consider the cone to be a
!                semi-infinite pyramid with circular cross-section. In
!                some contexts, this object is called one "nappe" of
!                the complete cone.
!
!     AXIS       is an axis vector of the cone.
!
!     ANGLE      is the angular separation from AXIS of the rays
!                comprising the cone. Let the notation
!
!                   < A, B >
!
!                denote the dot product of vectors A and B, and let
!
!                   ||A||
!
!                denote the norm of vector A. Then the cone is the set
!                of points
!
!                             X-APEX       AXIS
!                   { X:  < ----------,  -------- >  =  cos(ANGLE) }
!                           ||X-APEX||   ||AXIS||
!
!
!     ENDPT1,
!     ENDPT2     are endpoints of a line segment. These points
!                must be distinct.
!
!                Exactly one of ENDPT1 and ENDPT2 must be inside
!                the cone.
!
! $ Detailed_Output
!
!     NXPTS      is the number of points of intersection of the input
!                line segment and cone.
!
!     XPT        is the unique point of intersection of the segment and
!                cone that lies on the line segment connecting ENDPT1
!                and ENDPT2.
!
!     ISBRCK     is a logical flag that is set to .TRUE. if and only if
!                ENDPT1 and ENDPT2 bracket a root. Equivalently, the
!                endpoints bracket a root when exactly one of ENDPT1
!                and ENDPT2 is inside the cone.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR)
!         will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is used by the SPICELIB routine INCNSG to handle
!     cases where solution of a quadratic equation is subject to
!     excessive loss of precision.
!
! $ Examples
!
!     See usage in INCNSG.
!
! $ Restrictions
!
!     This is a private SPICELIB routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0 30-SEP-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     brute force intersection of line segment and cone
!
!-&







!
!     SPICELIB functions
!
      real(wp)              halfpi
      real(wp)              pi
      real(wp)              vdot

      logical               return
      logical               state1
      logical               state2
      logical               vzero

!
!     Local parameters
!
      real(wp)              cnvlim
      parameter           ( cnvlim = 1.0e-15_wp )

      integer(ip)           maxitr
      parameter           ( maxitr = 1000 )

!
!     Local variables
!
      real(wp)              cosang
      real(wp)              delta
      real(wp)              dp
      real(wp)              dp1
      real(wp)              dp2
      real(wp)              high
      real(wp)              locang
      real(wp)              locaxi ( 3 )
      real(wp)              low
      real(wp)              midpt
      real(wp)              off1   ( 3 )
      real(wp)              off2   ( 3 )
      real(wp)              prvdlt
      real(wp)              seg    ( 3 )
      real(wp)              uoff1  ( 3 )
      real(wp)              uoff2  ( 3 )
      real(wp)              ux     ( 3 )
      real(wp)              x      ( 3 )

      integer(ip)           nitr

      logical               state

!
!     Use discovery check-in.
!
      if ( return() ) return

!
!     Check the axis.
!
      if ( vzero(axis) ) then

         call setmsg ( 'Cone axis is the zero vector' )
         call sigerr ( 'SPICE(ZEROVECTOR)'            )
         return

      end if

!
!     Make a local version of the cone's axis and angle. The
!     angle will be less than or equal to pi/2 radians.
!
      if ( angle > halfpi() ) then

         locang = pi() - angle
         call vminus ( axis, locaxi )
      else
         locang = angle
         call vequ   ( axis, locaxi )
      end if

      call vhatip ( locaxi )

      cosang = cos( locang )

!
!     Calculate the offsets of the endpoints from the apex,
!     and get unit-length versions of these.
!
      call vsub ( endpt1, apex, off1 )
      call vsub ( endpt2, apex, off2 )

      call vhat ( off1, uoff1 )
      call vhat ( off2, uoff2 )

!
!     Get the dot products of the unit offsets with the axis.
!     These will serve as proxies for latitude.
!
      dp1 = vdot( uoff1, locaxi )
      dp2 = vdot( uoff2, locaxi )

!
!     The "state" variables at the endpoints are .TRUE. if
!     the endpoints are on or inside the cone.
!
      state1 = dp1 >= cosang
      state2 = dp2 >= cosang

!
!     The intersection is supposed to be bracketed. Return
!     if not, indicating the situation via ISBRCK.
!
      isbrck = state1 .neqv. state2

      if ( .not. isbrck) return

!
!     Prepare for a solution by bisection.
!
      call vsub ( off2, off1, seg )

      low    = 0.0_wp
      high   = 1.0_wp
      delta  = abs( high - low )
      prvdlt = 2.0_wp
      nitr   = 0

      do while (       ( delta > cnvlim ) &
                 .and. ( delta < prvdlt ) &
                 .and. ( nitr  < maxitr )  )

         midpt = ( low + high ) / 2

         call vlcom ( 1.0_wp, off1, midpt, seg, x )

         call vhat ( x, ux )

         dp    = vdot ( ux, locaxi )

         state = dp >= cosang

         if ( state .eqv. state1 ) then
!
!           There has been no state change between OFF1
!           and XPT.
!
            low  = midpt
         else
            high = midpt
         end if

         prvdlt = delta
         delta  = abs( high - low )

         nitr   = nitr + 1

      end do
!
!     X is an offset from APEX. The solution is an offset from the
!     origin.
!
      call vadd ( apex, x, xpt )

      return
      end



