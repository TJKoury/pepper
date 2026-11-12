!$Procedure      F_ZZSGLATX ( Test segment latitude extent routine )

      SUBROUTINE F_ZZSGLATX ( OK )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Test the SPICELIB line segment latitude extent computation
!     routine ZZSGLATX.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the routine ZZSGLATX.
!
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 31-AUG-2016 (NJB)
!
!        19-MAY-2013 (NJB)
!
!           Bugs in calls to CHCKAD were corrected. Errors in cases 4
!           and 6 were corrected.
!
!        04-DEC-2012 (NJB)
!
!           Original version.
!
!-&

! $ Revisions
!
!        19-MAY-2013 (NJB)
!
!           Bugs in calls to CHCKAD were corrected. Errors in cases 4
!           and 6 were corrected.
!
!           There was an error in the title of case 4. In case 4, there
!           was a single precision constant used as an input argument
!           to VLCOM.
!
!           In case 6, XMAXP was not initialized correctly.
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!
!
!     Local parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

!      INTEGER               LNSIZE
!      PARAMETER           ( LNSIZE = 80 )

!
!     Local Variables
!
!      CHARACTER*(LNSIZE)    CASNAM

      DOUBLE PRECISION      DIFF   ( 3 )
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXP   ( 3 )
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINP   ( 3 )
      DOUBLE PRECISION      P1     ( 3 )
      DOUBLE PRECISION      P2     ( 3 )
      DOUBLE PRECISION      SQ2
      DOUBLE PRECISION      SQ22
      DOUBLE PRECISION      XMAXLT
      DOUBLE PRECISION      XMAXP  ( 3 )
      DOUBLE PRECISION      XMINLT
      DOUBLE PRECISION      XMINP  ( 3 )

!      INTEGER               I
!      INTEGER               N

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZSGLATX' )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Tangent case: segment parallel to X-Y plane, Z>0' )

      SQ2  = SQRT( 2.D0 )
      SQ22 = SQ2 / 2.D0

      CALL VPACK (  1.D0,  -SQ2,  1.D0,  P1 )
      CALL VPACK (  1.D0,   SQ2,  1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude should occur at the segment midpoint.
!
      CALL VPACK ( 1.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

!
!     The minimum latitude occurs at an endpoint, but which one is
!     not specified. We use the sign of the Y component of MINP
!     to indicate which endpoint was selected.
!
      CALL VEQU ( P1, XMINP )

      IF ( MINP(2) .GT. 0.D0 ) THEN

         XMINP(2) = -XMINP(2)

      END IF

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

!
!     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
!
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Tangent case: segment parallel to X-Y plane, ' &
      //           'Z>0, segment max Y is at X-Z plane.'          )


      CALL VPACK (  1.D0,  -SQ2,  1.D0,  P1 )
      CALL VPACK (  1.D0,  0.D0,  1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude should occur at the segment midpoint.
!
      CALL VPACK ( 1.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

!
!     The minimum latitude occurs at the endpoint where Y is
!     minimized.
!
      CALL VEQU ( P1, XMINP )

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

!
!     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
!
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Tangent case: segment parallel to X-Y plane, ' &
      //           'Z>0, segment min Y is at X-Z plane.'          )


      CALL VPACK (  1.D0,  0.D0,  1.D0,  P1 )
      CALL VPACK (  1.D0,  SQ2,   1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude should occur at the segment starting
!     point.
!
      CALL VPACK ( 1.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

!
!     The minimum latitude occurs at the endpoint where Y is
!     maximized.
!
      CALL VEQU ( P2, XMINP )

      CALL CHCKAD ( 'MINP', MINP, '~~/',  XMINP, 3, VTIGHT, OK )

!
!     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
!
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



!
! --- Case --------------------------------------------------------
!

!
!     Adding multiples of the tangent point to the line segment
!     generates new segments in the same tangent plane. Try such
!     a case.
!
      CALL TCASE ( 'Tangent case: segment non parallel to X-Y plane, ' &
      //           'Z>0, segment min Y is negative.'                 )

      CALL VPACK ( 1.D0, -SQ2, 1.D0, P1 )

      CALL VPACK ( 3.D0,  SQ2, 3.D0, P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude should occur at the segment midpoint.
!
      CALL VLCOM ( 0.5D0, P1, 0.5D0, P2, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

!
!     The minimum latitude will occur at the endpoint at which Y
!     is minimum.
!
      CALL VEQU ( P1, XMINP )

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

!
!     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
!
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Tangent case: segment parallel to X-Y plane, ' &
      //           'Z>0, segment max Y is at X-Z plane.'          )


      CALL VPACK (  1.D0,  -SQ2,  1.D0,  P1 )
      CALL VPACK (  1.D0,  0.D0,  1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude should occur at the segment midpoint.
!
      CALL VPACK ( 1.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

!
!     The minimum latitude occurs at the endpoint where Y is
!     minimized.
!
      CALL VEQU ( P1, XMINP )

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

!
!     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
!
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Tangent *miss* case: segment parallel ' &
      //           'to X-Y plane, Z>0, segment max Y < 0.' )


      CALL VPACK (  1.D0,  -2.D0,  1.D0,  P1 )
      CALL VPACK (  1.D0,  -SQ2,   1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude should occur at P2.
!
      CALL VEQU ( P2, XMAXP)

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/6

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

!
!     The minimum latitude occurs at the endpoint where Y is
!     minimized.
!
      CALL VEQU ( P1, XMINP )

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

!
!     The minimum latitude has sine = 1/sqrt(6).
!
      XMINLT = ASIN( 1.D0/SQRT(6.D0) )

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'P2 = 2 * P1' )

      CALL VPACK ( SQ22, -SQ22, 1.D0, P1 )
      CALL VSCL  ( 2.D0, P1,          P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude will be found somewhere on the
!     line segment. Find the nearest point to MAXP on the line
!     containing the segment; make sure this point is actually
!     on the segment.
!
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )

      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

!
!     The same conditions apply to MINP.
!
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )


!
!     Latitude is pi/4 all along this segment.
!
      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = PI()/4

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'P2 = - P1' )

      CALL VPACK  ( SQ22, -SQ22, 1.D0, P1 )
      CALL VMINUS ( P1,                P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude will be found somewhere on the
!     line segment. Find the nearest point to MAXP on the line
!     containing the segment; make sure this point is actually
!     on the segment.
!
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )

      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

!
!     The same conditions apply to MINP.
!
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )


!
!     Latitude is pi/4 on portion of this segment above the plane Z=0,
!     -pi/4 below it.
!
      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = -PI()/4

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Segment is in X-Y plane.' )

      CALL VPACK  ( SQ22, -SQ22, 0.D0, P1 )
      CALL VPACK  ( SQ22,  SQ22, 0.D0, P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude will be found somewhere on the
!     line segment. Find the nearest point to MAXP on the line
!     containing the segment; make sure this point is actually
!     on the segment.
!
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )

      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

!
!     The same conditions apply to MINP.
!
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )


!
!     Latitude is 0 on the whole segment.
!
      XMAXLT = 0.D0

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = 0.D0

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Segment is contained in +Z axis' )

      CALL VPACK  ( 0.D0,  0.D0, 1.D0, P2 )
      CALL VPACK  ( 0.D0,  0.D0, 2.D0, P1 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude will be found somewhere on the
!     line segment. Find the nearest point to MAXP on the line
!     containing the segment; make sure this point is actually
!     on the segment.
!
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )

      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

!
!     The same conditions apply to MINP.
!
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )


!
!     Latitude is pi/2 on the whole segment.
!
      XMAXLT = PI()/2

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = PI()/2

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Segment is contained in -Z axis' )

      CALL VPACK  ( 0.D0,  0.D0, -1.D0, P1 )
      CALL VPACK  ( 0.D0,  0.D0, -2.D0, P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude will be found somewhere on the
!     line segment. Find the nearest point to MAXP on the line
!     containing the segment; make sure this point is actually
!     on the segment.
!
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )

      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

!
!     The same conditions apply to MINP.
!
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )


!
!     Latitude is -pi/2 on the whole segment.
!
      XMAXLT = -PI()/2

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = -PI()/2

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Segment intersects but is not contained ' &
      //           'in +Z axis' )

      CALL VPACK  (  1.D0,  0.D0, 1.D0, P2 )
      CALL VPACK  ( -1.D0,  0.D0, 1.D0, P1 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The maximum latitude will be found on the Z axis.
!
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

!
!     The minimum latitude will be found at one of the
!     segment's endpoints.
!
      CALL VEQU ( P1, XMINP )

      IF ( MINP(1) .GT. 0.D0 ) THEN
         XMINP(1) = -XMINP(1)
      END IF

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

!
!     Latitude is pi/2 at the maximum.
!
      XMAXLT = PI()/2

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

!
!     Latitude is pi/4 at the minimum.
!
      XMINLT = PI()/4

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Segment intersects but is not contained ' &
      //           'in -Z axis' )

      CALL VPACK  (  1.D0,  0.D0, -1.D0, P2 )
      CALL VPACK  ( -1.D0,  0.D0, -1.D0, P1 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The minimum latitude will be found on the Z axis.
!
      CALL VPACK ( 0.D0, 0.D0, -1.D0, XMINP )

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

!
!     The maximum latitude will be found at one of the
!     segment's endpoints.
!
      CALL VEQU ( P1, XMAXP )

      IF ( MAXP(1) .GT. 0.D0 ) THEN
         XMAXP(1) = -XMAXP(1)
      END IF

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

!
!     Latitude is -pi/2 at the minimum.
!
      XMINLT = -PI()/2

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

!
!     Latitude is -pi/4 at the maximum.
!
      XMAXLT = -PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
