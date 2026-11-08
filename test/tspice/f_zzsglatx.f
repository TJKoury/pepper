C$Procedure      F_ZZSGLATX ( Test segment latitude extent routine )
 
      SUBROUTINE F_ZZSGLATX ( OK )
      IMPLICIT NONE

C$ Abstract
C
C     Test the SPICELIB line segment latitude extent computation
C     routine ZZSGLATX.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the routine ZZSGLATX.
C        
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 31-AUG-2016 (NJB)
C
C        19-MAY-2013 (NJB)
C
C           Bugs in calls to CHCKAD were corrected. Errors in cases 4
C           and 6 were corrected.
C
C        04-DEC-2012 (NJB)
C     
C           Original version.
C
C-&

C$ Revisions
C
C        19-MAY-2013 (NJB)
C
C           Bugs in calls to CHCKAD were corrected. Errors in cases 4
C           and 6 were corrected.
C
C           There was an error in the title of case 4. In case 4, there
C           was a single precision constant used as an input argument
C           to VLCOM.
C
C           In case 6, XMAXP was not initialized correctly.
C
C-& 


C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT
C
C     Local parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

C      INTEGER               LNSIZE
C      PARAMETER           ( LNSIZE = 80 )

C
C     Local Variables
C       
C      CHARACTER*(LNSIZE)    CASNAM

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

C      INTEGER               I
C      INTEGER               N

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZSGLATX' )

 

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Tangent case: segment parallel to X-Y plane, Z>0' )
 
      SQ2  = SQRT( 2.D0 )
      SQ22 = SQ2 / 2.D0

      CALL VPACK (  1.D0,  -SQ2,  1.D0,  P1 )
      CALL VPACK (  1.D0,   SQ2,  1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude should occur at the segment midpoint.
C
      CALL VPACK ( 1.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

C
C     The minimum latitude occurs at an endpoint, but which one is
C     not specified. We use the sign of the Y component of MINP
C     to indicate which endpoint was selected.
C     
      CALL VEQU ( P1, XMINP )

      IF ( MINP(2) .GT. 0.D0 ) THEN

         XMINP(2) = -XMINP(2)

      END IF
      
      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

C
C     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
C
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Tangent case: segment parallel to X-Y plane, ' 
     .//           'Z>0, segment max Y is at X-Z plane.'          )
 

      CALL VPACK (  1.D0,  -SQ2,  1.D0,  P1 )
      CALL VPACK (  1.D0,  0.D0,  1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude should occur at the segment midpoint.
C
      CALL VPACK ( 1.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

C
C     The minimum latitude occurs at the endpoint where Y is
C     minimized.
C     
      CALL VEQU ( P1, XMINP )
      
      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

C
C     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
C
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Tangent case: segment parallel to X-Y plane, ' 
     .//           'Z>0, segment min Y is at X-Z plane.'          )
 

      CALL VPACK (  1.D0,  0.D0,  1.D0,  P1 )
      CALL VPACK (  1.D0,  SQ2,   1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude should occur at the segment starting
C     point.
C
      CALL VPACK ( 1.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

C
C     The minimum latitude occurs at the endpoint where Y is
C     maximized.
C     
      CALL VEQU ( P2, XMINP )
      
      CALL CHCKAD ( 'MINP', MINP, '~~/',  XMINP, 3, VTIGHT, OK )

C
C     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
C
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



C
C --- Case --------------------------------------------------------
C

C
C     Adding multiples of the tangent point to the line segment
C     generates new segments in the same tangent plane. Try such
C     a case.
C
      CALL TCASE ( 'Tangent case: segment non parallel to X-Y plane, ' 
     .//           'Z>0, segment min Y is negative.'                 )

      CALL VPACK ( 1.D0, -SQ2, 1.D0, P1 )

      CALL VPACK ( 3.D0,  SQ2, 3.D0, P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude should occur at the segment midpoint.
C
      CALL VLCOM ( 0.5D0, P1, 0.5D0, P2, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )   

C
C     The minimum latitude will occur at the endpoint at which Y
C     is minimum.
C   
      CALL VEQU ( P1, XMINP )
      
      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

C
C     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
C
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Tangent case: segment parallel to X-Y plane, ' 
     .//           'Z>0, segment max Y is at X-Z plane.'          )
 

      CALL VPACK (  1.D0,  -SQ2,  1.D0,  P1 )
      CALL VPACK (  1.D0,  0.D0,  1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude should occur at the segment midpoint.
C
      CALL VPACK ( 1.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

C
C     The minimum latitude occurs at the endpoint where Y is
C     minimized.
C     
      CALL VEQU ( P1, XMINP )
      
      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

C
C     The minimum latitude has sine = 1/2, so the latitude is 30 deg.
C
      XMINLT = PI()/6

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Tangent *miss* case: segment parallel ' 
     .//           'to X-Y plane, Z>0, segment max Y < 0.' )
 

      CALL VPACK (  1.D0,  -2.D0,  1.D0,  P1 )
      CALL VPACK (  1.D0,  -SQ2,   1.D0,  P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude should occur at P2.
C
      CALL VEQU ( P2, XMAXP)

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

      XMAXLT = PI()/6

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

C
C     The minimum latitude occurs at the endpoint where Y is
C     minimized.
C     
      CALL VEQU ( P1, XMINP )
      
      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

C
C     The minimum latitude has sine = 1/sqrt(6).
C
      XMINLT = ASIN( 1.D0/SQRT(6.D0) )

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'P2 = 2 * P1' )

      CALL VPACK ( SQ22, -SQ22, 1.D0, P1 )
      CALL VSCL  ( 2.D0, P1,          P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude will be found somewhere on the
C     line segment. Find the nearest point to MAXP on the line
C     containing the segment; make sure this point is actually
C     on the segment.
C
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )
     
      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

C
C     The same conditions apply to MINP.
C
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

      
C
C     Latitude is pi/4 all along this segment.
C           
      XMAXLT = PI()/4      

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = PI()/4      

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'P2 = - P1' )

      CALL VPACK  ( SQ22, -SQ22, 1.D0, P1 )
      CALL VMINUS ( P1,                P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude will be found somewhere on the
C     line segment. Find the nearest point to MAXP on the line
C     containing the segment; make sure this point is actually
C     on the segment.
C
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )
     
      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

C
C     The same conditions apply to MINP.
C
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

      
C
C     Latitude is pi/4 on portion of this segment above the plane Z=0,
C     -pi/4 below it.
C           
      XMAXLT = PI()/4      

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = -PI()/4      

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Segment is in X-Y plane.' )

      CALL VPACK  ( SQ22, -SQ22, 0.D0, P1 )
      CALL VPACK  ( SQ22,  SQ22, 0.D0, P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude will be found somewhere on the
C     line segment. Find the nearest point to MAXP on the line
C     containing the segment; make sure this point is actually
C     on the segment.
C
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )
     
      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

C
C     The same conditions apply to MINP.
C
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

      
C
C     Latitude is 0 on the whole segment.
C           
      XMAXLT = 0.D0

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = 0.D0      

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Segment is contained in +Z axis' )

      CALL VPACK  ( 0.D0,  0.D0, 1.D0, P2 )
      CALL VPACK  ( 0.D0,  0.D0, 2.D0, P1 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude will be found somewhere on the
C     line segment. Find the nearest point to MAXP on the line
C     containing the segment; make sure this point is actually
C     on the segment.
C
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )
     
      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

C
C     The same conditions apply to MINP.
C
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

      
C
C     Latitude is pi/2 on the whole segment.
C           
      XMAXLT = PI()/2

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = PI()/2    

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Segment is contained in -Z axis' )

      CALL VPACK  ( 0.D0,  0.D0, -1.D0, P1 )
      CALL VPACK  ( 0.D0,  0.D0, -2.D0, P2 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude will be found somewhere on the
C     line segment. Find the nearest point to MAXP on the line
C     containing the segment; make sure this point is actually
C     on the segment.
C
      CALL VSUB  ( P2,   P1,  DIFF )
      CALL UNORM ( DIFF, DIR, MAG  )
     
      CALL NPLNPT ( P1, DIR, MAXP, XMAXP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MAXP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

C
C     The same conditions apply to MINP.
C
      CALL NPLNPT ( P1, DIR, MINP, XMINP, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DIST', DIST, '~', 0.D0, TIGHT, OK )

      CALL VSUB ( MINP, P1, DIFF )

      DP = VDOT ( DIFF, DIR )

      CALL CHCKSD ( 'DP', DP, '>',    -VTIGHT, 0.D0, OK )
      CALL CHCKSD ( 'DP', DP, '<', MAG+VTIGHT, 0.D0, OK )

      
C
C     Latitude is -pi/2 on the whole segment.
C           
      XMAXLT = -PI()/2

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

      XMINLT = -PI()/2    

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Segment intersects but is not contained '
     .//           'in +Z axis' )

      CALL VPACK  (  1.D0,  0.D0, 1.D0, P2 )
      CALL VPACK  ( -1.D0,  0.D0, 1.D0, P1 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The maximum latitude will be found on the Z axis.
C
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XMAXP )

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )

C
C     The minimum latitude will be found at one of the 
C     segment's endpoints.
C
      CALL VEQU ( P1, XMINP )

      IF ( MINP(1) .GT. 0.D0 ) THEN
         XMINP(1) = -XMINP(1)
      END IF

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )
      
C
C     Latitude is pi/2 at the maximum.
C           
      XMAXLT = PI()/2

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )

C
C     Latitude is pi/4 at the minimum.
C
      XMINLT = PI()/4

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Segment intersects but is not contained '
     .//           'in -Z axis' )

      CALL VPACK  (  1.D0,  0.D0, -1.D0, P2 )
      CALL VPACK  ( -1.D0,  0.D0, -1.D0, P1 )

      CALL ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The minimum latitude will be found on the Z axis.
C
      CALL VPACK ( 0.D0, 0.D0, -1.D0, XMINP )

      CALL CHCKAD ( 'MINP', MINP, '~~/', XMINP, 3, VTIGHT, OK )

C
C     The maximum latitude will be found at one of the 
C     segment's endpoints.
C
      CALL VEQU ( P1, XMAXP )

      IF ( MAXP(1) .GT. 0.D0 ) THEN
         XMAXP(1) = -XMAXP(1)
      END IF

      CALL CHCKAD ( 'MAXP', MAXP, '~~/', XMAXP, 3, VTIGHT, OK )
      
C
C     Latitude is -pi/2 at the minimum.
C           
      XMINLT = -PI()/2

      CALL CHCKSD ( 'MINLAT', MINLAT, '~', XMINLT, VTIGHT, OK )

C
C     Latitude is -pi/4 at the maximum.
C
      XMAXLT = -PI()/4

      CALL CHCKSD ( 'MAXLAT', MAXLAT, '~', XMAXLT, VTIGHT, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
