!$Procedure F_PLTNP ( PLTNP tests )

      SUBROUTINE F_PLTNP ( OK )

! $ Abstract
!
!     Exercise the plate-point near point routine PLTNP.
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
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

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
!     This routine tests the DSKLIB type 2 point-plate distance
!     routine PLTNP.
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
!-    TSPICE Version 1.0.0, 18-AUG-2015 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 1.D-9 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               NSHAPE
      PARAMETER           ( NSHAPE = 5 )

      INTEGER               NALT
      PARAMETER           ( NALT   = 5 )

      INTEGER               MINALT
      PARAMETER           ( MINALT = 2 )

      INTEGER               NSCALE
      PARAMETER           ( NSCALE = 8 )

      INTEGER               MINSCL
      PARAMETER           ( MINSCL = 4  )

      INTEGER               NU
      PARAMETER           ( NU     = 5 )

      INTEGER               NR
      PARAMETER           ( NR     = 6 )

      INTEGER               MINR
      PARAMETER           ( MINR   = 3 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      EDGE   ( 3, 3 )
      DOUBLE PRECISION      EDGNML ( 3, 3 )
      DOUBLE PRECISION      L
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SHAPES ( 3, 3, NSHAPE )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      UEDGE  ( 3 )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      V3     ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XPNEAR ( 3 )
      DOUBLE PRECISION      XSHAPE ( 3, 3 )

      INTEGER               ALTIX
      INTEGER               I
      INTEGER               NEXT   ( 3 )
      INTEGER               PRED   ( 3 )
      INTEGER               RIX
      INTEGER               SCALIX
      INTEGER               SHAPIX
      INTEGER               UIX
      INTEGER               VIX

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!
      DATA                  NEXT / 2, 3, 1 /
      DATA                  PRED / 3, 1, 2 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_PLTNP' )

!**********************************************************************
!
!     Non-error exceptional cases
!
!**********************************************************************



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate case: plate is a single point.'

      CALL TCASE ( TITLE )

      CALL VPACK ( 3.D0, 4.D0, 5.D0, V1 )
      CALL VEQU  ( V1, V2 )
      CALL VEQU  ( V1, V3 )

      CALL VPACK (  0.D0,  0.D0, 5.D0, POINT  )

      CALL VEQU ( V1, XPNEAR )
      XDIST = 5.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate case: plate is a single point. ' &
      //       'Input point coincides with plate.'

      CALL TCASE ( TITLE )

      CALL VPACK ( 3.D0, 4.D0, 5.D0, V1 )
      CALL VEQU  ( V1, V2 )
      CALL VEQU  ( V1, V3 )

      CALL VEQU  ( V1, POINT )

      CALL VEQU ( V1, XPNEAR )
      XDIST = 0.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate case: plate is a line segment bounded by ' &
      //       'V1 and V2.'

      CALL TCASE ( TITLE )

      CALL VPACK ( 3.D0, 4.D0,  5.D0, V1 )
      CALL VPACK ( 6.D0, 8.D0, 10.D0, V2 )

      CALL VLCOM (  0.5D0, V1,  0.5D0, V2, V3  )


      CALL VPACK (  0.D0,  0.D0, 5.D0, POINT  )

      CALL VEQU ( V1, XPNEAR )
      XDIST = 5.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate case: plate is a line segment ' &
      //       'bounded by V1 and V2. ' &
      //       'Input point is interior of segment. '

      CALL TCASE ( TITLE )

      CALL VPACK ( 3.D0, 4.D0,  5.D0, V1 )
      CALL VPACK ( 6.D0, 8.D0, 10.D0, V2 )

      CALL VEQU  ( V2, V3 )

      CALL VLCOM (  0.5D0, V1,  0.5D0, V2, POINT  )

      CALL VEQU ( POINT, XPNEAR )
      XDIST = 0.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate case: plate is a line segment bounded by ' &
      //       'V1 and V3.'

      CALL TCASE ( TITLE )

      CALL VPACK ( 3.D0, 4.D0,  5.D0, V1 )
      CALL VPACK ( 6.D0, 8.D0, 10.D0, V3 )

      CALL VLCOM (  0.5D0, V1,  0.5D0, V3, V2  )


      CALL VPACK (  0.D0,  0.D0, 5.D0, POINT  )

      CALL VEQU ( V1, XPNEAR )
      XDIST = 5.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate case: plate is a line segment ' &
      //       'bounded by V1 and V3. ' &
      //       'Input point is interior of segment. '

      CALL TCASE ( TITLE )

      CALL VPACK ( 3.D0, 4.D0,  5.D0, V1 )
      CALL VPACK ( 6.D0, 8.D0, 10.D0, V3 )

      CALL VLCOM (  0.5D0, V1,  0.5D0, V3, V2  )

      CALL VLCOM (  0.5D0, V1,  0.5D0, V2, POINT  )

      CALL VEQU ( POINT, XPNEAR )
      XDIST = 0.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate case: plate is a line segment bounded by ' &
      //       'V2 and V3.'

      CALL TCASE ( TITLE )

      CALL VPACK ( 3.D0, 4.D0,  5.D0, V2 )
      CALL VPACK ( 6.D0, 8.D0, 10.D0, V3 )

      CALL VLCOM (  0.5D0, V2,  0.5D0, V3, V1  )


      CALL VPACK (  0.D0,  0.D0, 5.D0, POINT  )

      CALL VEQU ( V2, XPNEAR )
      XDIST = 5.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate case: plate is a line segment ' &
      //       'bounded by V2 and V3. ' &
      //       'Input point is interior of segment. '

      CALL TCASE ( TITLE )

      CALL VPACK ( 3.D0, 4.D0,  5.D0, V2 )
      CALL VPACK ( 6.D0, 8.D0, 10.D0, V3 )

      CALL VLCOM (  0.5D0, V2,  0.5D0, V3, V1  )

      CALL VLCOM (  0.5D0, V2,  0.5D0, V3, POINT  )

      CALL VEQU ( POINT, XPNEAR )
      XDIST = 0.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
!
!     Simple tests with easily verified results:
!
!

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Trivial case: plate is in X-Y plane; point is ' &
      //           'on +Z axis.'

      CALL TCASE ( TITLE )

      CALL VPACK ( -1.D0, -1.D0, 0.D0, V1 )
      CALL VPACK (  1.D0, -1.D0, 0.D0, V2 )
      CALL VPACK (  0.D0,  1.D0, 0.D0, V3 )

      CALL VPACK (  0.D0,  0.D0, 3.D0, POINT  )
      CALL VPACK (  0.D0,  0.D0, 0.D0, XPNEAR )

      XDIST = 3.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
!     The following cases use the triangle defined above.
!

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is below ' &
      //       '(in the -Y sense) the bottom ' &
      //       'edge of the plate.'

      CALL TCASE ( TITLE )

      CALL VPACK (  0.D0, -2.D0, 3.D0, POINT  )
      CALL VPACK (  0.D0, -1.D0, 0.D0, XPNEAR )

      XDIST = SQRT( 10.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is below the bottom ' &
      //       'edge of the plate and shifted in the -X direction.'

      CALL TCASE ( TITLE )

      CALL VPACK ( -2.D0, -2.D0, 3.D0, POINT  )
      CALL VEQU  ( V1,  XPNEAR )

      XDIST = SQRT( 11.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is below the bottom ' &
      //       'edge of the plate and shifted in the +X direction.'

      CALL TCASE ( TITLE )

      CALL VPACK (  2.D0, -2.D0, 3.D0, POINT  )
      CALL VEQU  ( V2,  XPNEAR )

      XDIST = SQRT( 11.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the +Y ' &
      //       'sense) the bottom edge of the plate but below ' &
      //       'outward normal to the left edge emanating from ' &
      //       'the lower left corner.'

      CALL TCASE ( TITLE )

      CALL VPACK (  -4.D0, 0.D0, 3.D0, POINT  )
      CALL VEQU  ( V1,  XPNEAR )

      XDIST = SQRT( 19.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the +Y ' &
      //       'sense) the bottom edge of the plate but below ' &
      //       'outward normal to the right edge emanating from ' &
      //       'the lower right corner.'

      CALL TCASE ( TITLE )

      CALL VPACK (   4.D0, 0.D0, 3.D0, POINT  )
      CALL VEQU  ( V2,  XPNEAR )

      XDIST = SQRT( 19.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is on ' &
      //       'the outward normal to the left edge ' &
      //       'emanating from the lower left corner.'

      CALL TCASE ( TITLE )

      CALL VPACK (  -3.D0, 0.D0, 3.D0, POINT  )
      CALL VEQU  ( V1,  XPNEAR )

      XDIST = SQRT( 14.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is on ' &
      //       'the outward normal to the right edge ' &
      //       'emanating from the lower right corner.'

      CALL TCASE ( TITLE )

      CALL VPACK (  3.D0, 0.D0, 3.D0, POINT  )
      CALL VEQU  ( V2,  XPNEAR )

      XDIST = SQRT( 14.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the +Y ' &
      //       'sense) the outward normal to the left edge ' &
      //       'emanating from the lower left corner.'

      CALL TCASE ( TITLE )

      CALL VPACK ( 0.5D0, 1.D0, 0.D0, OFFSET )

      CALL VADD  ( V1, OFFSET, XPNEAR )

      CALL VPACK ( -1.D0, 0.5D0, 0.D0, NORMAL )

      CALL VLCOM3 ( 1.D0, V1, 1.D0, OFFSET, 2.D0, NORMAL, POINT )

      POINT(3) = 3.D0

      XDIST    = SQRT( 14.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the +Y ' &
      //       'sense) the outward normal to the right edge ' &
      //       'emanating from the lower right corner.'

      CALL TCASE ( TITLE )

      CALL VPACK ( -0.5D0, 1.D0, 0.D0, OFFSET )

      CALL VADD  ( V2, OFFSET, XPNEAR )

      CALL VPACK ( 1.D0, 0.5D0, 0.D0, NORMAL )

      CALL VLCOM3 ( 1.D0, V2, 1.D0, OFFSET, 2.D0, NORMAL, POINT )

      POINT(3) = 3.D0

      XDIST    = SQRT( 14.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is on ' &
      //       'the outward normal to the left edge ' &
      //       'emanating from the top (in the +Y sense) vertex.'

      CALL TCASE ( TITLE )

      CALL VEQU  ( V3,  XPNEAR )

      CALL VPACK ( -1.D0, 0.5D0, 0.D0, NORMAL )

      CALL VLCOM ( 1.D0, V3, 2.D0, NORMAL, POINT )

      POINT(3) = 3.D0

      XDIST    = SQRT( 14.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is on ' &
      //       'the outward normal to the right edge ' &
      //       'emanating from the top (in the +Y sense) vertex.'

      CALL TCASE ( TITLE )

      CALL VEQU  ( V3,  XPNEAR )

      CALL VPACK ( 1.D0, 0.5D0, 0.D0, NORMAL )

      CALL VLCOM ( 1.D0, V3, 2.D0, NORMAL, POINT )

      POINT(3) = 3.D0

      XDIST    = SQRT( 14.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the +Y sense)' &
      //       'the outward normal to the left edge ' &
      //       'emanating from the top (in the +Y sense) vertex.'

      CALL TCASE ( TITLE )

      CALL VEQU  ( V3,  XPNEAR )

      CALL VPACK ( -1.D0, 0.5D0, 0.D0, NORMAL )

      CALL VLCOM ( 1.D0, V3, 2.D0, NORMAL, POINT )

      POINT(2) = POINT(2) + 1.D0
      POINT(3) = 3.D0

      XDIST    = SQRT( 17.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the +Y sense)' &
      //       'the outward normal to the right edge ' &
      //       'emanating from the top (in the +Y sense) vertex.'

      CALL TCASE ( TITLE )

      CALL VEQU  ( V3,  XPNEAR )

      CALL VPACK ( 1.D0, 0.5D0, 0.D0, NORMAL )

      CALL VLCOM ( 1.D0, V3, 2.D0, NORMAL, POINT )

      POINT(2) = POINT(2) + 1.D0
      POINT(3) = 3.D0

      XDIST    = SQRT( 17.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the +Y sense)' &
      //       'the top vertex.'


      CALL TCASE ( TITLE )

      CALL VPACK ( 0.D0, 3.D0, 3.D0, POINT )

      CALL VEQU  ( V3,  XPNEAR )

      XDIST = SQRT( 13.D0 )


      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
!     "Interior" cases follow. For these cases, the projection
!     of the point onto the plane containing the plate is a point
!     on the plate.
!



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Interior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is below (in the -Y sense)' &
      //       'the top vertex.'


      CALL TCASE ( TITLE )

      CALL VPACK ( 0.D0, 0.9D0, 3.D0, POINT )

      CALL VEQU  ( POINT,  XPNEAR )
      XPNEAR(3) = 0.D0

      XDIST = 3.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Interior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the -Y sense)' &
      //       'and to the right of the lower left vertex.'


      CALL TCASE ( TITLE )

      CALL VPACK ( 0.1D0, 0.1D0, 3.D0, OFFSET )

      CALL VADD ( V1, OFFSET, POINT )

      CALL VEQU  ( POINT,  XPNEAR )
      XPNEAR(3) = 0.D0

      XDIST = 3.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Interior case: plate is in X-Y plane; X-Y ' &
      //       'plane projection of point is above (in the -Y sense)' &
      //       'and to the left of the lower right vertex.'


      CALL TCASE ( TITLE )

      CALL VPACK ( -0.1D0, 0.1D0, 3.D0, OFFSET )

      CALL VADD ( V2, OFFSET, POINT )

      CALL VEQU  ( POINT,  XPNEAR )
      XPNEAR(3) = 0.D0

      XDIST = 3.D0

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )





!
!
!
!     Systematic tests:
!
!
!



!
! --- Case --------------------------------------------------------
!
      TITLE = 'Setup'

      CALL TCASE ( TITLE )

!
!     Create basic shapes. All shapes are variants of triangles.
!
!        Shape 1 is an acute triangle.
!
      CALL VPACK ( -1.D0, -1.D0,  0.D0, SHAPES(1,1,1) )
      CALL VPACK (  1.D0, -1.D0,  0.D0, SHAPES(1,2,1) )
      CALL VPACK (  0.D0,  1.D0,  0.D0, SHAPES(1,3,1) )

!
!        Shape 2 is a right triangle.
!
      CALL VPACK ( -1.D0, -1.D0,  0.D0, SHAPES(1,1,2) )
      CALL VPACK (  1.D0, -1.D0,  0.D0, SHAPES(1,2,2) )
      CALL VPACK ( -1.D0,  1.D0,  0.D0, SHAPES(1,3,2) )

!
!        Shape 3 is an obtuse triangle.
!
      CALL VPACK ( -4.D0, -1.D0,  0.D0, SHAPES(1,1,3) )
      CALL VPACK (  4.D0, -1.D0,  0.D0, SHAPES(1,2,3) )
      CALL VPACK (  0.D0,  1.D0,  0.D0, SHAPES(1,3,3) )

!
!        Shape 4 is a needle-like, acute triangle.
!
      CALL VPACK ( -1.D0, -1.D0,  0.D0, SHAPES(1,1,4) )
      CALL VPACK (  1.D0, -1.D0,  0.D0, SHAPES(1,2,4) )
      CALL VPACK (  0.D0,  1.D4,  0.D0, SHAPES(1,3,4) )

!
!        Shape 5 is an almost flat, obtuse triangle.
!
      CALL VPACK ( -1.D4, -1.D0,  0.D0, SHAPES(1,1,5) )
      CALL VPACK (  1.D4, -1.D0,  0.D0, SHAPES(1,2,5) )
      CALL VPACK (  0.D0,  1.D0,  0.D0, SHAPES(1,3,5) )



!
!     Create an offset vector and a transformation; we'll use
!     these to move the shapes away from the origin and from
!     the X-Y plane.
!
      CALL VPACK ( -17.D0, 13.D0, 23.D0, OFFSET )


      CALL EUL2M ( 30.D0*RPD(), -10.D0*RPD(), 70.D0*RPD(), &
                   3,           2,            3,           XFORM )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!


!
!     Loop over the triangles.
!
      DO SHAPIX = 1, NSHAPE
!
!        Loop over the scales.
!
         DO SCALIX = 1, NSCALE

!
!           Translate and transform the triangle.
!
            DO I = 1, 3

               CALL VADD ( OFFSET, SHAPES(1,I,SHAPIX), VTEMP       )
               CALL MXV  ( XFORM,  VTEMP,              XSHAPE(1,I) )

            END DO


            SCALE = 10.D0 ** (4.D0*(SCALIX-MINSCL))

!
!           Scale the triangle.
!
            DO I = 1, 3
               CALL VSCLIP( SCALE, XSHAPE(1,I) )
            END DO

!
!           Compute the triangle's edges.
!
            CALL VSUB ( XSHAPE(1,2), XSHAPE(1,1), EDGE(1,1) )
            CALL VSUB ( XSHAPE(1,3), XSHAPE(1,2), EDGE(1,2) )
            CALL VSUB ( XSHAPE(1,1), XSHAPE(1,3), EDGE(1,3) )

!
!           Compute a normal to the plane of the triangle. Pick the
!           normal so the vertices are ordered in the positive sense
!           about the normal.
!
            CALL UCRSS ( EDGE(1,1), EDGE(1,2), NORMAL )

!
!           Compute the outward normal vectors for the edges, where the
!           normal vectors lie in the plane of the triangle.
!
            CALL UCRSS ( EDGE(1,1), NORMAL, EDGNML(1,1) )
            CALL UCRSS ( EDGE(1,2), NORMAL, EDGNML(1,2) )
            CALL UCRSS ( EDGE(1,3), NORMAL, EDGNML(1,3) )

!
!           We're going to create points, the distance of which from
!           the current triangle is to be computed.
!
!           Loop over the point's height values.
!
            DO ALTIX = 1, NALT

               ALT = SCALE * ( 10.D0 ** DBLE(ALTIX-MINALT) )

!
!              Loop over the triangles' vertices.
!
               DO VIX = 1, 3
!
!                 Compute the angular separation between the
!                 rays emanating from vertex VIX in the directions
!                 of the outward normal to edge VIX and the
!                 normal to edge PRED(VIX). We'll use this later.
!
                  SEP = VSEP ( EDGNML(1,VIX), EDGNML(1,PRED(VIX)) )
!
!                 Compute the length of the edge between vertex
!                 VIX and vertex NEXT(VIX). We'll use this later.
!
                  L = VNORM( EDGE(1,VIX) )


                  DO UIX = 0, NU
!
!                    UIX controls the angular separation of the point
!                    from the normal rays emanating from vertex VIX.
!
                     ANGLE = SEP * ( DBLE(UIX) / DBLE(NU) )

                     CALL VROTV ( EDGNML(1,PRED(VIX)), NORMAL, &
                                  ANGLE,               DIR    )
                     CALL CHCKXC ( .FALSE., ' ', OK )


                     DO RIX = 1, NR

!
! --- Case --------------------------------------------------------
!
!
!                       Create a point in the plane of the triangle,
!                       lying in the sector having apex at vertex VIX
!                       and bounded by rays emanating from the apex in
!                       directions EDGNML(1,VIX) and
!                       EDGNML(1,PRED(VIX)). For such a point, the
!                       nearest point on the triangle is the vertex
!                       itself. We'll then add an "altitude" to the
!                       point.
!
!                       In the case immediately below, RIX controls the
!                       distance of the point from the vertex.
!
                        R = SCALE * ( 10.D0 ** (2.D0*(RIX - MINR) ) )



                        TITLE = 'Near point should be vertex #: ' &
                        //      'SHAPIX = #; SCALIX = #; ALTIX = #; ' &
                        //      'VIX = #; UIX = #, RIX = #; ' &
                        //      'R   = #; ALT = #.'

                        CALL REPMI ( TITLE, '#', VIX,         TITLE )
                        CALL REPMI ( TITLE, '#', SHAPIX,      TITLE )
                        CALL REPMI ( TITLE, '#', SCALIX,      TITLE )
                        CALL REPMI ( TITLE, '#', ALTIX,       TITLE )
                        CALL REPMI ( TITLE, '#', VIX,         TITLE )
                        CALL REPMI ( TITLE, '#', UIX,         TITLE )
                        CALL REPMI ( TITLE, '#', RIX,         TITLE )
                        CALL REPMD ( TITLE, '#', R,      14,  TITLE )
                        CALL REPMD ( TITLE, '#', ALT,    14,  TITLE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL TCASE ( TITLE )

!
!                       Create the point.
!
                        CALL VLCOM3 ( 1.D0, XSHAPE(1,VIX), &
                                      R,    DIR, &
                                      ALT,  NORMAL,       POINT )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       Compute the near point on the triangle and
!                       the distance between this point and POINT.
!
                        CALL PLTNP ( POINT,       XSHAPE(1,1), &
                                     XSHAPE(1,2), XSHAPE(1,3), &
                                     PNEAR,       DIST        )
                        CALL CHCKXC ( .FALSE., ' ', OK )


!
!                       Set tolerances based on the numerical
!                       difficulty of the case.
!
                        IF ( SHAPIX .LE. 3 ) THEN

                           TOL = TIGHT
                        ELSE
                           TOL = LOOSE
                        END IF

                        XDIST = VDIST ( XSHAPE(1,VIX), POINT )

                        CALL CHCKSD ( 'DIST', DIST,  '~/', &
                                              XDIST, TOL,  OK )


                        IF ( SHAPIX .LE. 3 ) THEN

                           IF ( RIX .LE. NR-3 ) THEN
                              TOL = TIGHT
                           ELSE
                              TOL = MEDTOL
                           END IF

                        ELSE

                           TOL = LOOSE

                        END IF

                        CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', &
                                       XSHAPE(1,VIX), 3,   TOL, OK )

!
! --- Case --------------------------------------------------------
!
!
!                       Test points whose near points lie on the
!                       edge between vertices VIX and NEXT(VIX).
!
!                       We can use the values of ALT and R we've
!                       already computed. R will be the distance
!                       from the edge of the projection of the
!                       point onto the plane of the triangle.
!
!                       S will replace ANGLE in this computation: S
!                       will be the distance from the outward normal
!                       ray of the edge, emanating from vertex VIX,
!                       of the projection of the point onto the plane
!                       of the triangle.
!
                        S = L * ( DBLE(UIX) / DBLE(NU) )


                        TITLE = 'Near point should be on the edge: ' &
                        //      'SHAPIX = #; SCALIX = #; ALTIX = #; ' &
                        //      'VIX = #; UIX = #, RIX = #; ' &
                        //      'R   = #; S = #; ALT = #.'

                        CALL REPMI ( TITLE, '#', SHAPIX,      TITLE )
                        CALL REPMI ( TITLE, '#', SCALIX,      TITLE )
                        CALL REPMI ( TITLE, '#', ALTIX,       TITLE )
                        CALL REPMI ( TITLE, '#', VIX,         TITLE )
                        CALL REPMI ( TITLE, '#', UIX,         TITLE )
                        CALL REPMI ( TITLE, '#', RIX,         TITLE )
                        CALL REPMD ( TITLE, '#', R,      14,  TITLE )
                        CALL REPMD ( TITLE, '#', S,      14,  TITLE )
                        CALL REPMD ( TITLE, '#', ALT,    14,  TITLE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL TCASE ( TITLE )

!
!                       Generate the point.
!
!                       We'll need the unit vector in the direction
!                       of the edge.
!
                        CALL VHAT ( EDGE(1,VIX), UEDGE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       Create the point.
!
                        CALL VLCOM3 ( 1.D0, XSHAPE(1,VIX), &
                                      R,    EDGNML(1,VIX), &
                                      S,    UEDGE,         VTEMP )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL VLCOM  ( 1.D0, VTEMP, &
                                      ALT,  NORMAL, POINT )


!
!                       Compute the near point on the triangle and
!                       the distance between this point and POINT.
!
                        CALL PLTNP ( POINT,       XSHAPE(1,1), &
                                     XSHAPE(1,2), XSHAPE(1,3), &
                                     PNEAR,       DIST        )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       The expected near point lies on the line
!                       containing the edge.
!
                        CALL NPLNPT ( XSHAPE(1,VIX), UEDGE, POINT, &
                                      XPNEAR,        XDIST        )


!
!                       Set tolerances based on the numerical
!                       difficulty of the case.
!
                        IF ( SHAPIX .LE. 3 ) THEN

                           IF (      ( RIX   .LE. NR-3   ) &
                               .AND. ( ALTIX .LE. NALT-3 ) ) THEN

                              TOL = MEDTOL
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE

                           TOL = LOOSE

                        END IF

                        CALL CHCKSD ( 'DIST', DIST,  '~/', &
                                              XDIST, TOL,  OK )


                        IF ( SHAPIX .LE. 3 ) THEN

                           IF (      ( RIX   .LE. NR-3   ) &
                               .AND. ( ALTIX .LE. NALT-3 ) ) THEN

                              TOL = TIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE

                           TOL = LOOSE

                        END IF


                        CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', &
                                       XPNEAR, 3,   TOL, OK )




!
! --- Case --------------------------------------------------------
!
!
!                       Test points whose near points lie in the
!                       interior of the triangle.
!
!                       We can use the values of ALT we've already
!                       computed. We'll create the projection of the
!                       point using linear combinations of the vertices.
!
                        TITLE = 'Near point should be in the ' &
                        //      'interior of the triangle: ' &
                        //      'SHAPIX = #; SCALIX = #; ALTIX = #; ' &
                        //      'VIX = #; UIX = #, RIX = #; ALT = #. '


                        CALL REPMI ( TITLE, '#', SHAPIX,      TITLE )
                        CALL REPMI ( TITLE, '#', SCALIX,      TITLE )
                        CALL REPMI ( TITLE, '#', ALTIX,       TITLE )
                        CALL REPMI ( TITLE, '#', VIX,         TITLE )
                        CALL REPMI ( TITLE, '#', UIX,         TITLE )
                        CALL REPMI ( TITLE, '#', RIX,         TITLE )
                        CALL REPMD ( TITLE, '#', ALT,    14,  TITLE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL TCASE ( TITLE )


!
!                       S ranges from 0 to 1.
!
                        S = DBLE(UIX) / DBLE(NU)

!
!                       Make R range from 0 to 1.
!
                        R = DBLE(RIX-1) / DBLE(NR-1)


!
!                       Generate the projection of the point. This
!                       is the expected near point. Use a convex
!                       combination of the vertices.
!
                        CALL VLCOM3( 1.D0-S,     XSHAPE( 1, VIX       ), &
                                     S*(1.D0-R), XSHAPE( 1, NEXT(VIX) ), &
                                     S*R,        XSHAPE( 1, PRED(VIX) ), &
                                     XPNEAR                            )

!
!                       Generate the point.
!
                        CALL VLCOM ( 1.D0, XPNEAR, ALT, NORMAL, POINT )

!
!                       Compute the near point on the triangle and
!                       the distance between this point and POINT.
!
                        CALL PLTNP ( POINT,       XSHAPE(1,1), &
                                     XSHAPE(1,2), XSHAPE(1,3), &
                                     PNEAR,       DIST        )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       The expected distance is just the altitude
!                       of the point with respect to the plane
!                       containing the triangle.
!
                        XDIST = ALT

!
!                       Set tolerances based on the numerical
!                       difficulty of the case.
!
                        IF ( SHAPIX .LE. 3 ) THEN

                           IF (      ( RIX   .LE. NR-3   ) &
                               .AND. ( ALTIX .LE. NALT-3 ) ) THEN

                              TOL = MEDTOL
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE

                           TOL = LOOSE

                        END IF

                        CALL CHCKSD ( 'DIST', DIST,  '~/', &
                                              XDIST, TOL,  OK )


                        IF ( SHAPIX .LE. 3 ) THEN

                           IF (      ( RIX   .LE. NR-3   ) &
                               .AND. ( ALTIX .LE. NALT-3 ) ) THEN

                              TOL = TIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE

                           TOL = LOOSE

                        END IF


                        CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', &
                                       XPNEAR, 3,   TOL, OK )

                     END DO
!
!                    End of "RIX" loop.
!
                  END DO
!
!                 End of "UIX" loop.
!
               END DO
!
!              End of vertex loop.
!
            END DO
!
!           End of altitude loop.
!
         END DO
!
!        End of scale loop.
!
      END DO
!
!     End of shape loop.
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


