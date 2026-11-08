C$Procedure F_PLTNP ( PLTNP tests )
 
      SUBROUTINE F_PLTNP ( OK )
 
C$ Abstract
C
C     Exercise the plate-point near point routine PLTNP.
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
 
      IMPLICIT NONE       
      
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
C     This routine tests the DSKLIB type 2 point-plate distance
C     routine PLTNP. 
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
C-    TSPICE Version 1.0.0, 18-AUG-2015 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

C
C     Local Parameters 
C
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

C
C     Local Variables 
C  
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

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C
      DATA                  NEXT / 2, 3, 1 /
      DATA                  PRED / 3, 1, 2 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_PLTNP' )

C**********************************************************************
C
C     Non-error exceptional cases
C
C**********************************************************************



C
C --- Case: ------------------------------------------------------
C 
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
      

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate case: plate is a single point. ' 
     .//       'Input point coincides with plate.'

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate case: plate is a line segment bounded by ' 
     .//       'V1 and V2.'

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate case: plate is a line segment '
     .//       'bounded by V1 and V2. '
     .//       'Input point is interior of segment. ' 

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate case: plate is a line segment bounded by ' 
     .//       'V1 and V3.'

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate case: plate is a line segment '
     .//       'bounded by V1 and V3. '
     .//       'Input point is interior of segment. ' 

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate case: plate is a line segment bounded by ' 
     .//       'V2 and V3.'

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Degenerate case: plate is a line segment '
     .//       'bounded by V2 and V3. '
     .//       'Input point is interior of segment. ' 

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



C**********************************************************************
C
C     Normal cases
C
C**********************************************************************
 

C
C
C     Simple tests with easily verified results:
C     
C

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: plate is in X-Y plane; point is ' 
     .//           'on +Z axis.' 

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


C
C     The following cases use the triangle defined above.
C

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is below '
     .//       '(in the -Y sense) the bottom '
     .//       'edge of the plate.' 

      CALL TCASE ( TITLE )

      CALL VPACK (  0.D0, -2.D0, 3.D0, POINT  )
      CALL VPACK (  0.D0, -1.D0, 0.D0, XPNEAR )

      XDIST = SQRT( 10.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is below the bottom '
     .//       'edge of the plate and shifted in the -X direction.' 

      CALL TCASE ( TITLE )

      CALL VPACK ( -2.D0, -2.D0, 3.D0, POINT  )
      CALL VEQU  ( V1,  XPNEAR )

      XDIST = SQRT( 11.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is below the bottom '
     .//       'edge of the plate and shifted in the +X direction.' 

      CALL TCASE ( TITLE )

      CALL VPACK (  2.D0, -2.D0, 3.D0, POINT  )
      CALL VEQU  ( V2,  XPNEAR )

      XDIST = SQRT( 11.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the +Y '
     .//       'sense) the bottom edge of the plate but below '
     .//       'outward normal to the left edge emanating from '
     .//       'the lower left corner.'

      CALL TCASE ( TITLE )

      CALL VPACK (  -4.D0, 0.D0, 3.D0, POINT  )
      CALL VEQU  ( V1,  XPNEAR )

      XDIST = SQRT( 19.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the +Y '
     .//       'sense) the bottom edge of the plate but below '
     .//       'outward normal to the right edge emanating from '
     .//       'the lower right corner.'

      CALL TCASE ( TITLE )

      CALL VPACK (   4.D0, 0.D0, 3.D0, POINT  )
      CALL VEQU  ( V2,  XPNEAR )

      XDIST = SQRT( 19.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is on '
     .//       'the outward normal to the left edge '
     .//       'emanating from the lower left corner.'

      CALL TCASE ( TITLE )

      CALL VPACK (  -3.D0, 0.D0, 3.D0, POINT  )
      CALL VEQU  ( V1,  XPNEAR )

      XDIST = SQRT( 14.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is on '
     .//       'the outward normal to the right edge '
     .//       'emanating from the lower right corner.'

      CALL TCASE ( TITLE )

      CALL VPACK (  3.D0, 0.D0, 3.D0, POINT  )
      CALL VEQU  ( V2,  XPNEAR )

      XDIST = SQRT( 14.D0 )

      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )

      
C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the +Y '
     .//       'sense) the outward normal to the left edge '
     .//       'emanating from the lower left corner.'

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

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the +Y '
     .//       'sense) the outward normal to the right edge '
     .//       'emanating from the lower right corner.'

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is on '
     .//       'the outward normal to the left edge '
     .//       'emanating from the top (in the +Y sense) vertex.'

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is on '
     .//       'the outward normal to the right edge '
     .//       'emanating from the top (in the +Y sense) vertex.'

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

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the +Y sense)'
     .//       'the outward normal to the left edge '
     .//       'emanating from the top (in the +Y sense) vertex.'

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


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the +Y sense)'
     .//       'the outward normal to the right edge '
     .//       'emanating from the top (in the +Y sense) vertex.'

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



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Exterior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the +Y sense)'
     .//       'the top vertex.'


      CALL TCASE ( TITLE )

      CALL VPACK ( 0.D0, 3.D0, 3.D0, POINT )

      CALL VEQU  ( V3,  XPNEAR )

      XDIST = SQRT( 13.D0 )


      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

      TOL = VTIGHT
      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


C
C     "Interior" cases follow. For these cases, the projection
C     of the point onto the plane containing the plate is a point
C     on the plate.
C



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Interior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is below (in the -Y sense)'
     .//       'the top vertex.'


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



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Interior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the -Y sense)'
     .//       'and to the right of the lower left vertex.'


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



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Interior case: plate is in X-Y plane; X-Y '
     .//       'plane projection of point is above (in the -Y sense)'
     .//       'and to the left of the lower right vertex.'


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





C
C
C
C     Systematic tests:
C
C
C


 
C
C --- Case --------------------------------------------------------
C
      TITLE = 'Setup'

      CALL TCASE ( TITLE )

C
C     Create basic shapes. All shapes are variants of triangles.
C
C        Shape 1 is an acute triangle.
C
      CALL VPACK ( -1.D0, -1.D0,  0.D0, SHAPES(1,1,1) )
      CALL VPACK (  1.D0, -1.D0,  0.D0, SHAPES(1,2,1) )
      CALL VPACK (  0.D0,  1.D0,  0.D0, SHAPES(1,3,1) )

C
C        Shape 2 is a right triangle.
C
      CALL VPACK ( -1.D0, -1.D0,  0.D0, SHAPES(1,1,2) )
      CALL VPACK (  1.D0, -1.D0,  0.D0, SHAPES(1,2,2) )
      CALL VPACK ( -1.D0,  1.D0,  0.D0, SHAPES(1,3,2) )

C
C        Shape 3 is an obtuse triangle.
C
      CALL VPACK ( -4.D0, -1.D0,  0.D0, SHAPES(1,1,3) )
      CALL VPACK (  4.D0, -1.D0,  0.D0, SHAPES(1,2,3) )
      CALL VPACK (  0.D0,  1.D0,  0.D0, SHAPES(1,3,3) )

C
C        Shape 4 is a needle-like, acute triangle.
C
      CALL VPACK ( -1.D0, -1.D0,  0.D0, SHAPES(1,1,4) )
      CALL VPACK (  1.D0, -1.D0,  0.D0, SHAPES(1,2,4) )
      CALL VPACK (  0.D0,  1.D4,  0.D0, SHAPES(1,3,4) )

C
C        Shape 5 is an almost flat, obtuse triangle.
C
      CALL VPACK ( -1.D4, -1.D0,  0.D0, SHAPES(1,1,5) )
      CALL VPACK (  1.D4, -1.D0,  0.D0, SHAPES(1,2,5) )
      CALL VPACK (  0.D0,  1.D0,  0.D0, SHAPES(1,3,5) )



C
C     Create an offset vector and a transformation; we'll use
C     these to move the shapes away from the origin and from
C     the X-Y plane.
C
      CALL VPACK ( -17.D0, 13.D0, 23.D0, OFFSET )


      CALL EUL2M ( 30.D0*RPD(), -10.D0*RPD(), 70.D0*RPD(), 
     .             3,           2,            3,           XFORM )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case --------------------------------------------------------
C


C
C     Loop over the triangles.
C
      DO SHAPIX = 1, NSHAPE
C
C        Loop over the scales.
C
         DO SCALIX = 1, NSCALE

C
C           Translate and transform the triangle.
C
            DO I = 1, 3

               CALL VADD ( OFFSET, SHAPES(1,I,SHAPIX), VTEMP       )
               CALL MXV  ( XFORM,  VTEMP,              XSHAPE(1,I) )

            END DO

            
            SCALE = 10.D0 ** (4.D0*(SCALIX-MINSCL))

C
C           Scale the triangle.
C
            DO I = 1, 3
               CALL VSCLIP( SCALE, XSHAPE(1,I) )
            END DO

C
C           Compute the triangle's edges.
C
            CALL VSUB ( XSHAPE(1,2), XSHAPE(1,1), EDGE(1,1) )
            CALL VSUB ( XSHAPE(1,3), XSHAPE(1,2), EDGE(1,2) )
            CALL VSUB ( XSHAPE(1,1), XSHAPE(1,3), EDGE(1,3) )

C
C           Compute a normal to the plane of the triangle. Pick the
C           normal so the vertices are ordered in the positive sense
C           about the normal.
C
            CALL UCRSS ( EDGE(1,1), EDGE(1,2), NORMAL )

C
C           Compute the outward normal vectors for the edges, where the
C           normal vectors lie in the plane of the triangle.
C
            CALL UCRSS ( EDGE(1,1), NORMAL, EDGNML(1,1) )
            CALL UCRSS ( EDGE(1,2), NORMAL, EDGNML(1,2) )
            CALL UCRSS ( EDGE(1,3), NORMAL, EDGNML(1,3) )

C
C           We're going to create points, the distance of which from
C           the current triangle is to be computed.
C
C           Loop over the point's height values.
C
            DO ALTIX = 1, NALT

               ALT = SCALE * ( 10.D0 ** DBLE(ALTIX-MINALT) )

C
C              Loop over the triangles' vertices.
C
               DO VIX = 1, 3
C
C                 Compute the angular separation between the
C                 rays emanating from vertex VIX in the directions
C                 of the outward normal to edge VIX and the 
C                 normal to edge PRED(VIX). We'll use this later.
C                 
                  SEP = VSEP ( EDGNML(1,VIX), EDGNML(1,PRED(VIX)) )
C
C                 Compute the length of the edge between vertex
C                 VIX and vertex NEXT(VIX). We'll use this later.
C                 
                  L = VNORM( EDGE(1,VIX) )


                  DO UIX = 0, NU
C
C                    UIX controls the angular separation of the point
C                    from the normal rays emanating from vertex VIX.
C
                     ANGLE = SEP * ( DBLE(UIX) / DBLE(NU) ) 

                     CALL VROTV ( EDGNML(1,PRED(VIX)), NORMAL,
     .                            ANGLE,               DIR    )
                     CALL CHCKXC ( .FALSE., ' ', OK )


                     DO RIX = 1, NR
 
C
C --- Case --------------------------------------------------------
C
C
C                       Create a point in the plane of the triangle,
C                       lying in the sector having apex at vertex VIX
C                       and bounded by rays emanating from the apex in
C                       directions EDGNML(1,VIX) and
C                       EDGNML(1,PRED(VIX)). For such a point, the
C                       nearest point on the triangle is the vertex
C                       itself. We'll then add an "altitude" to the
C                       point.
C
C                       In the case immediately below, RIX controls the
C                       distance of the point from the vertex.
C
                        R = SCALE * ( 10.D0 ** (2.D0*(RIX - MINR) ) )



                        TITLE = 'Near point should be vertex #: '
     .                  //      'SHAPIX = #; SCALIX = #; ALTIX = #; '
     .                  //      'VIX = #; UIX = #, RIX = #; '
     .                  //      'R   = #; ALT = #.'

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

C
C                       Create the point.
C
                        CALL VLCOM3 ( 1.D0, XSHAPE(1,VIX),
     .                                R,    DIR,
     .                                ALT,  NORMAL,       POINT )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       Compute the near point on the triangle and
C                       the distance between this point and POINT.
C                        
                        CALL PLTNP ( POINT,       XSHAPE(1,1), 
     .                               XSHAPE(1,2), XSHAPE(1,3),
     .                               PNEAR,       DIST        ) 
                        CALL CHCKXC ( .FALSE., ' ', OK )


C
C                       Set tolerances based on the numerical
C                       difficulty of the case.
C
                        IF ( SHAPIX .LE. 3 ) THEN
                        
                           TOL = TIGHT
                        ELSE
                           TOL = LOOSE
                        END IF

                        XDIST = VDIST ( XSHAPE(1,VIX), POINT )
                        
                        CALL CHCKSD ( 'DIST', DIST,  '~/', 
     .                                        XDIST, TOL,  OK )


                        IF ( SHAPIX .LE. 3 ) THEN

                           IF ( RIX .LE. NR-3 ) THEN
                              TOL = TIGHT
                           ELSE
                              TOL = MEDTOL
                           END IF

                        ELSE
                           
                           TOL = LOOSE

                        END IF

                        CALL CHCKAD ( 'PNEAR', PNEAR, '~~/',
     .                                 XSHAPE(1,VIX), 3,   TOL, OK )

C
C --- Case --------------------------------------------------------
C
C
C                       Test points whose near points lie on the
C                       edge between vertices VIX and NEXT(VIX).
C
C                       We can use the values of ALT and R we've
C                       already computed. R will be the distance
C                       from the edge of the projection of the 
C                       point onto the plane of the triangle.
C                       
C                       S will replace ANGLE in this computation: S
C                       will be the distance from the outward normal
C                       ray of the edge, emanating from vertex VIX,
C                       of the projection of the point onto the plane
C                       of the triangle.
C
                        S = L * ( DBLE(UIX) / DBLE(NU) )


                        TITLE = 'Near point should be on the edge: '
     .                  //      'SHAPIX = #; SCALIX = #; ALTIX = #; '
     .                  //      'VIX = #; UIX = #, RIX = #; '
     .                  //      'R   = #; S = #; ALT = #.'

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

C
C                       Generate the point.
C
C                       We'll need the unit vector in the direction
C                       of the edge.
C
                        CALL VHAT ( EDGE(1,VIX), UEDGE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       Create the point.
C
                        CALL VLCOM3 ( 1.D0, XSHAPE(1,VIX),
     .                                R,    EDGNML(1,VIX),
     .                                S,    UEDGE,         VTEMP )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL VLCOM  ( 1.D0, VTEMP, 
     .                                ALT,  NORMAL, POINT )


C
C                       Compute the near point on the triangle and
C                       the distance between this point and POINT.
C                        
                        CALL PLTNP ( POINT,       XSHAPE(1,1), 
     .                               XSHAPE(1,2), XSHAPE(1,3),
     .                               PNEAR,       DIST        ) 
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       The expected near point lies on the line
C                       containing the edge.
C
                        CALL NPLNPT ( XSHAPE(1,VIX), UEDGE, POINT,
     .                                XPNEAR,        XDIST        )


C
C                       Set tolerances based on the numerical
C                       difficulty of the case.
C
                        IF ( SHAPIX .LE. 3 ) THEN

                           IF (      ( RIX   .LE. NR-3   ) 
     .                         .AND. ( ALTIX .LE. NALT-3 ) ) THEN

                              TOL = MEDTOL
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE

                           TOL = LOOSE
 
                        END IF
                        
                        CALL CHCKSD ( 'DIST', DIST,  '~/', 
     .                                        XDIST, TOL,  OK )


                        IF ( SHAPIX .LE. 3 ) THEN

                           IF (      ( RIX   .LE. NR-3   ) 
     .                         .AND. ( ALTIX .LE. NALT-3 ) ) THEN

                              TOL = TIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE
                           
                           TOL = LOOSE

                        END IF


                        CALL CHCKAD ( 'PNEAR', PNEAR, '~~/',
     .                                 XPNEAR, 3,   TOL, OK )




C
C --- Case --------------------------------------------------------
C
C
C                       Test points whose near points lie in the
C                       interior of the triangle.
C
C                       We can use the values of ALT we've already
C                       computed. We'll create the projection of the
C                       point using linear combinations of the vertices.
C                      
                        TITLE = 'Near point should be in the '
     .                  //      'interior of the triangle: '
     .                  //      'SHAPIX = #; SCALIX = #; ALTIX = #; '
     .                  //      'VIX = #; UIX = #, RIX = #; ALT = #. '


                        CALL REPMI ( TITLE, '#', SHAPIX,      TITLE )
                        CALL REPMI ( TITLE, '#', SCALIX,      TITLE )
                        CALL REPMI ( TITLE, '#', ALTIX,       TITLE )
                        CALL REPMI ( TITLE, '#', VIX,         TITLE )
                        CALL REPMI ( TITLE, '#', UIX,         TITLE )
                        CALL REPMI ( TITLE, '#', RIX,         TITLE )
                        CALL REPMD ( TITLE, '#', ALT,    14,  TITLE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL TCASE ( TITLE )

                        
C
C                       S ranges from 0 to 1.
C
                        S = DBLE(UIX) / DBLE(NU)

C
C                       Make R range from 0 to 1.
C
                        R = DBLE(RIX-1) / DBLE(NR-1)


C
C                       Generate the projection of the point. This
C                       is the expected near point. Use a convex
C                       combination of the vertices.
C
                        CALL VLCOM3( 1.D0-S,     XSHAPE( 1, VIX       ),
     .                               S*(1.D0-R), XSHAPE( 1, NEXT(VIX) ),
     .                               S*R,        XSHAPE( 1, PRED(VIX) ),
     .                               XPNEAR                            )

C
C                       Generate the point.
C                       
                        CALL VLCOM ( 1.D0, XPNEAR, ALT, NORMAL, POINT )

C
C                       Compute the near point on the triangle and
C                       the distance between this point and POINT.
C                        
                        CALL PLTNP ( POINT,       XSHAPE(1,1), 
     .                               XSHAPE(1,2), XSHAPE(1,3),
     .                               PNEAR,       DIST        ) 
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       The expected distance is just the altitude
C                       of the point with respect to the plane 
C                       containing the triangle.
C
                        XDIST = ALT

C
C                       Set tolerances based on the numerical
C                       difficulty of the case.
C
                        IF ( SHAPIX .LE. 3 ) THEN

                           IF (      ( RIX   .LE. NR-3   ) 
     .                         .AND. ( ALTIX .LE. NALT-3 ) ) THEN

                              TOL = MEDTOL
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE

                           TOL = LOOSE
 
                        END IF
                        
                        CALL CHCKSD ( 'DIST', DIST,  '~/', 
     .                                        XDIST, TOL,  OK )

 
                        IF ( SHAPIX .LE. 3 ) THEN

                           IF (      ( RIX   .LE. NR-3   ) 
     .                         .AND. ( ALTIX .LE. NALT-3 ) ) THEN

                              TOL = TIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE
                           
                           TOL = LOOSE

                        END IF


                        CALL CHCKAD ( 'PNEAR', PNEAR, '~~/',
     .                                 XPNEAR, 3,   TOL, OK )

                     END DO
C
C                    End of "RIX" loop.
C
                  END DO
C
C                 End of "UIX" loop.
C
               END DO
C
C              End of vertex loop.
C
            END DO
C
C           End of altitude loop.
C
         END DO
C
C        End of scale loop.
C
      END DO
C
C     End of shape loop.
C
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


