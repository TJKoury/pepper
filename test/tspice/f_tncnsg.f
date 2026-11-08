C$Procedure F_TNCNSG ( T_INCNSG tests )
 
      SUBROUTINE F_TNCNSG ( OK )
 
C$ Abstract
C
C     Exercise the line segment-cone intersection routine T_INCNSG.
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
C     This routine tests the line segment-cone intersection
C     routine T_INCNSG. 
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
C-    TSPICE Version 1.0.0, 23-AUG-2016 (NJB)
C
C        Added test cases.
C
C        18-MAR-2016 (NJB)
C
C           Removed unneeded declarations.
C
C        24-SEP-2014 (NJB)
C
C           Original version.
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP


C
C     Local Parameters 
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-10 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-9 )

C
C     Local Variables 
C  
      CHARACTER*(LNSIZE)    TITLE
  

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      CONDIR ( 3 )
      DOUBLE PRECISION      CONPT1 ( 3 )
      DOUBLE PRECISION      CONPT2 ( 3 )
      DOUBLE PRECISION      CONVTX ( 3 )
      DOUBLE PRECISION      CONX   ( 3 )
      DOUBLE PRECISION      CONZ   ( 3 )
      DOUBLE PRECISION      DALPHA
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      DSPAN
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      EP1    ( 3 )
      DOUBLE PRECISION      EP1OFF ( 3 )
      DOUBLE PRECISION      EP2    ( 3 )
      DOUBLE PRECISION      EP2OFF ( 3 )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SPAN
      DOUBLE PRECISION      TARG   ( 3 )
      DOUBLE PRECISION      TARGY
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      XVEC   ( 3 )
      DOUBLE PRECISION      XXPT1  ( 3 )
      DOUBLE PRECISION      XXPT2  ( 3 )  
      DOUBLE PRECISION      XZ
      DOUBLE PRECISION      YVEC   ( 3 )
      DOUBLE PRECISION      Z

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NCONE
      INTEGER               NLAT
      INTEGER               NSPAN
      INTEGER               NXPTS
      INTEGER               XNXPTS

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_TNCNSG' )




C**********************************************************************
C
C     Error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Axis is zero vector.' )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, AXIS )
 
      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 )

      CALL VPACK ( -1.D0, 0.D0, XZ/2, ENDPT1 )
      CALL VPACK ( -1.D0, 0.D0, 2.D0, ENDPT2 )

      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Angular radius of cone is negative.' )

 

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = -1.D-12

      XZ    = SQRT( 3.D0 )

      CALL VPACK ( -1.D0, 0.D0, XZ/2, ENDPT1 )
      CALL VPACK ( -1.D0, 0.D0, 2.D0, ENDPT2 )

      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDANGLE)', OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Line segment has zero length.' )



      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 )

      CALL VPACK ( -1.D0, 0.D0, XZ/2, ENDPT1 )
      CALL VEQU  ( ENDPT1, ENDPT2 )

      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .TRUE., 'SPICE(ENDPOINTSMATCH)', OK )



C
C     To be completed:
C
C --- Case: ------------------------------------------------------
C 
C      TITLE =  'Coefficients have absolute values that are too large.'
C
C      CALL TCASE ( TITLE )





C**********************************************************************
C
C     Non-error exceptional cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Cone is the half-space {Z>=0}. Segment is '
     .//           'vertical and intersects cone.'             )

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = 90.D0 * RPD()

 
      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0,  3.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )
      
      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )       

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )
      
      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Cone is the half-space {Z>=0}. Segment is '
     .//           'vertical and misses cone.'                )
 
      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0, -2.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space '
     .//           '{Z>=0}; cone angle is pi/2 - 1.e-9. Segment is '
     .//           'vertical and intersects cone.'                  )

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = ( 90.D0 * RPD() ) - 1.D-9

 
      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0,  3.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )
      
      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )
      
      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space '
     .//           '{Z>=0}; cone angle is pi/2 - 1.e-9. Segment is '
     .//           'vertical and misses cone.'                      )
 
      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0, -2.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space '
     .//           '{ X: <X-APEX,AXIS>  >  0}; '
     .//           'cone angle is pi/2 - 1.e-9. Segment is '
     .//           'parallel to AXIS and intersects cone.' )


      CALL VPACK ( 100.D0, -200.D0, 300.D0, APEX )

      CALL VPACK  ( -7.D0, 5.D0, 1.D0, AXIS )
 
      CALL FRAME ( AXIS, XVEC, YVEC )

      ANGLE = ( 90.D0 * RPD() ) - 1.D-9

 
      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, -3.D0, AXIS, VTEMP  )

      CALL VADD ( VTEMP, APEX, ENDPT1 )

      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC,  3.D0, AXIS, VTEMP )
      
      CALL VADD ( VTEMP, APEX, ENDPT2 )



      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (up)', NXPTS, '=', 1, 0, OK )

      Z = SQRT(5.D0) * TAN( HALFPI() - ANGLE )


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )
      
      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (up)', XPT1, '~~/', XXPT1, 3, TOL, OK )

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (down)', NXPTS, '=', 1, 0, OK )


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, -Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )

      
      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (down)', XPT1, '~~/', XXPT1, 3, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space '
     .//           '{ X: <X-APEX,AXIS>  <  0}; '
     .//           'cone angle is pi/2 + 1.e-9. Segment is '
     .//           'parallel to AXIS and intersects cone.' )


      CALL VPACK ( 100.D0, -200.D0, 300.D0, APEX )

      CALL VPACK  ( -7.D0, 5.D0, 1.D0, AXIS )
 
      CALL FRAME ( AXIS, XVEC, YVEC )

      ANGLE = ( 90.D0 * RPD() ) + 1.D-9

 
      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, -3.D0, AXIS, VTEMP  )

      CALL VADD ( VTEMP, APEX, ENDPT1 )

      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC,  3.D0, AXIS, VTEMP )
      
      CALL VADD ( VTEMP, APEX, ENDPT2 )



      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (up)', NXPTS, '=', 1, 0, OK )

      Z = SQRT(5.D0) * TAN( HALFPI() - ANGLE )


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )
      
      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (up)', XPT1, '~~/', XXPT1, 3, TOL, OK )

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (down)', NXPTS, '=', 1, 0, OK )


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, -Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )

      
      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (down)', XPT1, '~~/', XXPT1, 3, TOL, OK )

        
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space '
     .//           '{ X: <X-APEX,AXIS>  <  0}; '
     .//           'cone angle is pi/2 - 1.e-9. Segment is '
     .//           'orthogonal to cone''s axis and intersects cone '
     .//           'twice.'                                       )


      CALL VPACK ( 100.D0, -200.D0, 300.D0, APEX )

      CALL VPACK  ( -7.D0, 5.D0, 1.D0, AXIS )
 
      CALL FRAME ( AXIS, XVEC, YVEC )

      ANGLE = ( 90.D0 * RPD() ) - 1.D-9

C
C     Z is the magnitude of the projections of the endpoints
C     onto the cone's axis.
C
      Z = 1.D-10
      
      CALL VLCOM3 (  1.D0, XVEC, -2.D0, YVEC, Z, AXIS, VTEMP )      
      CALL VADD ( VTEMP, APEX, ENDPT1 )

      CALL VLCOM3 ( -1.D0, XVEC,  2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (1)', NXPTS, '=', 2, 0, OK )

       

C
C     Compute expected value of XPT1.
C
      R = SQRT(5.D0)
      Z = 1.D-10
      S = Z / TAN( ABS(HALFPI()-ANGLE) )


      CALL VLCOM3 (  1.D0*S/R, XVEC, -2.D0*S/R, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )
      

      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (1)', XPT1, '~~/', XXPT1, 3, TOL, OK )

      CALL VLCOM3 ( -1.D0*S/R, XVEC,  2.D0*S/R, YVEC, Z, AXIS, VTEMP  )
      CALL VADD ( VTEMP, APEX, XXPT2 )
      

      TOL = 1.D-7

      CALL CHCKAD ( 'XPT2 (1)', XPT2, '~~/', XXPT2, 3, TOL, OK )

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (2)', NXPTS, '=', 2, 0, OK )


      CALL VLCOM3 ( -1.D0*S/R, XVEC,  2.D0*S/R, YVEC, Z, AXIS, VTEMP  )
      CALL VADD ( VTEMP, APEX, XXPT1 )

      
      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (2)', XPT1, '~~/', XXPT1, 3, TOL, OK )

      CALL VLCOM3 (  1.D0*S/R, XVEC, -2.D0*S/R, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT2 )

      CALL CHCKAD ( 'XPT2 (2)', XPT2, '~~/', XXPT2, 3, TOL, OK )
       


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space '
     .//           '{ X: <X-APEX,AXIS>  <  0}; '
     .//           'cone angle is pi/2 + 1.e-9. Segment is '
     .//           'orthogonal to cone''s axis and intersects cone '
     .//           'twice.'                                       )


      CALL VPACK ( 100.D0, -200.D0, 300.D0, APEX )

      CALL VPACK  ( -7.D0, 5.D0, 1.D0, AXIS )
 
      CALL FRAME ( AXIS, XVEC, YVEC )

      ANGLE = ( 90.D0 * RPD() ) + 1.D-9

C
C     Z is the magnitude of the projections of the endpoints
C     onto the cone's axis.
C
      Z = -1.D-10
      
      CALL VLCOM3 (  1.D0, XVEC, -2.D0, YVEC, Z, AXIS, VTEMP )      
      CALL VADD ( VTEMP, APEX, ENDPT1 )

      CALL VLCOM3 ( -1.D0, XVEC,  2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (1)', NXPTS, '=', 2, 0, OK )

       

C
C     Compute expected value of XPT1.
C
      R = SQRT(5.D0)
      Z = 1.D-10
      S = ABS(Z) / TAN( ABS(HALFPI()-ANGLE) )


      CALL VLCOM3 (  1.D0*S/R, XVEC, -2.D0*S/R, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )
      

      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (1)', XPT1, '~~/', XXPT1, 3, TOL, OK )

      CALL VLCOM3 ( -1.D0*S/R, XVEC,  2.D0*S/R, YVEC, Z, AXIS, VTEMP  )
      CALL VADD ( VTEMP, APEX, XXPT2 )
      

      TOL = 1.D-7

      CALL CHCKAD ( 'XPT2 (1)', XPT2, '~~/', XXPT2, 3, TOL, OK )

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (2)', NXPTS, '=', 2, 0, OK )


      CALL VLCOM3 ( -1.D0*S/R, XVEC,  2.D0*S/R, YVEC, Z, AXIS, VTEMP  )
      CALL VADD ( VTEMP, APEX, XXPT1 )

      
      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (2)', XPT1, '~~/', XXPT1, 3, TOL, OK )

      CALL VLCOM3 (  1.D0*S/R, XVEC, -2.D0*S/R, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT2 )

      CALL CHCKAD ( 'XPT2 (2)', XPT2, '~~/', XXPT2, 3, TOL, OK )
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Cone is ALMOST the half-space {Z>=0}; '
     .//           'cone angle is pi/2 - 1.e-15. Segment is '
     .//           'vertical and intersects cone.'             )

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = ( 90.D0 * RPD() ) - 1.D-15

 
      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0,  3.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )
      
      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )
      
      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Cone is ALMOST the half-space {Z>=0}; '
     .//           'cone angle is pi/2 - 1.e-15. Segment is '
     .//           'vertical and misses cone.'             )
 
      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0, -2.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Cone is the half-space {X<=5}. Segment is '
     .//           'parallel to X axis and intersects cone.'   )

      CALL VPACK  (  5.D0, 3.D0, 4.D0, APEX )
      CALL VPACK  ( -1.D0, 0.D0, 0.D0, AXIS )
 
      ANGLE = 90.D0 * RPD()

 
      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK (  6.D0, 2.D0, -3.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( 5.D0, 2.D0, -3.D0, XXPT1 )
      
      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( 5.D0, 2.D0, -3.D0, XXPT1 )
      
      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Cone is the half-space {X<=5}. Segment is '
     .//           'parallel to X axis and misses cone.'   )

      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -4.D0, 2.D0, -3.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Cone is ALMOST the half-space {Z>=0}; '
     .//           'cone angle is pi/2 - 1.e-7. Segment is '
     .//           'vertical and intersects cone.'             )

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = ( 90.D0 * RPD() ) - 1.D-7
 
      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0,  3.D0, ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( -1.D0, 2.D0, TAN(HALFPI()-ANGLE)*SQRT(5.D0), XXPT1 )
      
      TOL = 1.D-8

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

C
C     Repeat call with segment endpoints reversed.
C
      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .              ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )
      
      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )



C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: cone apex is origin; axis is +Z; '
     .//       'angular radius is 30 degrees; segment''s '
     .//       'first endpoint is (-1,0,sqrt(3)/2); second endpoint is '
     .//       '(0,0,sqrt(3)/2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0, 0.D0, XZ, ENDPT1 )
      CALL VPACK (  0.D0, 0.D0, XZ, ENDPT2 )

      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect one point of intersection to be found.
C
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )
 
      IF ( OK ) THEN

         CALL VPACK  ( -0.5D0, 0.D0, XZ, XXPT1 )
         CALL CLEARD ( 3, XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )
         
      END IF


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: cone apex is origin; axis is +Z; '
     .//       'angular radius is 30 degrees; segment''s '
     .//       'first endpoint is (-1,0,sqrt(3)/2); second endpoint is '
     .//       '(-1,0,2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 )

      CALL VPACK ( -1.D0, 0.D0, XZ/2, ENDPT1 )
      CALL VPACK ( -1.D0, 0.D0, 2.D0, ENDPT2 )

      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect one point of intersection to be found.
C
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )
 
      IF ( OK ) THEN

         CALL VPACK  ( -1.D0, 0.D0, XZ, XXPT1 )
         CALL CLEARD ( 3, XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )
         
      END IF


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: cone apex is origin; axis is +Z; '
     .//       'angular radius is 30 degrees; segment''s '
     .//       'first endpoint is (-1,0,sqrt(3)/2); second endpoint is '
     .//       '(1,0,sqrt(3)/2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0, 0.D0, XZ, ENDPT1 )
      CALL VPACK (  1.D0, 0.D0, XZ, ENDPT2 )

      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect two points of intersection to be found.
C
      XNXPTS = 2

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )
 
      IF ( OK ) THEN

         CALL VPACK ( -0.5D0, 0.D0, XZ, XXPT1 )
         CALL VPACK (  0.5D0, 0.D0, XZ, XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )
         CALL CHCKAD ( 'XPT2', XPT2, '~~/', XXPT2, 3, VTIGHT, OK )
         
      END IF


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Repeat previous case but offset both the apex '
     .//       'and ray''s vertex.'

      CALL TCASE ( TITLE )


      CALL VPACK  ( 3.D0, 4.D0, -5.D0, APEX )
 
      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0,  0.D0, XZ, ENDPT1 )
      CALL VADD  ( ENDPT1, APEX,     VTEMP  )
      CALL VEQU  ( VTEMP,            ENDPT1 )

      CALL VPACK (  1.D0, 0.D0, XZ, ENDPT2 )
      CALL VADD  ( ENDPT2, APEX,     VTEMP  )
      CALL VEQU  ( VTEMP,            ENDPT2 )


      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect two points of intersection to be found.
C
      XNXPTS = 2

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )
 
      IF ( OK ) THEN

         CALL VPACK ( -0.5D0, 0.D0, XZ, XXPT1 )
         CALL VADD  ( XXPT1,  APEX,     VTEMP )
         CALL VEQU  ( VTEMP,            XXPT1 )

         CALL VPACK (  0.5D0, 0.D0, XZ, XXPT2 )
         CALL VADD  ( XXPT2,  APEX,     VTEMP )
         CALL VEQU  ( VTEMP,            XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )
         CALL CHCKAD ( 'XPT2', XPT2, '~~/', XXPT2, 3, VTIGHT, OK )
         
      END IF


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Cone angle exceeds pi/2: cone apex is origin; axis '
     .//       'is +Z; angular radius is 150 degrees; segment''s '
     .//       'first endpoint is (-1,0,-sqrt(3)/2); second endpoint '
     .//       'is (1,0,-sqrt(3)/2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )
 
      ANGLE = 150.D0 * RPD()

      XZ    = -SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0, 0.D0, XZ, ENDPT1 )
      CALL VPACK (  1.D0, 0.D0, XZ, ENDPT2 )

      CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .              ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect two points of intersection to be found.
C
      XNXPTS = 2

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )
 
      IF ( OK ) THEN

         CALL VPACK ( -0.5D0, 0.D0, XZ, XXPT1 )
         CALL VPACK (  0.5D0, 0.D0, XZ, XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )
         CALL CHCKAD ( 'XPT2', XPT2, '~~/', XXPT2, 3, VTIGHT, OK )
         
      END IF




C**********************************************************************
C
C     Normal cases --- varied geometry
C
C**********************************************************************

C
C     The following are intersection cases.     
C
C     All intersection cases for lines that are not orthogonal to the 
C     cone's axis can be characterized by:
C
C        1) The line's intersection with the plane containing the apex
C           and normal to the axis. This point of intersection is the
C           vertex of a ray contained in the line and intersecting the
C           cone.
C
C        2) The ray's angle relative to the plane described in (1).
C
C        3) The azimuth of the ray relative to the line segment starting
C           at the ray's vertex and ending at the cone's apex.
C          
C
C     Pick an arbitrary cone axis and apex.
C
C
      CALL VPACK ( 3.D0, -2.D0,  7.D0, APEX )

      CALL VPACK ( 2.D0,  3.D0, -2.D0, AXIS )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )

      CALL VHAT  ( AXIS,  CONZ )       
    
C
C     Pick an arbitrary ray vertex in the X-Y plane.
C
      THETA  = 27.D0 * RPD()

      R      = 1000.D0

      CALL VPACK ( R*COS(THETA), R*SIN(THETA), 0.D0, VTEMP )
C
C     Create the transformation matrix for mapping vectors
C     from the standard frame to the cone frame.
C
      CALL TWOVEC ( CONZ, 3, VTEMP, 1, XFORM )

      DO I = 1, 3

         CONX(I) = XFORM(1,I)

      END DO

      CALL VSCL ( R, CONX, CONVTX )

C
C     Loop over cone angles.
C
      NCONE  = 10

      DALPHA = HALFPI() / (NCONE+1)

      
C
C     Note: NLAT can be increased to 100 without an excessive
C     increase in run time. However, the value 10 exercises a
C     difficult numerical case: that in which the ray is normal
C     to the cone at the first intercept.
C
      NLAT   = 10

      DLAT   = HALFPI() / (NLAT+1)


C
C     Let the cone angles range from DALPHA to pi/2 - DALPHA.
C
      DO I = 1, NCONE

         ANGLE = I * DALPHA 

C
C        Let the ray "latitude" vary from DLAT to pi/2 - DLAT. Note
C        that this is an approximation unless the ray's direction
C        vector has a zero Y component.
C
         DO J = 1, NLAT

            LAT = J * DLAT

C
C           Let H be the Z intercept of a ray emanating from CONVTX,
C           pointing toward the +Z axis, and making angle LAT relative
C           to the X-Y plane.
C
            H = R * TAN( LAT )
C
C           Let SPAN be the length of a line segment parallel to the Y
C           axis and passing through the +Z axis at height H.
C
            SPAN = 2.D0 * H * TAN( ANGLE )

C
C           We'll place a "target" on the segment from which SPAN was 
C           derived.
C            
            NSPAN = 11

            DSPAN = SPAN / (NSPAN-1)

            DO K = 1, NSPAN

               TARGY = (-SPAN/2) + (K-1)*DSPAN

               IF ( ( K .EQ. 1 ) .OR. ( K .EQ. NSPAN ) ) THEN
C
C                 Contract TARGY a bit, since these are formally
C                 tangent cases, and we want to ensure a hit.
C
                  TARGY = TARGY * ( 1.D0 - 1.D-3 )

               END IF

               CALL VPACK ( 0.D0, TARGY,  H, TARG )

               CALL VSUB  ( TARG, CONVTX, CONDIR )

               CALL VHATIP( CONDIR )

               CALL VEQU  ( CONVTX, CONPT1 )

C
C              Let R2 be the distance of the second endpoint from
C              the vertex.
C
               R2 = 1.D5
               CALL VLCOM ( 1.D0, CONPT1, R2, CONDIR, CONPT2 )

C
C              Shift the endpoints to make them relative to the origin
C              of the standard frame. Convert the endpoints back to the
C              standard basis.
C
               CALL MTXV ( XFORM,  CONPT1, VTEMP  )
               CALL VADD ( VTEMP,  APEX,   ENDPT1 )

               CALL MTXV ( XFORM,  CONPT2, VTEMP  )
               CALL VADD ( VTEMP,  APEX,   ENDPT2 )

               CALL MTXV ( XFORM,  CONVTX, VTEMP  )
               CALL VADD ( VTEMP,  APEX,   VERTEX )

               CALL MTXV ( XFORM,  TARG,   VTEMP  )
               CALL VADD ( VTEMP,  APEX,   TARG   )

               CALL MTXV ( XFORM,  CONDIR, VTEMP )
               CALL VEQU ( VTEMP,          RAYDIR )



C
C --- Case: ------------------------------------------------------
C 
               TITLE = 'Intercept case: I = #; J = #; K = #'
               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL TCASE ( TITLE )


               CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .                       ENDPT2, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( 'NXPTS', NXPTS, '>', 0, 0, OK )

               IF ( NXPTS .GE. 1 ) THEN
C
C                 Check XPT1.
C
C                 Make sure XPT1 is on the ray containing the
C                 line segment.
C
                  CALL NPLNPT ( VERTEX, RAYDIR, XPT1, PNEAR, DIST )

                  TOL = TIGHT

                  CALL CHCKAD( 'PNEAR', PNEAR, '~~/', XPT1, 3, TOL, OK )

C
C                 Make sure XPT1 is on the correct side of the ray's 
C                 vertex.
C
                  CALL VSUB ( XPT1, VERTEX, EP1OFF )

                  DP = VDOT ( EP1OFF, RAYDIR )

                  CALL CHCKSD ( 'DP 1', DP, '>', 0.D0, 0.D0, OK )

C
C                 Make sure XPT1 is on the cone's surface.
C
                  CALL VSUB ( XPT1, APEX, OFFSET )

                  SEP = VSEP( OFFSET, AXIS )

                  TOL = TIGHT
                  
                  CALL CHCKSD ( 'XPT1 SEP', SEP, '~', ANGLE, TOL, OK )


                  IF ( NXPTS .EQ. 2 ) THEN
                     
C
C                    Check XPT2.
C
C                    Make sure XPT2 is on the ray containing the
C                    line segment.
C
                     CALL NPLNPT ( VERTEX, RAYDIR, XPT2, PNEAR, DIST )

                     TOL = TIGHT

                     CALL CHCKAD( 'PNEAR', PNEAR, '~~/', 
     .                                     XPT2, 3, TOL, OK )

C
C                    Make sure XPT2 is on the correct side of the ray's
C                    vertex.
C
                     CALL VSUB ( XPT2, VERTEX, EP2OFF )

                     DP = VDOT ( EP2OFF, RAYDIR )

                     CALL CHCKSD ( 'DP 2', DP, '>', 0.D0, 0.D0, OK )

C
C                    Check that XPT2 is farther from VERTEX than XPT1.
C
                     CALL CHCKSD ( 'XPT2 offset', VNORM(EP2OFF), '>',
     .                             VNORM(EP1OFF), 0.D0,          OK )

C
C                    Make sure XPT2 is on the cone's surface.
C
                     CALL VSUB ( XPT2, APEX, OFFSET )

                     SEP = VSEP( OFFSET, AXIS )

                     TOL = TIGHT
                  
                     CALL CHCKSD ( 'XPT2 SEP', SEP, '~', 
     .                                         ANGLE, TOL, OK )

                  END IF

               END IF


C
C --- Case: ------------------------------------------------------
C 
               TITLE = 'Intercept case with reversed endpoints: '
     .         //      'I = #; J = #; K = #'
               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL TCASE ( TITLE )


               CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, 
     .                       ENDPT1, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( 'NXPTS', NXPTS, '>', 0, 0, OK )

               IF ( NXPTS .GE. 1 ) THEN
C
C                 Check XPT1.
C
C                 Make sure XPT1 is on the ray containing the
C                 line segment.
C
                  CALL NPLNPT ( VERTEX, RAYDIR, XPT1, PNEAR, DIST )

                  TOL = MED

                  CALL CHCKAD( 'PNEAR', PNEAR, '~~/', XPT1, 3, TOL, OK )

C
C                 Make sure XPT1 is on the correct side of the ray's 
C                 vertex.
C
                  CALL VSUB ( XPT1, VERTEX, EP1OFF )

                  DP = VDOT ( EP1OFF, RAYDIR )

                  CALL CHCKSD ( 'DP 1', DP, '>', 0.D0, 0.D0, OK )

C
C                 Make sure XPT1 is on the cone's surface.
C
                  CALL VSUB ( XPT1, APEX, OFFSET )

                  SEP = VSEP( OFFSET, AXIS )

                  TOL = MED
                  
                  CALL CHCKSD ( 'XPT1 SEP', SEP, '~', ANGLE, TOL, OK )


                  IF ( NXPTS .EQ. 2 ) THEN
                     
C
C                    Check XPT2.
C
C                    Make sure XPT2 is on the ray containing the
C                    line segment.
C
                     CALL NPLNPT ( VERTEX, RAYDIR, XPT2, PNEAR, DIST )

                     TOL = TIGHT

                     CALL CHCKAD( 'PNEAR', PNEAR, '~~/', 
     .                                     XPT2, 3, TOL, OK )

C
C                    Make sure XPT2 is on the correct side of the ray's
C                    vertex.
C
                     CALL VSUB ( XPT2, VERTEX, EP2OFF )

                     DP = VDOT ( EP2OFF, RAYDIR )

                     CALL CHCKSD ( 'DP 2', DP, '>', 0.D0, 0.D0, OK )

C
C                    Check that XPT1 is farther from VERTEX than XPT2.
C
                     CALL CHCKSD ( 'XPT1 offset', VNORM(EP1OFF), '>',
     .                             VNORM(EP2OFF), 0.D0,          OK )

C
C                    Make sure XPT2 is on the cone's surface.
C
                     CALL VSUB ( XPT2, APEX, OFFSET )

                     SEP = VSEP( OFFSET, AXIS )

                     TOL = MED
                  
                     CALL CHCKSD ( 'XPT2 SEP', SEP, '~', 
     .                                         ANGLE, TOL, OK )

                  END IF


               END IF


C
C --- Case: ------------------------------------------------------
C 
               TITLE = 'Intercept case with second endpoint in '
     .         //      'cone''s interior. '
     .         //      'I = #; J = #; K = #'
               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL TCASE ( TITLE )

C
C              Find the first intercept. Extend the segment slightly
C              to create the second endpoint.
C              
               CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .                       ENDPT2, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( '(0) NXPTS', NXPTS, '>', 0, 0, OK )

               CALL VSUB ( XPT1, ENDPT1, EP1OFF )

               CALL VLCOM ( 1.D0, ENDPT1, (1.D0 + 1.D-3), EP1OFF, EP2 )

C
C              On this second call to T_INCNSG, use the new value
C              of the second endpoint.
C              
               CALL T_INCNSG ( APEX, AXIS,  ANGLE, ENDPT1, 
     .                       EP2,  NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              We expect exactly one hit.
C
               CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 1, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
               TITLE = 'Intercept case with first endpoint in '
     .         //      'cone''s interior. The endpoints are '
     .         //      'reversed relative to those of the previous '
     .         //      'case. I = #; J = #; K = #.'
               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL TCASE ( TITLE )

C
C              Find the first intercept. Extend the segment slightly
C              to create the second endpoint.
C              
               CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .                       ENDPT2, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( '(0) NXPTS', NXPTS, '>', 0, 0, OK )

               CALL VSUB ( XPT1, ENDPT1, EP1OFF )

               CALL VLCOM ( 1.D0, ENDPT1, (1.D0 + 1.D-3), EP1OFF, EP2 )

C
C              On this second call to T_INCNSG, use the new value
C              of the second endpoint as the first endpoint.
C              
               CALL T_INCNSG ( APEX,   AXIS,  ANGLE, EP2,  
     .                       ENDPT1, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              We expect exactly one hit.
C
               CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 1, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
               IF ( K .EQ. ((NSPAN+1)/2) ) THEN
C
C                 Perform this test only for rays contained in the 
C                 plane containing ENDPT1 and AXIS; this simplifies
C                 the geometry.
C
               
                  TITLE = 'Intercept case with first endpoint in '
     .            //      'cone''s interior. '
     .            //      'I = #; J = #; K = #'
                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL REPMI ( TITLE, '#', K, TITLE )

                  CALL TCASE ( TITLE )

C
C                 Find the first intercept. Extend the segment slightly
C                 to create the new value of the first endpoint.
C              
                  CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .                          ENDPT2, NXPTS, XPT1,  XPT2   )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSI ( '(0) NXPTS', NXPTS, '>', 0, 0, OK )

                  CALL VSUB ( XPT1, ENDPT1, EP1OFF )

                  CALL VLCOM ( 1.D0,          ENDPT1, 
     .                        (1.D0 + 1.D-3), EP1OFF, EP1 )

C
C                 On this second call to T_INCNSG, use the new value of
C                 the first endpoint.
C              
                  CALL T_INCNSG ( APEX,   AXIS,  ANGLE, EP1, 
     .                          ENDPT2, NXPTS, XPT1,  XPT2   )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  MARGIN = 1.D-10

                  IF ( LAT .LT. (HALFPI()-ANGLE)-MARGIN ) THEN
C
C                    We expect exactly one hit.
C
                     CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 1, 0, OK )
  
                  ELSE IF ( LAT .GT. (HALFPI()-ANGLE)+MARGIN ) THEN
C
C                    There should be no hits; the ray rises more steeply
C                    than the cone.
C
                     CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 0, 0, OK )

                  END IF


C
C --- Case: ------------------------------------------------------
C
C                 Perform this test only for rays contained in the 
C                 plane containing ENDPT1 and AXIS; this simplifies
C                 the geometry.
C
               
                  TITLE = 'Intercept case with second endpoint in '
     .            //      'cone''s interior. This is a repeat of '
     .            //      'the previous case, with endpoints switched. '
     .            //      'I = #; J = #; K = #'
                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL REPMI ( TITLE, '#', K, TITLE )

                  CALL TCASE ( TITLE )

C
C                 On this second call to T_INCNSG, use the new value of
C                 the first endpoint.
C              
                  CALL T_INCNSG ( APEX, AXIS,  ANGLE, ENDPT2, 
     .                          EP1,  NXPTS, XPT1,  XPT2   )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  MARGIN = 1.D-10

                  IF ( LAT .LT. (HALFPI()-ANGLE)-MARGIN ) THEN
C
C                    We expect exactly one hit.
C
                     CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 1, 0, OK )
  
                  ELSE IF ( LAT .GT. (HALFPI()-ANGLE)+MARGIN ) THEN
C
C                    There should be no hits; the ray rises more steeply
C                    than the cone.
C
                     CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 0, 0, OK )

                  END IF

               END IF

            END DO

C
C --- Case: ------------------------------------------------------
C
C           Miss case: make the target too far off in the Y direction
C           for an intercept to exist. 
C
            K     = 0
            TARGY = (-SPAN/2) + (K-1)*DSPAN

            TITLE = 'Non-intercept case: Y < -SPAN/2. '
     .      //      'I = #; J = #; K = #'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL REPMI ( TITLE, '#', K, TITLE )

            CALL TCASE ( TITLE )

            CALL VPACK ( 0.D0, TARGY,  H, TARG )

            CALL VSUB  ( TARG, CONVTX, CONDIR )

            CALL VHATIP( CONDIR )

            CALL VEQU  ( CONVTX, CONPT1 )

C
C           Let R2 be the distance of the second endpoint from
C           the vertex.
C
            R2 = 1.D5
            CALL VLCOM ( 1.D0, CONPT1, R2, CONDIR, CONPT2 )

C
C           Shift the endpoints to make them relative to the origin
C           of the standard frame. Convert the endpoints back to the
C           standard basis.
C
            CALL MTXV ( XFORM,  CONPT1, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   ENDPT1 )

            CALL MTXV ( XFORM,  CONPT2, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   ENDPT2 )

            CALL MTXV ( XFORM,  TARG,   VTEMP  )
            CALL VADD ( VTEMP,  APEX,   TARG   )

            CALL MTXV ( XFORM,  CONVTX, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   VERTEX )

            CALL MTXV ( XFORM,  RAYDIR, VTEMP )
            CALL VEQU ( VTEMP,          RAYDIR )
            
            CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .                    ENDPT2, NXPTS, XPT1,  XPT2   )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           We expect a miss.
C
            CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C
C           Miss case: make the target too far off in the Y direction
C           for an intercept to exist. 
C
            K     = NSPAN+1
            TARGY = DBLE(NSPAN/2) * DSPAN

            TITLE = 'Non-intercept case: Y > SPAN/2. '
     .      //      'I = #; J = #; K = #'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL REPMI ( TITLE, '#', K, TITLE )

            CALL TCASE ( TITLE )

            CALL VPACK ( 0.D0, TARGY,  H, TARG )

            CALL VSUB  ( TARG, CONVTX, RAYDIR )

            CALL VHATIP( RAYDIR )

            CALL VEQU  ( CONVTX, CONPT1 )

C
C           Let R2 be the distance of the second endpoint from
C           the vertex.
C
            R2 = 1.D5
            CALL VLCOM ( 1.D0, CONPT1, R2, CONDIR, CONPT2 )

C
C           Shift the endpoints to make them relative to the origin
C           of the standard frame. Convert the endpoints back to the
C           standard basis.
C
            CALL MTXV ( XFORM,  CONPT1, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   ENDPT1 )

            CALL MTXV ( XFORM,  CONPT2, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   ENDPT2 )

            CALL MTXV ( XFORM,  TARG,   VTEMP  )
            CALL VADD ( VTEMP,  APEX,   TARG   )

            CALL MTXV ( XFORM,  CONVTX, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   VERTEX )

            CALL MTXV ( XFORM,  RAYDIR, VTEMP )
            CALL VEQU ( VTEMP,          RAYDIR )
            
            CALL T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, 
     .                    ENDPT2, NXPTS, XPT1,  XPT2   )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           We expect a miss.
C
            CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )

         END DO

      END DO


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


