C$Procedure F_ZZEDTMPT ( ZZEDTMPT tests )
 
      SUBROUTINE F_ZZEDTMPT ( OK )
 
C$ Abstract
C
C     Exercise the routine ZZEDTMPT.
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
      
      INCLUDE 'zzctr.inc'
      
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
C     This routine tests the SPICELIB routine ZZEDTMPT.
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
C-    TSPICE Version 1.0.0, 30-JUN-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C

C
C     Other functions
C
C        T_RANDD ( LOWER, UPPER, SEED )
C     
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      T_RANDD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT

C
C     Local Parameters 
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-13 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )
C
C     Local Variables 
C  
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(1)         LCHAR  ( 2 )

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      AXIOFF ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BETA
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      DOT
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLNVEC ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      PROJ   ( 3 )
      DOUBLE PRECISION      REFPLN ( UBPL )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SRCRAD
      DOUBLE PRECISION      TANPLN ( UBPL )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XPOINT ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NSHAPE
      INTEGER               SEED
      INTEGER               SHADIX

      LOGICAL               UMBRAL


C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C
      DATA                  LCHAR  / 'T', 'F' /
      DATA                  ORIGIN / 3 * 0.D0 /


C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZEDTMPT' )



C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C     The following simple cases are meant to assist debugging.
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Light source and target are spheres of '
     .//           'the same size: A. Distance between centers '
     .//           'is 3*A. Umbral case.'                       )
 
      A      =  1.D3
      B      =  A
      C      =  A
      SRCRAD =  A

      UMBRAL = .TRUE.

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )
 
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSCL ( 0.5D0*SQRT(2.D0)*A, PLNVEC, XPOINT )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Light source and target are spheres of '
     .//           'the same size: A. Distance between centers '
     .//           'is 3*A. Penumbral case.'                     )
 
      A      =  1.D3
      B      =  A
      C      =  A
      SRCRAD =  A

      UMBRAL = .FALSE.

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )
 
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     A tangent segment connecting the sphere and target, 
C     in the plane defined by PLNVEC, crosses the axis with
C     angle ALPHA, where
C
C        sin( ALPHA ) = A / (1.5 * A)
C
C     The complementary angle is BETA.
C
      ALPHA = ASIN ( 1.D0/1.5D0 )

      BETA  = HALFPI() - ALPHA

      CALL VSCL ( SIN(BETA) * A * SQRT(2.D0)/2, PLNVEC, XPOINT )

      XPOINT(1) = A * COS(BETA)


      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Triaxial target. Source is larger than longest '
     .//           'target axis. Umbral case.'                     )
 
      A      =  1.D3
      B      =  2*A
      C      =  A/2
      SRCRAD =  10*A


      UMBRAL = .TRUE.

      CALL VPACK ( 20*A,  0.D0, 0.D0, AXIS   )
 
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure POINT is on the target. Find the scaled version 
C     of POINT on the target surface and compare.
C
      CALL EDPNT ( POINT, A, B, C, XPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C     Make sure POINT is in the reference plane. Project POINT
C     onto the plane and compare.
C
      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL VPRJP ( POINT, REFPLN, XPOINT )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C     Make sure POINT is on the correct side of the axis with respect
C     to PLNVEC.
C
      DOT = VDOT ( POINT, PLNVEC )

      CALL CHCKSD ( 'point DOT', DOT, '>', 0.D0, TOL, OK )
      
C
C     Create a tangent plane at the terminator point. The distance
C     between this plane and the center of the source should be
C     the radius of the source.
C
      CALL SURFNM ( A, B, C, POINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NVP2PL ( NORMAL, POINT, TANPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( AXIS, TANPLN, PROJ   )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      DIST = VDIST( AXIS, PROJ )

      TOL  = TIGHT

      CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )

C
C     Make sure the source center is on the correct side of the
C     tangent plane.
C
      CALL VSUB ( AXIS, PROJ, AXIOFF )

      DOT = VDOT ( AXIOFF, NORMAL )

      CALL CHCKSD ( 'axis DOT', DOT, '<', 0.D0, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Triaxial target. Source is smaller than shortest '
     .//           'target axis. Umbral case.'                       )
 
      A      =  1.D3
      B      =  2*A
      C      =  A/2
      SRCRAD =  A/10


      UMBRAL = .TRUE.

      CALL VPACK ( 40*A,  0.D0, 0.D0, AXIS   )
 
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure POINT is on the target. Find the scaled version 
C     of POINT on the target surface and compare.
C
      CALL EDPNT ( POINT, A, B, C, XPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C     Make sure POINT is in the reference plane. Project POINT
C     onto the plane and compare.
C
      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL VPRJP ( POINT, REFPLN, XPOINT )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C     Make sure POINT is on the correct side of the axis with respect
C     to PLNVEC.
C
      DOT = VDOT ( POINT, PLNVEC )

      CALL CHCKSD ( 'DOT', DOT, '>', 0.D0, TOL, OK )
      
C
C     Create a tangent plane at the terminator point. The distance
C     between this plane and the center of the source should be
C     the radius of the source.
C
      CALL SURFNM ( A, B, C, POINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NVP2PL ( NORMAL, POINT, TANPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( AXIS, TANPLN, PROJ   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DIST = VDIST( AXIS, PROJ )

      TOL  = TIGHT

      CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )
      

C
C     Make sure the source center is on the correct side of the
C     tangent plane.
C
      CALL VSUB ( AXIS, PROJ, AXIOFF )

      DOT = VDOT ( AXIOFF, NORMAL )

      CALL CHCKSD ( 'axis DOT', DOT, '<', 0.D0, TOL, OK )

            
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Triaxial target. Source is larger than longest '
     .//           'target axis. Penumbral case.'                  )
 
      A      =  1.D3
      B      =  2*A
      C      =  A/2
      SRCRAD =  10*A

      UMBRAL = .FALSE.

      CALL VPACK ( 30*A,  0.D0, 0.D0, AXIS   )
 
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure POINT is on the target. Find the scaled version 
C     of POINT on the target surface and compare.
C
      CALL EDPNT ( POINT, A, B, C, XPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C     Make sure POINT is in the reference plane. Project POINT
C     onto the plane and compare.
C
      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL VPRJP ( POINT, REFPLN, XPOINT )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C     Make sure POINT is on the correct side of the axis with respect
C     to PLNVEC.
C
      DOT = VDOT ( POINT, PLNVEC )

      CALL CHCKSD ( 'DOT', DOT, '>', 0.D0, TOL, OK )
      
C
C     Create a tangent plane at the terminator point. The distance
C     between this plane and the center of the source should be
C     the radius of the source.
C
      CALL SURFNM ( A, B, C, POINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NVP2PL ( NORMAL, POINT, TANPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( AXIS, TANPLN, PROJ   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DIST = VDIST( AXIS, PROJ )

      TOL  = TIGHT

      CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )


C
C     Make sure the source center is on the correct side of the
C     tangent plane.
C
      CALL VSUB ( AXIS, PROJ, AXIOFF )

      DOT = VDOT ( AXIOFF, NORMAL )

      CALL CHCKSD ( 'axis DOT', DOT, '>', 0.D0, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Triaxial target. Source is smaller than shortest '
     .//           'target axis. Penumbral case.'                    )
 
      A      =  1.D3
      B      =  2*A
      C      =  A/2
      SRCRAD =  A/10


      UMBRAL = .FALSE.

      CALL VPACK ( 30*A,  0.D0, 0.D0, AXIS   )
 
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure POINT is on the target. Find the scaled version 
C     of POINT on the target surface and compare.
C
      CALL EDPNT ( POINT, A, B, C, XPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C     Make sure POINT is in the reference plane. Project POINT
C     onto the plane and compare.
C
      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL VPRJP ( POINT, REFPLN, XPOINT )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

C
C     Make sure POINT is on the correct side of the axis with respect
C     to PLNVEC.
C
      DOT = VDOT ( POINT, PLNVEC )

      CALL CHCKSD ( 'DOT', DOT, '>', 0.D0, TOL, OK )
      
C
C     Create a tangent plane at the terminator point. The distance
C     between this plane and the center of the source should be
C     the radius of the source.
C
      CALL SURFNM ( A, B, C, POINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NVP2PL ( NORMAL, POINT, TANPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( AXIS, TANPLN, PROJ   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DIST = VDIST( AXIS, PROJ )

      TOL  = TIGHT

      CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )
      

C
C     Make sure the source center is on the correct side of the
C     tangent plane.
C
      CALL VSUB ( AXIS, PROJ, AXIOFF )

      DOT = VDOT ( AXIOFF, NORMAL )

      CALL CHCKSD ( 'axis DOT', DOT, '>', 0.D0, TOL, OK )


C
C     
C     The following cases use a variety of target shapes, scales, and
C     light source positions
C     
C
      NLAT = 11
      NLON = 10

      DLAT =   PI() / (NLAT - 1)
      DLON = 2*PI() /  NLON

      NSHAPE = 10

      SEED   = -100


      DO SHADIX = 1, 2
C
C        Set the shadow type flag.
C
         UMBRAL =  ( SHADIX .EQ. 1 )
            
         DO I = 1, NLON

            LON = (I-1)*DLON

            DO J = 1, NLAT

               LAT = HALFPI() - (J-1)*DLAT

               DO K = 1, NSHAPE
C
C                 Pick scale factor.
C
                  SCALE = 10.D0 ** T_RANDD( -100.D0, 100.D0, SEED )

                  A      = SCALE *  T_RANDD( 1.D0, 10.D0, SEED ) 
                  B      = SCALE *  T_RANDD( 1.D0, 10.D0, SEED ) 
                  C      = SCALE *  T_RANDD( 1.D0, 10.D0, SEED ) 
                  SRCRAD = SCALE *  T_RANDD( 1.D0, 10.D0, SEED ) 
                  CALL CHCKXC ( .FALSE., ' ', OK )


C
C                 Pick a rotation angle for the cutting half plane.
C
                  THETA = T_RANDD ( 0.D0, 2*PI(), SEED )

                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
                  TITLE =  'Random case. SCALE = #; A = #; B = #; '
     .            //       'C = #; SRCRAD = #, THETA (deg) = #; '
     .            //       'UMBRAL = #.'        
                  
                  CALL REPMD ( TITLE, '#', SCALE,       14, TITLE )
                  CALL REPMD ( TITLE, '#', A,           14, TITLE )
                  CALL REPMD ( TITLE, '#', B,           14, TITLE )
                  CALL REPMD ( TITLE, '#', C,           14, TITLE )
                  CALL REPMD ( TITLE, '#', SRCRAD,      14, TITLE )
                  CALL REPMD ( TITLE, '#', THETA*DPR(), 14, TITLE )
                  CALL REPMC ( TITLE, '#', LCHAR(SHADIX),   TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )

                  CALL LATREC ( 1000.D0*SCALE, LON, LAT, AXIS )
                  
                  CALL VEQU  ( AXIS, X )
                  CALL FRAME ( X, Y, Z )

                  CALL VROTV ( Z, AXIS, THETA, PLNVEC )

                  CALL ZZEDTMPT ( UMBRAL, A,    B,      C, 
     .                            SRCRAD, AXIS, PLNVEC, POINT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Make sure POINT is on the target. Find the scaled
C                 version of POINT on the target surface and compare.
C
                  CALL EDPNT ( POINT, A, B, C, XPOINT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  TOL = VTIGHT

                  CALL CHCKAD ( 'POINT', POINT, '~~/', 
     .                                   XPOINT, 3, TOL, OK )

C
C                 Make sure POINT is in the reference plane. Project
C                 POINT onto the plane and compare.
C
                  CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
                  CALL CHCKXC ( .FALSE., ' ', OK )
      
                  CALL VPRJP ( POINT, REFPLN, XPOINT )

                  TOL = VTIGHT

                  CALL CHCKAD ( 'POINT', POINT, '~~/', 
     .                                   XPOINT, 3, TOL, OK )
C
C                 Make sure POINT is on the correct side of the axis
C                 with respect to PLNVEC.
C
                  DOT = VDOT ( POINT, PLNVEC )

                  CALL CHCKSD ( 'DOT', DOT, '>', 0.D0, TOL, OK )
      
C
C                 Create a tangent plane at the terminator point. The
C                 distance between this plane and the center of the
C                 source should be the radius of the source.
C
                  CALL SURFNM ( A, B, C, POINT, NORMAL )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL NVP2PL ( NORMAL, POINT, TANPLN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL VPRJP ( AXIS, TANPLN, PROJ   )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  DIST = VDIST( AXIS, PROJ )

                  TOL  = TIGHT

                  CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )
      
C
C                 Make sure the source center is on the correct side of
C                 the tangent plane.
C
                  CALL VSUB ( AXIS, PROJ, AXIOFF )

                  DOT = VDOT ( AXIOFF, NORMAL )

                  IF ( UMBRAL ) THEN

                     CALL CHCKSD ( 'axis DOT', DOT, '<', 0.D0, TOL, OK )
                  ELSE
                     CALL CHCKSD ( 'axis DOT', DOT, '>', 0.D0, TOL, OK )
                  END IF

               END DO

            END DO

         END DO

      END DO



C**********************************************************************
C
C     Error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad target radii.' )


      A      =  1.D3
      B      =  A
      C      =  A
      SRCRAD =  A

      UMBRAL = .FALSE.

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )
 
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL,  0.D0, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, -1.D0, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, A,    0.D0, C, SRCRAD, AXIS, 
     .                PLNVEC, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, A,   -1.D0, C, SRCRAD, AXIS, 
     .                PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, A,    B,  0.D0, SRCRAD, AXIS, 
     .                PLNVEC, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, A,    B, -1.D0, SRCRAD, AXIS, 
     .                PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad source radius' )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, 0.D0, AXIS, 
     .                PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, -1.D0, AXIS, 
     .                PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad axis vector' )

      CALL CLEARD ( 3, AXIS )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, 
     .                PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Source and target are too close.' )

      CALL VPACK ( A,  0.D0, 0.D0, AXIS   )
 
      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, 
     .                PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(OBJECTSTOOCLOSE)', OK )

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad plane reference vector' )

      CALL CLEARD ( 3, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, 
     .                PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Axis and plane reference vector are '
     .//           'linearly dependent.'                 )

      CALL VEQU ( AXIS, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, 
     .                PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


