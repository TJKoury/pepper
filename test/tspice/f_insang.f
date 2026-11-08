C$Procedure F_INSANG ( INSANG tests )
 
      SUBROUTINE F_INSANG ( OK )
 
C$ Abstract
C
C     Exercise the ray-voxel grid intersection routine INSANG.
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
C     This routine tests the DSKLIB type 2 ray-voxel grid
C     intersection routine INSANG.
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
C-    TSPICE Version 1.0.0, 20-JUN-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI


C
C     Local Parameters
C 
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-9 )
 
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )
      
      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               NSHAPE
      PARAMETER           ( NSHAPE = 3 )

C
C     Local Variables  
C
      CHARACTER*(MSGLEN)    TITLE 

      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      BETA
      DOUBLE PRECISION      GAMMA
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      E1     ( 3 )
      DOUBLE PRECISION      E2     ( 3 )
      DOUBLE PRECISION      E3     ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      PLATES ( 3, 3, NSHAPE )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLPLT ( 3, 3, NSHAPE )
      DOUBLE PRECISION      SCLVTX ( 3 )
      DOUBLE PRECISION      SIDES  ( 3, 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )
      DOUBLE PRECISION      ZVEC   ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NRAD
      INTEGER               NSCALE
      INTEGER               NXPTS

      LOGICAL               FOUND

C
C     Saved values
C
      SAVE


C
C     Initial values 
C
      DATA                  ORIGIN / 3 * 0.D0 /
      DATA                  ZVEC   / 0.D0, 0.D0, 1.D0 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_INSANG' )


C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Simple intersection case.' )


      CALL VPACK ( 0.D0, 0.D0, 10.D0, E1 )
      CALL VPACK ( 4.D0, 0.D0, 10.D0, E2 )
      CALL VPACK ( 0.D0, 4.D0, 10.D0, E3 )
      CALL VPACK ( 1.D0, 1.D0, 10.D0, V  )


      CALL INSANG ( V, E1, E2, E3, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSCL ( SCALE, V, XPT )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     Create a SPICELIB plane containing the plate defined
C     by E1, E2, E3.
C
      CALL NVC2PL ( ZVEC, 10.D0, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Find the expected intercept.
C     
      CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

 
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Simple non-intersection case.' )

C
C     Use the edge vectors from the previous case. Update V.
C
      CALL VPACK ( 0.D0, 5.D0, 10.D0, V  )


      CALL INSANG ( V, E1, E2, E3, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


 
C
C --- Case: ------------------------------------------------------
C 


C
C     
C     The following set of cases covers a variety of plate shapes,
C     vertex positions, and scales.
C     
C

C
C     Create a rotation to be applied to all inputs to INSANG.
C
      ALPHA =  30.D0 * RPD()
      BETA  = -10.D0 * RPD()
      GAMMA =  70.D0 * RPD()

      CALL EUL2M ( GAMMA, BETA, ALPHA, 3, 2, 3, XFORM )

      CALL IDENT ( XFORM )

C
C     Create the plate shapes to be used in the following tests.
C
C
C        Plate 1 is an acute triangle.
C
      CALL VPACK ( 0.D0, 0.D0, 0.D0, PLATES(1,1,1) )
      CALL VPACK ( 2.D0, 0.D0, 0.D0, PLATES(1,2,1) )
      CALL VPACK ( 1.D0, 4.D0, 0.D0, PLATES(1,3,1) )

C
C        Plate 2 is a right triangle.
C
      CALL VPACK ( 0.D0, 0.D0, 0.D0, PLATES(1,1,2) )
      CALL VPACK ( 4.D0, 0.D0, 0.D0, PLATES(1,2,2) )
      CALL VPACK ( 4.D0, 3.D0, 0.D0, PLATES(1,3,2) )

C
C        Plate 3 is an obtuse triangle.
C
      CALL VPACK ( 0.D0, 0.D0, 0.D0, PLATES(1,1,3) )
      CALL VPACK ( 4.D0, 0.D0, 0.D0, PLATES(1,2,3) )
      CALL VPACK ( 2.D0, 1.D0, 0.D0, PLATES(1,3,3) )

C
C     Set parameters used for vertex placement.
C
      NLON   = 5
      DLON   = TWOPI() / NLON

      NLAT   = 10
      DLAT   = PI() / ( NLAT - 1 )

      NRAD   = 5

C
C     Set parameters used to determine the scale of the inputs.
C
      NSCALE = 7

       DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - (J*DLAT)

            DO K = 1, NRAD

               R = 10.D0 ** ( K - 2 )

               DO L = 1, NSCALE

                  SCALE = 10.D0 ** ( 20.D0 * DBLE( L - (NSCALE/2) ) )

C                  SCALE = 1.D0

C
C                 Create the vertex.
C
                  CALL LATREC ( R, LON, LAT, VERTEX )

C
C                 Scale the vertex.
C
                  CALL VSCL ( SCALE, VERTEX, SCLVTX )


C
C                 Loop over the shapes.
C
                  DO M = 1, 3

                     DO N = 1, 3

                        CALL VSCL ( SCALE, PLATES(1,N,M), 
     .                                     SCLPLT(1,N,M) )
                     END DO

C
C                    Transform vertex and plate using XFORM.
C
                     CALL MXV  ( XFORM, VERTEX, VTEMP  )
                     CALL VEQU ( VTEMP,         VERTEX )
   
                     
                     DO N = 1, 3
                        CALL MXV ( XFORM, PLATES(1,N,M), VTEMP         )
                        CALL VEQU( VTEMP,                PLATES(1,N,M) )
                     END DO               

C
C                    Create the edges of the pyramid for the current
C                    plate.
C
                     CALL VSUB ( SCLPLT(1,1,M), SCLVTX, E1 )
                     CALL VSUB ( SCLPLT(1,2,M), SCLVTX, E2 )
                     CALL VSUB ( SCLPLT(1,3,M), SCLVTX, E3 )

C
C --- Case: ------------------------------------------------------
C 
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. '
     .               //      'Aim at plate''s centroid.' 
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )
                     
                     CALL VLCOM3 ( 1.D0, E1, 1.D0, E2, 1.D0, E3, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL VSCL ( S, V, XPT )
                    
C
C                    Create a SPICELIB plane containing the plate
C                    defined by E1, E2, E3.
C
                     CALL VSUB ( E2, E1, SIDES(1,1) )
                     CALL VSUB ( E3, E1, SIDES(1,3) )

                     CALL PSV2PL ( E1,         SIDES(1,1),
     .                             SIDES(1,3), PLANE      )
                     CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C                    Find the expected intercept.
C     
                     CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

                     TOL = TIGHT

                     CALL CHCKAD ( 'XPT', XPT, '~~/', 
     .                                    XXPT, 3, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. '
     .               //      'Aim at interior point close to '
     .               //      'the E1-E2 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E1, 0.5D0, E2, 1.D-3, E3, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL VSCL ( S, V, XPT )


C
C                    Create a SPICELIB plane containing the plate
C                    defined by E1, E2, E3.
C
                     CALL VSUB ( E2, E1, SIDES(1,1) )
                     CALL VSUB ( E3, E1, SIDES(1,3) )

                     CALL PSV2PL ( E1,         SIDES(1,1),
     .                             SIDES(1,3), PLANE      )
                     CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C                    Find the expected intercept.
C     
                     CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

                                        IF ( .NOT. OK ) STOP
 
                     TOL = TIGHT

                     CALL CHCKAD ( 'XPT', XPT, '~~/', 
     .                                    XXPT, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. '
     .               //      'Aim at exterior point close to '
     .               //      'the E1-E2 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E1, 0.5D0, E2, -1.D-3, E3, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )



C
C --- Case: ------------------------------------------------------
C 
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. '
     .               //      'Aim at interior point close to '
     .               //      'the E1-E3 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E1, 0.5D0, E3, 1.D-3, E2, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL VSCL ( S, V, XPT )


C
C                    Create a SPICELIB plane containing the plate
C                    defined by E1, E2, E3.
C
                     CALL VSUB ( E2, E1, SIDES(1,1) )
                     CALL VSUB ( E3, E1, SIDES(1,3) )

                     CALL PSV2PL ( E1,         SIDES(1,1),
     .                             SIDES(1,3), PLANE      )
                     CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C                    Find the expected intercept.
C     
                     CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

                                        IF ( .NOT. OK ) STOP

                     TOL = TIGHT

                     CALL CHCKAD ( 'XPT', XPT, '~~/', 
     .                                    XXPT, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. '
     .               //      'Aim at exterior point close to '
     .               //      'the E1-E3 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E1, 0.5D0, E3, -1.D-3, E2, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )



C
C --- Case: ------------------------------------------------------
C 
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. '
     .               //      'Aim at interior point close to '
     .               //      'the E2-E3 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E2, 0.5D0, E3, 1.D-3, E1, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL VSCL ( S, V, XPT )


C
C                    Create a SPICELIB plane containing the plate
C                    defined by E1, E2, E3.
C
                     CALL VSUB ( E2, E1, SIDES(1,1) )
                     CALL VSUB ( E3, E1, SIDES(1,3) )

                     CALL PSV2PL ( E1,         SIDES(1,1),
     .                             SIDES(1,3), PLANE      )
                     CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C                    Find the expected intercept.
C     
                     CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

                                        IF ( .NOT. OK ) STOP


                     TOL = TIGHT

                     CALL CHCKAD ( 'XPT', XPT, '~~/', 
     .                                    XXPT, 3, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. '
     .               //      'Aim at exterior point close to '
     .               //      'the E2-E3 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E2, 0.5D0, E3, -1.D-3, E1, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

                  END DO

               END DO

            END DO
            
         END DO

      END DO



C**********************************************************************
C
C     Exceptional cases
C
C**********************************************************************

C
C     INSANG is error-free.
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Ray direction is zero vector.' )

      CALL VPACK ( 0.D0, 0.D0, 10.D0, E1 )
      CALL VPACK ( 4.D0, 0.D0, 10.D0, E2 )
      CALL VPACK ( 0.D0, 4.D0, 10.D0, E3 )
      CALL CLEARD ( 3, V )


      CALL INSANG ( V, E1, E2, E3, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Edge vectors are linearly dependent.' )

C
C     Create independent edge vectors, but pass dependent
C     selections of the them to INSANG.
C
      CALL VPACK ( 0.D0, 0.D0, 10.D0, E1 )
      CALL VPACK ( 4.D0, 0.D0, 10.D0, E2 )
      CALL VPACK ( 0.D0, 4.D0, 10.D0, E3 )
      CALL VPACK ( 1.D0, 1.D0, 10.D0, V  )
 
C
C     E2 = E1:
C
      CALL INSANG ( V, E1, E1, E3, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSD ( 'SCALE', SCALE, '=', 0.D0, 0.D0, OK )

C
C     E3 = E1:
C
      CALL INSANG ( V, E1, E2, E1, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSD ( 'SCALE', SCALE, '=', 0.D0, 0.D0, OK )

C
C     E3 = E2:
C
      CALL INSANG ( V, E1, E2, E2, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSD ( 'SCALE', SCALE, '=', 0.D0, 0.D0, OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END







