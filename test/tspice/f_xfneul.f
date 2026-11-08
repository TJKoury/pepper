C$Procedure      F_XFNEUL ( State transformations and Euler angles)
 
      SUBROUTINE F_XFNEUL ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C-    TSPICE Version 2.0.0, 31-OCT-2005 (NJB)
C
C        Updated to test EUL2XF for all axis sequences, including those
C        for which the second axis matches the first or third.
C        Simplified the algorithm for constructing the expected
C        rotation.
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXM and VSCLG calls. 
C
C     This routine tests the routines XF2EUL and EUL2XF
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
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
 
C
C     Local Variables
C
      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      AXIS   ( 3, 3, 3 )
      DOUBLE PRECISION      CEULER ( 6 )
      DOUBLE PRECISION      DIAG   ( 3, 3, 3 )
      DOUBLE PRECISION      EULER  ( 6 )
      DOUBLE PRECISION      OMEGA  ( 3, 3, 3 )
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      XEULER ( 6 )
      DOUBLE PRECISION      XF     ( 6, 6, 3 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XPECT  ( 6, 6 )
      DOUBLE PRECISION      XTEMP  ( 6, 6 )

      INTEGER               A      ( 3 )
      INTEGER               D
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M

      LOGICAL               UNIQUE
 
 
 
 
 
      D(I,J) = IABS(  ( IABS(I-J) - 1 ) / ( IABS(I-J) + 1 ) )
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_XFNEUL' )
C     Validate the computation of state transformation from
C     Euler angles and derivatives.
C
 
      DO I = 1,3
         DO J = 1,3
            DO K = 1,3
               OMEGA(I,J,K) = 0.0D0
               AXIS (I,J,K) = 0.0D0
               DIAG (I,J,K) = 0.0D0
            END DO
         END DO
      END DO
 
 
      DO I = 1,3
         DO J = 1, 3
            DIAG(J,J,I) = 1.0D0 - DBLE( D(I,J) )
            AXIS(J,J,I) =         DBLE( D(I,J) )
         END DO
      END DO
 
      OMEGA(1,2,3) =  1.0D0
      OMEGA(2,3,1) =  1.0D0
      OMEGA(3,1,2) =  1.0D0
 
      OMEGA(3,2,1) = -1.0D0
      OMEGA(2,1,3) = -1.0D0
      OMEGA(1,3,2) = -1.0D0
 
 
 
      CALL TCASE ( 'Validate the computation of state '
     .//           'transformation from Euler angles and '
     .//           'derivatives. Every possible combination of '
     .//           'axes is tested. ' )
 
 
         EULER(1) =  0.33D0
         EULER(2) = -0.20D0
         EULER(3) =  0.50D0
         EULER(4) = -0.30D0
         EULER(5) =  0.10D0
         EULER(6) =  0.70D0
 
         DO I = 1, 3

            DO J = 1, 3
  
               DO K = 1, 3
C
C                 We construct the state transformation matrix
C                 from scratch and compare that to the results
C                 returned by EUL2XF.
C
                  A(1) = I
                  A(2) = J
                  A(3) = K

C
C                 Set the expected state transformation to the identity
C                 to begin with.
C
                  CALL CLEARD ( 36, XPECT )

                  DO M = 1, 6
                     XPECT(M,M) = 1.D0
                  END DO

                  DO M = 1, 3
C
C                    Construct the state transformation for the Mth
C                    rotation in the sequence.  We start out with
C                    the rotation matrix and angular velocity vector.
C
                     CALL ROTATE ( EULER(M), A(M), ROT )

C
C                    Set the angular velocity vector:  the component
C                    corresponding to axis M is the Mth rate; the other
C                    components are zero.
C                    
                     CALL CLEARD ( 3, AV )

                     AV( A(M) ) = EULER ( M+3 )
                     
                     CALL RAV2XF ( ROT, AV, XF(1,1,M) )
 
                     CALL MXMG  ( XPECT, XF(1,1,M), 6, 6, 6, XTEMP )
                     CALL MOVED ( XTEMP, 36,                 XPECT )

                  END DO
                                  

                  CALL EUL2XF ( EULER, I, J, K, XFORM )

                  CALL TSTMSG ( '#', 'Rotation is a #-#-# ' )
                  CALL TSTMSI ( I )
                  CALL TSTMSI ( J )
                  CALL TSTMSI ( K )

                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKAD ( 'XFORM', XFORM, '~', XPECT,
     .                           36,     2.0D-14,    OK  )

               END DO

            END DO

         END DO
 
 
      CALL TCASE ( 'Validate the computation of euler angles and '
     .//           'derivatives from the state transformation '
     .//           'matrix. Every combination of rotation axes '
     .//           'is exercised. ' )
 
 
         EULER(1) =  0.33D0
         EULER(2) =  0.20D0
         EULER(3) =  0.50D0
         EULER(4) =  0.30D0
         EULER(5) =  0.10D0
         EULER(6) =  0.70D0
 
         DO I = 1, 3
            DO J = 1, 3
 
               IF ( I .NE. J ) THEN
 
                  DO K = 1, 3
 
                     IF ( K .NE. J ) THEN
 
                        CALL EUL2XF ( EULER, I, J, K, XFORM )
                        CALL XF2EUL ( XFORM, I, J, K, CEULER, UNIQUE )
 
                        CALL TSTMSG ( '#', 'Rotation is a #-#-# ' )
                        CALL TSTMSI ( I )
                        CALL TSTMSI ( J )
                        CALL TSTMSI ( K )
 
                        CALL CHCKXC ( .FALSE., ' ', OK )
                        CALL CHCKSL ( 'UNIQUE', UNIQUE, .TRUE., OK )
                        CALL CHCKAD ( 'XFORM', CEULER, '~', EULER,
     .                                 6,      2.0D-14,    OK  )
 
                     END IF
 
                  END DO
 
               END IF
            END DO
 
         END DO
 
 
      CALL TCASE ( 'Exercise the degenerate cases where the '
     .//           'second angle is nearly zero. ' )
 
 
 
         EULER(1)  =  0.0D0
         EULER(2)  =  1.0D-10
         EULER(3)  =  0.50D0
         EULER(4)  =  0.0D0
         EULER(5)  =  0.10D0
         EULER(6)  =  0.70D0
 
         XEULER(1) =  0.0D0
         XEULER(2) =  0.0D0
         XEULER(3) =  0.50D0
         XEULER(4) =  0.0D0
         XEULER(5) =  0.10D0
         XEULER(6) =  0.70D0
 
 
         DO I = 1, 3
            DO J = 1, 3
 
               IF ( I .NE. J ) THEN
 
                  DO K = 1, 3
 
                     IF ( K .EQ. I ) THEN
 
                        CALL EUL2XF ( EULER, I, J, K, XFORM )
                        CALL XF2EUL ( XFORM, I, J, K, CEULER, UNIQUE )
 
 
                        CALL TSTMSG ( '#', 'Rotation is a #-#-# ' )
                        CALL TSTMSI ( I )
                        CALL TSTMSI ( J )
                        CALL TSTMSI ( K )
 
                        CALL CHCKXC ( .FALSE., ' ', OK )
                        CALL CHCKSL ( 'UNIQUE', UNIQUE, .FALSE., OK )
                        CALL CHCKAD ( 'XFORM', CEULER, '~', XEULER,
     .                                 6,      2.0D-14,    OK  )
 
                     END IF
 
                  END DO
 
               END IF
            END DO
 
         END DO
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
