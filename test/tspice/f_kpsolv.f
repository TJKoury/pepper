C$Procedure      F_KPSOLV (Family of tests for KPSOLV )
 
      SUBROUTINE F_KPSOLV ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine loops over a large set of points in the
C     unit circle to test the accuracy of the computations
C     carried out by KPSOLV.
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
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      KPSOLV
 
C
C     Local Variables
C
      DOUBLE PRECISION      EVEC ( 2 )
      DOUBLE PRECISION      FX
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
      DOUBLE PRECISION      R
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      X
 
      INTEGER               I
      INTEGER               J
 
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_KPSOLV' )
 
      CALL TCASE ( 'Determine how well the solution fits h,k '
     .//           'whose polar coordinates are r = {0, .05, .1, '
     .//           '... .9} and theta = { 0, .1 ... pi*2 } ' )
 
 
         R = 0.0D0
 
         DO I = 1,20
 
            THETA = 0.0D0
 
            DO J = 1, 63
 
               CALL TSTMSG ( '#', 'Failure for R = # and THETA = #' )
               CALL TSTMSF ( R     )
               CALL TSTMSF ( THETA )
 
               H       = R * COS(THETA)
               K       = R * SIN(THETA)
               EVEC(1) = H
               EVEC(2) = K
 
               X  = KPSOLV( EVEC )
               FX = H*COS(X) + K*SIN(X)
 
               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKSD ( 'FX', FX, '~', X, 1.0D-15, OK )
 
               THETA = THETA + 0.1D0
 
            END DO
 
            R = R + 0.05D0
 
         END DO
 
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
