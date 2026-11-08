!$Procedure      F_KPSOLV (Family of tests for KPSOLV )

      SUBROUTINE F_KPSOLV ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine loops over a large set of points in the
!     unit circle to test the accuracy of the computations
!     carried out by KPSOLV.
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
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      KPSOLV

!
!     Local Variables
!
      DOUBLE PRECISION      EVEC ( 2 )
      DOUBLE PRECISION      FX
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
      DOUBLE PRECISION      R
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      X

      INTEGER               I
      INTEGER               J



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_KPSOLV' )

      CALL TCASE ( 'Determine how well the solution fits h,k ' &
      //           'whose polar coordinates are r = {0, .05, .1, ' &
      //           '... .9} and theta = { 0, .1 ... pi*2 } ' )


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
