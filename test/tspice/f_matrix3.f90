!$Procedure F_MATRIX3 (Family of tests on 3x3-matrix operations)

      SUBROUTINE F_MATRIX3 ( OK )

! $ Abstract
!
!     This routine performs rudimentary tests on a collection of
!     3x3-matrix operation routines.
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
!     None.
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
!     M.C. Kim      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 24-AUG-2016 (MCK) (BVS)
!
!        Initial code to test
!
!           DET
!           MEQU
!           MTXM
!           MTXV
!           MXM
!           MXMT
!           MXV
!           TRACE
!           VTMV
!           XPOSE
!
!-&

! $ Index_Entries
!
!     test 3x3 matrix operation routines
!
!-&

!
!     Test Utility Functions
!

      DOUBLE PRECISION      T_RANDD

!
!     SPICELIB Functions
!

      DOUBLE PRECISION      DET
      DOUBLE PRECISION      TRACE
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VTMV

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.0D-14 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.0D-15 )

!
!     Initial seed number for pseudo-random number generation
!
      INTEGER               SEED0
      PARAMETER           ( SEED0 = 19920810 )

!
!     Dimension of 3-by-3 square matrices
!
      INTEGER               N
      PARAMETER           ( N = 3 )

!
!     Number of elements in an N-by-N matrix.
!
      INTEGER               NE
      PARAMETER           ( NE = N * N )

!
!     Local Variables
!

!
!     indices
!
      INTEGER               I
      INTEGER               J
      INTEGER               INDX

!
!     Seed number for pseudo-random number generation
!
      INTEGER               SEED

!
!     3-component vectors
!
      DOUBLE PRECISION      VINDX ( N )
      DOUBLE PRECISION      VRAN  ( N )
      DOUBLE PRECISION      VOUT  ( N )
      DOUBLE PRECISION      VEXPT ( N )
      DOUBLE PRECISION      VTEMP ( N )

!
!     3-by-3 matrices
!
      DOUBLE PRECISION      MZERO ( N, N )
      DOUBLE PRECISION      MRAN  ( N, N )
      DOUBLE PRECISION      MINDX ( N, N )
      DOUBLE PRECISION      MOUT  ( N, N )

!
!     Evaluated number from a library routine.
!
      DOUBLE PRECISION      EVAL

!
!     Expected value of EVAL
!
      DOUBLE PRECISION      EXPT

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_MATRIX3' )

!
!     Form arrays to be used for more than one test case.
!

!
!     A zero matrix
!
      DO J = 1, N
         DO I = 1, N
            MZERO( I, J ) = 0.0D0
         END DO
      END DO

!
!     An index vector, whose components are their indices
!
      DO I = 1, N
         VINDX(I) = DBLE(I)
      END DO

!
!     An index matrix, whose elements are their column-wise
!     storage indices
!
      INDX = 0
      DO J = 1, N
         DO I = 1, N
            INDX = INDX + 1
            MINDX( I, J ) = DBLE( INDX )
         END DO
      END DO

!
!     Initialize the seed number of random number generation.
!
      SEED = SEED0

!
!     Form a random vector, whose components are
!     uniformly distributed on the interval [-1.0D0, +1.0D0].
!

      DO I = 1, N
         VRAN(I) = T_RANDD ( -1.0D0, +1.0D0, SEED )
      END DO

!
!     Form a random matrix, whose elements are
!     uniformly distributed on the interval [-1.0D0, +1.0D0].
!
      DO J = 1, N
         DO I = 1, N
            MRAN( I, J ) = T_RANDD ( -1.0D0, +1.0D0, SEED )
         END DO
      END DO

!
!     Cases to test MEQU
!

      CALL TCASE ( 'MEQU: copying a 3x3 zero matrix and ' &
      //           'checking strict equality between the zero matrix ' &
      //           'and the copied matrix' )

      CALL MEQU ( MZERO, MOUT )

      CALL CHCKAD ( 'MOUT', MOUT, '=', MZERO, NE, 0.0D0, OK )


      CALL TCASE ( 'MEQU: copying a 3x3 random matrix and ' &
      //           'checking strict equality between the random matrix ' &
      //           'and the copied matrix' )

      CALL MEQU ( MRAN, MOUT )

      CALL CHCKAD ( 'MOUT', MOUT, '=', MRAN, NE, 0.0D0, OK )

!
!     Cases to test XPOSE
!
      CALL TCASE ( 'XPOSE: generating the transpose of ' &
      //           'an index matrix, and checking equality between ' &
      //           'original and transposed elements' )

      CALL XPOSE ( MINDX, MOUT )

      INDX = 0
      DO J = 1, N
         DO I = 1, N
            INDX = INDX + 1
            EXPT = DBLE( INDX )
            CALL CHCKSD ( 'MOUT(J,I)', MOUT(J,I), '=', EXPT, &
                           0.0D0, OK )
         END DO
      END DO

!

      CALL TCASE ( 'XPOSE: generating the transpose of ' &
      //           'a random matrix, and checking equality between ' &
      //           'original and transposed elements' )

      CALL XPOSE ( MRAN, MOUT )

      DO J = 1, N
         DO I = 1, N
            CALL CHCKSD ( 'MOUT(J,I)', MOUT(J,I), '=', MRAN(I,J), &
                           0.0D0, OK )
         END DO
      END DO

!
!     Cases to test TRACE
!

      CALL TCASE ( 'TRACE: an index matrix, ' &
      //           'whose trace is 15.0D0' )

      EVAL = TRACE( MINDX )

      EXPT = 15.0D0

      CALL CHCKSD ( 'EVAL', EVAL, '=', EXPT, 0.0D0, OK )


      CALL TCASE ( 'TRACE: random matrix' )

      EVAL = TRACE( MRAN )

      EXPT = 0.0D0
      DO I = 1, N
         EXPT = EXPT + MRAN( I, I )
      END DO

      CALL CHCKSD ( 'EVAL', EVAL, '=', EXPT, 0.0D0, OK )

!
!     Cases to test DET
!

      CALL TCASE ( 'DET: random matrix' )

      EVAL = DET ( MRAN )

!
!     The determinant of a 3x3x matrix can be computed by adding three
!     up diagonal followed by subtracting three down-diagonal products.
!
      EXPT = MRAN(1,1) * MRAN(2,2) * MRAN(3,3) &
           + MRAN(1,2) * MRAN(2,3) * MRAN(3,1) &
           + MRAN(1,3) * MRAN(2,1) * MRAN(3,2) &
           - MRAN(1,1) * MRAN(2,3) * MRAN(3,2) &
           - MRAN(1,2) * MRAN(2,1) * MRAN(3,3) &
           - MRAN(1,3) * MRAN(2,2) * MRAN(3,1)

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, VTIGHT, OK )

!
!     Cases to test MXV
!
      CALL TCASE ( 'MXV: random matrix and random vector' )

      CALL MXV ( MRAN, VRAN, VOUT )

      DO I = 1, N
         DO J = 1, N
            VTEMP( J ) = MRAN( I, J )
         END DO
         VEXPT( I ) = VDOT( VTEMP, VRAN )
      END DO

      CALL CHCKAD ( 'VOUT', VOUT, '~', VEXPT, N, VTIGHT, OK )

!
!     Cases to test MTXV
!
      CALL TCASE ( 'MTXV: random matrix and random vector' )

      CALL MTXV ( MRAN, VRAN, VOUT )

      DO I = 1, N
         DO J = 1, N
            VTEMP( J ) = MRAN( J, I )
         END DO
         VEXPT( I ) = VDOT( VTEMP, VRAN )
      END DO

      CALL CHCKAD ( 'VOUT', VOUT, '=', VEXPT, N, 0.0D0, OK )

!
!     Cases to test VTMV
!
      CALL TCASE ( 'VTMV: random vector, random matrix, index vector ')

      EVAL = VTMV( VRAN, MRAN, VINDX )

!
!     First compute the product of MRAN and VINDX.
!
      CALL MXV( MRAN, VINDX, VTEMP )

!
!     Then, compute the expected value using the routine VDOT.
!
      EXPT  = VDOT( VRAN, VTEMP )

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, TIGHT, OK )

!
!     Cases to test MXM
!
      CALL TCASE ( 'MXM: Freivald''s Algorithm' )

!
!     Compute the product MOUT = MRAN * MINDX.
!
      CALL MXM ( MRAN, MINDX, MOUT )

!
!     Check MRAN * ( MINDX * VRAN ) = MOUT * VRAN, where VRAN is
!     an arbitrary vector.
!
      CALL MXV ( MINDX, VRAN, VOUT )
      CALL MXV ( MRAN, VOUT, VTEMP )
      CALL MXV ( MOUT, VRAN, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, N, TIGHT, OK )

!
!     Cases to test MTXM
!
      CALL TCASE ( 'MTXM: Freivald''s Algorithm' )

!
!     Compute the product MOUT = Transpose(MRAN) * MINDX.
!
      CALL MTXM ( MRAN, MINDX, MOUT )

!
!     Check Transpose(MRAN) * ( MINDX * VRAN ) = MOUT * VRAN,
!     where VRAN is an arbitrary vector.
!
      CALL MXV ( MINDX, VRAN, VOUT )
      CALL MTXV ( MRAN, VOUT, VTEMP )
      CALL MXV ( MOUT, VRAN, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, N, TIGHT, OK )

!
!     Cases to test MXMT
!
      CALL TCASE ( 'MXMT: Freivald''s Algorithm' )

!
!     Compute the product MOUT = MRAN * Transpose(MINDX).
!
      CALL MXMT ( MRAN, MINDX, MOUT )

!
!     Check MRAN * ( Transpose(MINDX) * VRAN ) = MOUT * VRAN,
!     where VRAN is an arbitrary vector.
!
      CALL MTXV ( MINDX, VRAN, VOUT )
      CALL MXV ( MRAN, VOUT, VTEMP )
      CALL MXV ( MOUT, VRAN, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, N, TIGHT, OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
