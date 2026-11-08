C$Procedure F_MATRIX3 (Family of tests on 3x3-matrix operations)

      SUBROUTINE F_MATRIX3 ( OK )

C$ Abstract
C
C     This routine performs rudimentary tests on a collection of
C     3x3-matrix operation routines.
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
C     None.
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
C     M.C. Kim      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 24-AUG-2016 (MCK) (BVS)
C
C        Initial code to test
C
C           DET
C           MEQU
C           MTXM
C           MTXV
C           MXM
C           MXMT
C           MXV
C           TRACE
C           VTMV
C           XPOSE
C
C-&

C$ Index_Entries
C
C     test 3x3 matrix operation routines
C
C-&

C
C     Test Utility Functions
C

      DOUBLE PRECISION      T_RANDD

C
C     SPICELIB Functions
C

      DOUBLE PRECISION      DET
      DOUBLE PRECISION      TRACE
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VTMV

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.0D-14 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.0D-15 )

C
C     Initial seed number for pseudo-random number generation
C
      INTEGER               SEED0
      PARAMETER           ( SEED0 = 19920810 )

C
C     Dimension of 3-by-3 square matrices
C
      INTEGER               N
      PARAMETER           ( N = 3 )

C
C     Number of elements in an N-by-N matrix.
C
      INTEGER               NE
      PARAMETER           ( NE = N * N )

C
C     Local Variables
C

C
C     indices
C
      INTEGER               I
      INTEGER               J
      INTEGER               INDX

C
C     Seed number for pseudo-random number generation
C
      INTEGER               SEED

C
C     3-component vectors
C
      DOUBLE PRECISION      VINDX ( N )
      DOUBLE PRECISION      VRAN  ( N )
      DOUBLE PRECISION      VOUT  ( N )
      DOUBLE PRECISION      VEXPT ( N )
      DOUBLE PRECISION      VTEMP ( N )

C
C     3-by-3 matrices
C
      DOUBLE PRECISION      MZERO ( N, N )
      DOUBLE PRECISION      MRAN  ( N, N )
      DOUBLE PRECISION      MINDX ( N, N )
      DOUBLE PRECISION      MOUT  ( N, N )

C
C     Evaluated number from a library routine.
C
      DOUBLE PRECISION      EVAL

C
C     Expected value of EVAL
C
      DOUBLE PRECISION      EXPT

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_MATRIX3' )

C
C     Form arrays to be used for more than one test case.
C

C
C     A zero matrix
C
      DO J = 1, N
         DO I = 1, N
            MZERO( I, J ) = 0.0D0
         END DO
      END DO

C
C     An index vector, whose components are their indices
C
      DO I = 1, N
         VINDX(I) = DBLE(I)
      END DO

C
C     An index matrix, whose elements are their column-wise
C     storage indices
C
      INDX = 0
      DO J = 1, N
         DO I = 1, N
            INDX = INDX + 1
            MINDX( I, J ) = DBLE( INDX )
         END DO
      END DO

C
C     Initialize the seed number of random number generation.
C
      SEED = SEED0

C
C     Form a random vector, whose components are
C     uniformly distributed on the interval [-1.0D0, +1.0D0].
C

      DO I = 1, N
         VRAN(I) = T_RANDD ( -1.0D0, +1.0D0, SEED )
      END DO

C
C     Form a random matrix, whose elements are
C     uniformly distributed on the interval [-1.0D0, +1.0D0].
C
      DO J = 1, N
         DO I = 1, N
            MRAN( I, J ) = T_RANDD ( -1.0D0, +1.0D0, SEED )
         END DO
      END DO

C
C     Cases to test MEQU
C

      CALL TCASE ( 'MEQU: copying a 3x3 zero matrix and '
     .//           'checking strict equality between the zero matrix '
     .//           'and the copied matrix' )

      CALL MEQU ( MZERO, MOUT )

      CALL CHCKAD ( 'MOUT', MOUT, '=', MZERO, NE, 0.0D0, OK )


      CALL TCASE ( 'MEQU: copying a 3x3 random matrix and '
     .//           'checking strict equality between the random matrix '
     .//           'and the copied matrix' )

      CALL MEQU ( MRAN, MOUT )

      CALL CHCKAD ( 'MOUT', MOUT, '=', MRAN, NE, 0.0D0, OK )

C
C     Cases to test XPOSE
C
      CALL TCASE ( 'XPOSE: generating the transpose of '
     .//           'an index matrix, and checking equality between '
     .//           'original and transposed elements' )

      CALL XPOSE ( MINDX, MOUT )

      INDX = 0
      DO J = 1, N
         DO I = 1, N
            INDX = INDX + 1
            EXPT = DBLE( INDX )
            CALL CHCKSD ( 'MOUT(J,I)', MOUT(J,I), '=', EXPT,
     .                     0.0D0, OK )
         END DO
      END DO

C

      CALL TCASE ( 'XPOSE: generating the transpose of '
     .//           'a random matrix, and checking equality between '
     .//           'original and transposed elements' )

      CALL XPOSE ( MRAN, MOUT )

      DO J = 1, N
         DO I = 1, N
            CALL CHCKSD ( 'MOUT(J,I)', MOUT(J,I), '=', MRAN(I,J),
     .                     0.0D0, OK )
         END DO
      END DO

C
C     Cases to test TRACE
C

      CALL TCASE ( 'TRACE: an index matrix, '
     .//           'whose trace is 15.0D0' )

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

C
C     Cases to test DET
C

      CALL TCASE ( 'DET: random matrix' )

      EVAL = DET ( MRAN )

C
C     The determinant of a 3x3x matrix can be computed by adding three
C     up diagonal followed by subtracting three down-diagonal products.
C
      EXPT = MRAN(1,1) * MRAN(2,2) * MRAN(3,3)
     .     + MRAN(1,2) * MRAN(2,3) * MRAN(3,1)
     .     + MRAN(1,3) * MRAN(2,1) * MRAN(3,2)
     .     - MRAN(1,1) * MRAN(2,3) * MRAN(3,2)
     .     - MRAN(1,2) * MRAN(2,1) * MRAN(3,3)
     .     - MRAN(1,3) * MRAN(2,2) * MRAN(3,1)

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, VTIGHT, OK )

C
C     Cases to test MXV
C
      CALL TCASE ( 'MXV: random matrix and random vector' )

      CALL MXV ( MRAN, VRAN, VOUT )

      DO I = 1, N
         DO J = 1, N
            VTEMP( J ) = MRAN( I, J )
         END DO
         VEXPT( I ) = VDOT( VTEMP, VRAN )
      END DO

      CALL CHCKAD ( 'VOUT', VOUT, '~', VEXPT, N, VTIGHT, OK )

C
C     Cases to test MTXV
C
      CALL TCASE ( 'MTXV: random matrix and random vector' )

      CALL MTXV ( MRAN, VRAN, VOUT )

      DO I = 1, N
         DO J = 1, N
            VTEMP( J ) = MRAN( J, I )
         END DO
         VEXPT( I ) = VDOT( VTEMP, VRAN )
      END DO

      CALL CHCKAD ( 'VOUT', VOUT, '=', VEXPT, N, 0.0D0, OK )

C
C     Cases to test VTMV
C
      CALL TCASE ( 'VTMV: random vector, random matrix, index vector ')

      EVAL = VTMV( VRAN, MRAN, VINDX )

C
C     First compute the product of MRAN and VINDX.
C
      CALL MXV( MRAN, VINDX, VTEMP )

C
C     Then, compute the expected value using the routine VDOT.
C
      EXPT  = VDOT( VRAN, VTEMP )

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, TIGHT, OK )

C
C     Cases to test MXM
C
      CALL TCASE ( 'MXM: Freivald''s Algorithm' )

C
C     Compute the product MOUT = MRAN * MINDX.
C
      CALL MXM ( MRAN, MINDX, MOUT )

C
C     Check MRAN * ( MINDX * VRAN ) = MOUT * VRAN, where VRAN is
C     an arbitrary vector.
C
      CALL MXV ( MINDX, VRAN, VOUT )
      CALL MXV ( MRAN, VOUT, VTEMP )
      CALL MXV ( MOUT, VRAN, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, N, TIGHT, OK )

C
C     Cases to test MTXM
C
      CALL TCASE ( 'MTXM: Freivald''s Algorithm' )

C
C     Compute the product MOUT = Transpose(MRAN) * MINDX.
C
      CALL MTXM ( MRAN, MINDX, MOUT )

C
C     Check Transpose(MRAN) * ( MINDX * VRAN ) = MOUT * VRAN,
C     where VRAN is an arbitrary vector.
C
      CALL MXV ( MINDX, VRAN, VOUT )
      CALL MTXV ( MRAN, VOUT, VTEMP )
      CALL MXV ( MOUT, VRAN, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, N, TIGHT, OK )

C
C     Cases to test MXMT
C
      CALL TCASE ( 'MXMT: Freivald''s Algorithm' )

C
C     Compute the product MOUT = MRAN * Transpose(MINDX).
C
      CALL MXMT ( MRAN, MINDX, MOUT )

C
C     Check MRAN * ( Transpose(MINDX) * VRAN ) = MOUT * VRAN,
C     where VRAN is an arbitrary vector.
C
      CALL MTXV ( MINDX, VRAN, VOUT )
      CALL MXV ( MRAN, VOUT, VTEMP )
      CALL MXV ( MOUT, VRAN, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, N, TIGHT, OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
