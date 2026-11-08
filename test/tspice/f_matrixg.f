C$Procedure F_MATRIXG (Family of tests on general matrix operations)

      SUBROUTINE F_MATRIXG ( OK )

C$ Abstract
C
C     This routine performs rudimentary tests on a collection of
C     general matrix operation routines.
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
C-    TSPICE Version 1.1.0, 11-FEB-2017 (MCK) (BVS) (NJB)
C
C        Removed unneeded declarations.
C
C-    TSPICE Version 1.0.0, 27-JUL-2016 (MCK) (BVS)
C
C        Initial code to test
C
C           MEQUG
C           XPOSEG
C           TRACEG
C           MXVG
C           MTXVG
C           VTMVG
C           MXMG
C           MTXMG
C           MXMTG
C
C-&

C$ Index_Entries
C
C     test general matrix operation routines
C
C-&

C
C     Test Utility Functions
C

      DOUBLE PRECISION      T_RANDD

C
C     SPICELIB Functions
C

      DOUBLE PRECISION      TRACEG
      DOUBLE PRECISION      VDOTG
      DOUBLE PRECISION      VTMVG

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.0D-14 )

C
C     Initial seed number for pseudo-random number generation
C
      INTEGER               SEED0
      PARAMETER           ( SEED0  = 20140416 )

C
C     Number of rows in an arbitrary matrix
C
      INTEGER               NR
      PARAMETER           ( NR = 7 )

C
C     Number of columns in an arbitrary matrix
C
      INTEGER               NC
      PARAMETER           ( NC = 6 )

C
C     Number of rows (or columns) of a square matrix.
C     Should be set to MAX( NR, NC ).
C
      INTEGER               N
      PARAMETER           ( N = 7 )

C
C     Number of elements in an NR-by-NC matrix
C
      INTEGER               NE
      PARAMETER           ( NE = NR * NC )

C
C     Local Variables
C

C
C     An NR-by-NC pseudo-random matrix
C
      DOUBLE PRECISION      MRAN  ( NR, NC )

C
C     A single-column matrix
C
      DOUBLE PRECISION      MNX1  ( N,  1  )

C
C     A single-row matrix
C
      DOUBLE PRECISION      M1XN  ( 1,  N  )

 
C
C     Matrices having the same dimension as that of MRAN
C
      DOUBLE PRECISION      MOUT  ( NR, NC )

C
C     Transpose of MRAN
C
      DOUBLE PRECISION      XPOSEM ( NC, NR )

C
C     An N-by-N matrix
C
      DOUBLE PRECISION      MATSQ  ( N,  N  )

C
C     Some other work space matrices
C
      DOUBLE PRECISION      MNRN  ( NR, N  )
      DOUBLE PRECISION      MNNC  ( N,  NC )
      DOUBLE PRECISION      MNNR  ( N,  NR )
      DOUBLE PRECISION      MNCN  ( NC, N  )

C
C     A column vector
C
      DOUBLE PRECISION      VCOL  ( NC )

C
C     A row vector
C
      DOUBLE PRECISION      VROW  ( NR )

 
C
C     Some other work space vectors
C
      DOUBLE PRECISION      VOUT  ( N )
      DOUBLE PRECISION      VTEMP ( N )
      DOUBLE PRECISION      VEXPT ( N )

C
C     seed number for pseudo-random number generation
C
      INTEGER               SEED

C
C     An index
C
      INTEGER               I

C
C     A row index
C
      INTEGER               IR

C
C     A column index
C
      INTEGER               IC

C
C     Some double precision numbers
C
      DOUBLE PRECISION      EVAL
      DOUBLE PRECISION      EXPT

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_MATRIXG' )

C
C     Form arrays to be used for more than one test case.
C

C
C     Arbitrary matrices, whose components are pseudo-random
C     numbers uniformly distributed on the interval [-1.0D0,+1.0D0].
C
      SEED = SEED0

      DO IC = 1, NC
         DO IR = 1, NR
            MRAN(IR, IC) = T_RANDD ( -1.0D0, 1.0D0, SEED )
         END DO
      END DO

      DO I = 1, N
         DO IR = 1, NR
            MNRN(IR, I) = T_RANDD ( -1.0D0, 1.0D0, SEED )
         END DO
      END DO

      DO IC = 1, NC
         DO I = 1, N
            MNNC(I, IC) = T_RANDD ( -1.0D0, 1.0D0, SEED )
         END DO
      END DO

      DO I = 1, NR
         DO IR = 1, N
            MNNR(IR, I) = T_RANDD ( -1.0D0, 1.0D0, SEED )
         END DO
      END DO

      DO IC = 1, N
         DO I = 1, NC
            MNCN(I, IC) = T_RANDD ( -1.0D0, 1.0D0, SEED )
         END DO
      END DO

C
C     A single-row matrix, whose elements are their column index
C
      DO I = 1, N
         M1XN( 1, I ) = DBLE( I )
      END DO

C
C     A square index matrix, whose components are storage indices
C
      EVAL = 0.0D0

      DO IC = 1, N
         DO IR = 1, N
            EVAL = EVAL + 1.0D0
            MATSQ(IR, IC) = EVAL
         END DO
      END DO

C
C     Arbitrary vectors, whose components are pseudo-random
C     numbers uniformly distributed on the interval [-1.0D0,+1.0D0]
C
      DO IC = 1, NC
         VCOL(IC) = T_RANDD ( -1.0D0, 1.0D0, SEED )
      END DO

      DO IR = 1, NR
         VROW(IR) = T_RANDD ( -1.0D0, 1.0D0, SEED )
      END DO

C
C     Cases to test MEGUG
C

      CALL TCASE ( 'MEQUG: arbitrary matrix' )

      CALL MEQUG ( MRAN, NR, NC, MOUT )

C
C     Numerical tolerance for this test should be zero.
C
      CALL CHCKAD ( 'MOUT', MOUT, '~', MRAN, NE, 0.0D0, OK )

C
C     Cases to test XPOSEG
C

      CALL TCASE ( 'XPOSEG: 1-by-N matrix' )

      CALL XPOSEG ( M1XN, 1, N, MNX1 )

C
C     Numerical tolerance for this test should be zero.
C
      CALL CHCKAD ( 'MNX1', MNX1, '~', M1XN, N, 0.0D0, OK )


      CALL TCASE ( 'XPOSEG: N-by-1 matrix' )

      CALL XPOSEG ( MNX1, N, 1, M1XN )

C
C     Numerical tolerance for this test should be zero.
C
      CALL CHCKAD ( 'M1XN', M1XN, '~', MNX1, N, 0.0D0, OK )


      CALL TCASE ( 'XPOSEG: arbitrary matrix' )

      CALL XPOSEG ( MRAN, NR, NC, XPOSEM )

C
C     Check the equality of M = ( M + Transpose( Transpose( M ) ) ) / 2
C     for an arbitrary matrix M ( that is MRAN herein).
C
      DO IC = 1, NC
         DO IR = 1, NR
            MOUT(IR,IC) = ( MRAN(IR,IC) + XPOSEM(IC,IR) ) / 2.0D0
         END DO
      END DO

C
C     Numerical tolerance for this test should be zero.
C
      CALL CHCKAD ( 'MOUT', MOUT, '~', MRAN, NE, 0.0D0, OK )


      CALL TCASE ( 'XPOSEG: applying twice.' )

      CALL XPOSEG ( MRAN, NR, NC, XPOSEM )

      CALL XPOSEG ( XPOSEM, NC, NR, MOUT )

C
C     Numerical tolerance for this test should be zero.
C
      CALL CHCKAD ( 'MOUT', MOUT, '~', MRAN, NE, 0.0D0, OK )

C
C     Cases to test TRACEG
C
      CALL TCASE ( 'TRACEG: square integer matrix' )

      EVAL = TRACEG ( MATSQ, N )

      EXPT = 0.0D0
      DO I = 1, N
         EXPT = EXPT + MATSQ( I, I )
      END DO

C
C     Being MATSQ an integer matrix, numerical tolerance for this test
C     should be zero.
C
      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, 0.0D0, OK )

C
C     Cases to test MXVG
C
      CALL TCASE ( 'MXVG: multiplying an arbitrary matrix and '
     .//'an arbitrary column vector.' )

      CALL MXVG ( MRAN, VCOL, NR, NC, VOUT )

      DO IR = 1, NR
         DO IC = 1, NC
            VTEMP( IC ) = MRAN( IR, IC )
         END DO
         VEXPT( IR ) = VDOTG( VTEMP, VCOL, NC )
      END DO

      CALL CHCKAD ( 'VOUT', VOUT, '~', VEXPT, NR, TIGHT, OK )

C
C     Cases to test MTXVG
C
      CALL TCASE ( 'MTXVG: multiplying the transpose of '
     .//'an arbitrary matrix and an arbitrary vector' )

      CALL MTXVG ( MRAN, VROW, NC, NR, VOUT )

      DO IC = 1, NC
         DO IR = 1, NR
            VTEMP( IR ) = MRAN( IR, IC )
         END DO
         VEXPT( IC ) = VDOTG( VTEMP, VROW, NR )
      END DO

      CALL CHCKAD ( 'VOUT', VOUT, '~', VEXPT, NC, TIGHT, OK )

C
C     Cases to test VTMVG
C
      CALL TCASE ( 'VTMVG: ' )

      EVAL = VTMVG( VROW, MRAN, VCOL, NR, NC )

      EXPT = 0.0D0
      DO IC = 1, NC
         DO IR = 1, NR
            EXPT = EXPT + VROW(IR) * MRAN(IR,IC) * VCOL(IC)
         END DO
      END DO

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, TIGHT, OK )

C
C     Cases to test MXMG
C
      CALL TCASE ( 'MXMG: Freivald''s Algorithm' )

C
C     Compute the product MOUT = MNRN * MNNC.
C
      CALL MXMG ( MNRN, MNNC, NR, N, NC, MOUT )

C
C     Check MNRN * ( MNNC * VCOL ) = MOUT * VCOL, where VCOL is
C     an arbitrary vector.
C
      CALL MXVG ( MNNC, VCOL, N, NC, VOUT )
      CALL MXVG ( MNRN, VOUT, NR, N, VTEMP )
      CALL MXVG ( MOUT, VCOL, NR, NC, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, NR, TIGHT, OK )

C
C     Cases to test MTXMG
C
      CALL TCASE ( 'MTXMG: Freivald''s Algorithm' )

C
C     Compute the product MOUT = Transpose(MNNR) * MNNC.
C
      CALL MTXMG ( MNNR, MNNC, NR, N, NC, MOUT )

C
C     Check Transpose(MNNR) * ( MNNC * VCOL ) = MOUT * VCOL,
C     where VCOL is an arbitrary vector.
C
      CALL MXVG ( MNNC, VCOL, N, NC, VOUT )
      CALL MTXVG ( MNNR, VOUT, NR, N, VTEMP )
      CALL MXVG ( MOUT, VCOL, NR, NC, VOUT  )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, NR, TIGHT, OK )

C
C     Cases to test MXMTG
C
      CALL TCASE ( 'MXMTG: Freivald''s Algorithm' )

C
C     Compute the product MOUT = MNRN * Transpose(MNCN).
C
      CALL MXMTG ( MNRN, MNCN, NR, N, NC, MOUT )

C
C     Check MNRN * ( Transpose(MNCN) * VCOL ) = MOUT * VCOL,
C     where VCOL is an arbitrary vector.
C
      CALL MTXVG ( MNCN, VCOL, N, NC, VOUT )
      CALL MXVG ( MNRN, VOUT, NR, N, VTEMP )
      CALL MXVG ( MOUT, VCOL, NR, NC, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, NR, TIGHT, OK )

      CALL T_SUCCESS ( OK )

C
C     Tests for zero/negative NR/NC inputs.
C
C     THERE CAN BE NO SUCH TESTS BECAUSE OF THE WAY INPUT AND
C     OUTPUT MATRICES ARE DECLARED IN THESE ROUTINES.
C

      RETURN
      END
