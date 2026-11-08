!$Procedure F_MATRIXG (Family of tests on general matrix operations)

      SUBROUTINE F_MATRIXG ( OK )

! $ Abstract
!
!     This routine performs rudimentary tests on a collection of
!     general matrix operation routines.
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
!-    TSPICE Version 1.1.0, 11-FEB-2017 (MCK) (BVS) (NJB)
!
!        Removed unneeded declarations.
!
!-    TSPICE Version 1.0.0, 27-JUL-2016 (MCK) (BVS)
!
!        Initial code to test
!
!           MEQUG
!           XPOSEG
!           TRACEG
!           MXVG
!           MTXVG
!           VTMVG
!           MXMG
!           MTXMG
!           MXMTG
!
!-&

! $ Index_Entries
!
!     test general matrix operation routines
!
!-&

!
!     Test Utility Functions
!

      DOUBLE PRECISION      T_RANDD

!
!     SPICELIB Functions
!

      DOUBLE PRECISION      TRACEG
      DOUBLE PRECISION      VDOTG
      DOUBLE PRECISION      VTMVG

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.0D-14 )

!
!     Initial seed number for pseudo-random number generation
!
      INTEGER               SEED0
      PARAMETER           ( SEED0  = 20140416 )

!
!     Number of rows in an arbitrary matrix
!
      INTEGER               NR
      PARAMETER           ( NR = 7 )

!
!     Number of columns in an arbitrary matrix
!
      INTEGER               NC
      PARAMETER           ( NC = 6 )

!
!     Number of rows (or columns) of a square matrix.
!     Should be set to MAX( NR, NC ).
!
      INTEGER               N
      PARAMETER           ( N = 7 )

!
!     Number of elements in an NR-by-NC matrix
!
      INTEGER               NE
      PARAMETER           ( NE = NR * NC )

!
!     Local Variables
!

!
!     An NR-by-NC pseudo-random matrix
!
      DOUBLE PRECISION      MRAN  ( NR, NC )

!
!     A single-column matrix
!
      DOUBLE PRECISION      MNX1  ( N,  1  )

!
!     A single-row matrix
!
      DOUBLE PRECISION      M1XN  ( 1,  N  )


!
!     Matrices having the same dimension as that of MRAN
!
      DOUBLE PRECISION      MOUT  ( NR, NC )

!
!     Transpose of MRAN
!
      DOUBLE PRECISION      XPOSEM ( NC, NR )

!
!     An N-by-N matrix
!
      DOUBLE PRECISION      MATSQ  ( N,  N  )

!
!     Some other work space matrices
!
      DOUBLE PRECISION      MNRN  ( NR, N  )
      DOUBLE PRECISION      MNNC  ( N,  NC )
      DOUBLE PRECISION      MNNR  ( N,  NR )
      DOUBLE PRECISION      MNCN  ( NC, N  )

!
!     A column vector
!
      DOUBLE PRECISION      VCOL  ( NC )

!
!     A row vector
!
      DOUBLE PRECISION      VROW  ( NR )


!
!     Some other work space vectors
!
      DOUBLE PRECISION      VOUT  ( N )
      DOUBLE PRECISION      VTEMP ( N )
      DOUBLE PRECISION      VEXPT ( N )

!
!     seed number for pseudo-random number generation
!
      INTEGER               SEED

!
!     An index
!
      INTEGER               I

!
!     A row index
!
      INTEGER               IR

!
!     A column index
!
      INTEGER               IC

!
!     Some double precision numbers
!
      DOUBLE PRECISION      EVAL
      DOUBLE PRECISION      EXPT

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_MATRIXG' )

!
!     Form arrays to be used for more than one test case.
!

!
!     Arbitrary matrices, whose components are pseudo-random
!     numbers uniformly distributed on the interval [-1.0D0,+1.0D0].
!
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

!
!     A single-row matrix, whose elements are their column index
!
      DO I = 1, N
         M1XN( 1, I ) = DBLE( I )
      END DO

!
!     A square index matrix, whose components are storage indices
!
      EVAL = 0.0D0

      DO IC = 1, N
         DO IR = 1, N
            EVAL = EVAL + 1.0D0
            MATSQ(IR, IC) = EVAL
         END DO
      END DO

!
!     Arbitrary vectors, whose components are pseudo-random
!     numbers uniformly distributed on the interval [-1.0D0,+1.0D0]
!
      DO IC = 1, NC
         VCOL(IC) = T_RANDD ( -1.0D0, 1.0D0, SEED )
      END DO

      DO IR = 1, NR
         VROW(IR) = T_RANDD ( -1.0D0, 1.0D0, SEED )
      END DO

!
!     Cases to test MEGUG
!

      CALL TCASE ( 'MEQUG: arbitrary matrix' )

      CALL MEQUG ( MRAN, NR, NC, MOUT )

!
!     Numerical tolerance for this test should be zero.
!
      CALL CHCKAD ( 'MOUT', MOUT, '~', MRAN, NE, 0.0D0, OK )

!
!     Cases to test XPOSEG
!

      CALL TCASE ( 'XPOSEG: 1-by-N matrix' )

      CALL XPOSEG ( M1XN, 1, N, MNX1 )

!
!     Numerical tolerance for this test should be zero.
!
      CALL CHCKAD ( 'MNX1', MNX1, '~', M1XN, N, 0.0D0, OK )


      CALL TCASE ( 'XPOSEG: N-by-1 matrix' )

      CALL XPOSEG ( MNX1, N, 1, M1XN )

!
!     Numerical tolerance for this test should be zero.
!
      CALL CHCKAD ( 'M1XN', M1XN, '~', MNX1, N, 0.0D0, OK )


      CALL TCASE ( 'XPOSEG: arbitrary matrix' )

      CALL XPOSEG ( MRAN, NR, NC, XPOSEM )

!
!     Check the equality of M = ( M + Transpose( Transpose( M ) ) ) / 2
!     for an arbitrary matrix M ( that is MRAN herein).
!
      DO IC = 1, NC
         DO IR = 1, NR
            MOUT(IR,IC) = ( MRAN(IR,IC) + XPOSEM(IC,IR) ) / 2.0D0
         END DO
      END DO

!
!     Numerical tolerance for this test should be zero.
!
      CALL CHCKAD ( 'MOUT', MOUT, '~', MRAN, NE, 0.0D0, OK )


      CALL TCASE ( 'XPOSEG: applying twice.' )

      CALL XPOSEG ( MRAN, NR, NC, XPOSEM )

      CALL XPOSEG ( XPOSEM, NC, NR, MOUT )

!
!     Numerical tolerance for this test should be zero.
!
      CALL CHCKAD ( 'MOUT', MOUT, '~', MRAN, NE, 0.0D0, OK )

!
!     Cases to test TRACEG
!
      CALL TCASE ( 'TRACEG: square integer matrix' )

      EVAL = TRACEG ( MATSQ, N )

      EXPT = 0.0D0
      DO I = 1, N
         EXPT = EXPT + MATSQ( I, I )
      END DO

!
!     Being MATSQ an integer matrix, numerical tolerance for this test
!     should be zero.
!
      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, 0.0D0, OK )

!
!     Cases to test MXVG
!
      CALL TCASE ( 'MXVG: multiplying an arbitrary matrix and ' &
      //'an arbitrary column vector.' )

      CALL MXVG ( MRAN, VCOL, NR, NC, VOUT )

      DO IR = 1, NR
         DO IC = 1, NC
            VTEMP( IC ) = MRAN( IR, IC )
         END DO
         VEXPT( IR ) = VDOTG( VTEMP, VCOL, NC )
      END DO

      CALL CHCKAD ( 'VOUT', VOUT, '~', VEXPT, NR, TIGHT, OK )

!
!     Cases to test MTXVG
!
      CALL TCASE ( 'MTXVG: multiplying the transpose of ' &
      //'an arbitrary matrix and an arbitrary vector' )

      CALL MTXVG ( MRAN, VROW, NC, NR, VOUT )

      DO IC = 1, NC
         DO IR = 1, NR
            VTEMP( IR ) = MRAN( IR, IC )
         END DO
         VEXPT( IC ) = VDOTG( VTEMP, VROW, NR )
      END DO

      CALL CHCKAD ( 'VOUT', VOUT, '~', VEXPT, NC, TIGHT, OK )

!
!     Cases to test VTMVG
!
      CALL TCASE ( 'VTMVG: ' )

      EVAL = VTMVG( VROW, MRAN, VCOL, NR, NC )

      EXPT = 0.0D0
      DO IC = 1, NC
         DO IR = 1, NR
            EXPT = EXPT + VROW(IR) * MRAN(IR,IC) * VCOL(IC)
         END DO
      END DO

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, TIGHT, OK )

!
!     Cases to test MXMG
!
      CALL TCASE ( 'MXMG: Freivald''s Algorithm' )

!
!     Compute the product MOUT = MNRN * MNNC.
!
      CALL MXMG ( MNRN, MNNC, NR, N, NC, MOUT )

!
!     Check MNRN * ( MNNC * VCOL ) = MOUT * VCOL, where VCOL is
!     an arbitrary vector.
!
      CALL MXVG ( MNNC, VCOL, N, NC, VOUT )
      CALL MXVG ( MNRN, VOUT, NR, N, VTEMP )
      CALL MXVG ( MOUT, VCOL, NR, NC, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, NR, TIGHT, OK )

!
!     Cases to test MTXMG
!
      CALL TCASE ( 'MTXMG: Freivald''s Algorithm' )

!
!     Compute the product MOUT = Transpose(MNNR) * MNNC.
!
      CALL MTXMG ( MNNR, MNNC, NR, N, NC, MOUT )

!
!     Check Transpose(MNNR) * ( MNNC * VCOL ) = MOUT * VCOL,
!     where VCOL is an arbitrary vector.
!
      CALL MXVG ( MNNC, VCOL, N, NC, VOUT )
      CALL MTXVG ( MNNR, VOUT, NR, N, VTEMP )
      CALL MXVG ( MOUT, VCOL, NR, NC, VOUT  )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, NR, TIGHT, OK )

!
!     Cases to test MXMTG
!
      CALL TCASE ( 'MXMTG: Freivald''s Algorithm' )

!
!     Compute the product MOUT = MNRN * Transpose(MNCN).
!
      CALL MXMTG ( MNRN, MNCN, NR, N, NC, MOUT )

!
!     Check MNRN * ( Transpose(MNCN) * VCOL ) = MOUT * VCOL,
!     where VCOL is an arbitrary vector.
!
      CALL MTXVG ( MNCN, VCOL, N, NC, VOUT )
      CALL MXVG ( MNRN, VOUT, NR, N, VTEMP )
      CALL MXVG ( MOUT, VCOL, NR, NC, VOUT )

      CALL CHCKAD ( 'VOUT', VOUT, '~', VTEMP, NR, TIGHT, OK )

      CALL T_SUCCESS ( OK )

!
!     Tests for zero/negative NR/NC inputs.
!
!     THERE CAN BE NO SUCH TESTS BECAUSE OF THE WAY INPUT AND
!     OUTPUT MATRICES ARE DECLARED IN THESE ROUTINES.
!

      RETURN
      END
