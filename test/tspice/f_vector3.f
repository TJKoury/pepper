C$Procedure F_VECTOR3 (Family of tests on 3-vector operations)

      SUBROUTINE F_VECTOR3 ( OK )

C$ Abstract
C
C     This routine performs rudimentary tests on a collection
C     of 3D-vector operation routines.
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
C-    TSPICE Version 1.0.0, 26-SEP-2014 (MCK)
C
C        Initial code to test 
C
C           ROTVEC
C           UCRSS
C           UNORM
C           VADD
C           VCRSS
C           VDIST
C           VDOT
C           VEQU
C           VHAT
C           VHATIP
C           VLCOM
C           VLCOM3
C           VMINUS
C           VNORM
C           VPACK
C           VPERP
C           VPROJ
C           VREL
C           VROTV
C           VSCL
C           VSCLIP
C           VSEP
C           VSUB
C           VUPACK
C           VZERO
C
C-&

C$ Index_Entries
C
C     test 3-vector operation routines
C-&

C
C     Test Utility Functions
C
      DOUBLE PRECISION      T_RANDD

C
C     SPICELIB Functions
C
      LOGICAL               VZERO
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VREL
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      TWOPI

C
C     Local parameters
C
      INTEGER               N
      PARAMETER           ( N = 3 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.0D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.0D-15 )

C
C     initial seed number for pseudo-random number generation
C
      INTEGER               SEED0
      PARAMETER           ( SEED0 = 1980518 )

C
C     Local Variables
C

C
C     An index
C
      INTEGER               I

C
C     Seed number for pseudo-random number generation
C
      INTEGER               SEED

      DOUBLE PRECISION      V_ZERO ( N )
      DOUBLE PRECISION      V_INDX ( N )
      DOUBLE PRECISION      V_RAN  ( N )

      DOUBLE PRECISION      V_OUT  ( N )
      DOUBLE PRECISION      V_EXPT ( N )
      DOUBLE PRECISION      V_TEMP ( N )

      DOUBLE PRECISION      VMAG
      DOUBLE PRECISION      EVAL
      DOUBLE PRECISION      EXPT
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      DUMMY
      DOUBLE PRECISION      SCALE1
      DOUBLE PRECISION      SCALE2
      DOUBLE PRECISION      SCALE3
      DOUBLE PRECISION      THETA

      LOGICAL               CHECK

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_VECTOR3' )

C
C     Form vectors to be used for more than one test case.
C

C
C     A zero vector
C
      DO I = 1, N
         V_ZERO(I) = 0.0D0
      END DO

C
C     An index vector, whose components are their indices
C
      DO I = 1, N
         V_INDX(I) = DBLE(I)
      END DO

C
C     A pseudo-random vector, whose components are
C     uniformly distributed on the interval [-1.0D0, +1.0D0]
C
      SEED = SEED0

      DO I = 1, N
         V_RAN(I) = T_RANDD ( -1.0D0, 1.0D0, SEED )
      END DO

C
C     Cases to test VZERO
C
      CALL TCASE ( 'VZERO: zero vector' )

      CHECK = VZERO ( V_ZERO )

      CALL CHCKSL ( 'CHECK', CHECK, .TRUE., OK )


      CALL TCASE ( 'VZERO: index vector' )

      CHECK = VZERO ( V_INDX )

      CALL CHCKSL ( 'CHECK', CHECK, .FALSE., OK )


      CALL TCASE ( 'VZERO: random vector' )

      CHECK = VZERO ( V_RAN )

      CALL CHCKSL ( 'CHECK', CHECK, .FALSE., OK )

C
C     Cases to test VEQU
C
      CALL TCASE ( 'VEQU: zero vector' )

      CALL VEQU ( V_ZERO, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )


      CALL TCASE ( 'VEQU: index vector' )

      CALL VEQU ( V_INDX, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_INDX, N, 0.0D0, OK )


      CALL TCASE ( 'VEQU: random vector' )

      CALL VEQU ( V_RAN, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )

C
C     Cases to test VADD
C
      CALL TCASE ( 'VADD: zero vector and random vector' )

      CALL VADD ( V_ZERO, V_RAN, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )


      CALL TCASE ( 'VADD: index vector and random vector' )

      CALL VADD ( V_INDX, V_RAN, V_OUT )

      DO I = 1, N
         V_EXPT(I) = V_INDX(I) + V_RAN(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, 0.0D0, OK )

C
C     Cases to test VSUB
C
      CALL TCASE ( 'VSUB: zero vector and random vector' )

      CALL VSUB ( V_RAN, V_ZERO, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )


      CALL TCASE ( 'VSUB: index vector and random vector' )

      CALL VSUB ( V_RAN, V_INDX, V_OUT )

      DO I = 1, N
         V_EXPT(I) = V_RAN(I) - V_INDX(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, 0.0D0, OK )

C
C     Cases to test VDOT.
C
      CALL TCASE ( 'VDOT: zero vector and random vector' )

      EVAL   = VDOT ( V_ZERO, V_RAN )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK )


      CALL TCASE ( 'VDOT: index vector and random vector' )

      EVAL = VDOT ( V_INDX, V_RAN )

      EXPT  = 0.0D0

      DO I = 1, N
         EXPT = EXPT + V_INDX(I) * V_RAN(I)
      END DO

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, 0.0D0, OK )

C
C     Cases to test VPACK
C
      CALL TCASE ( 'VPACK: random vector.' )

      X = V_RAN( 1 )
      Y = V_RAN( 2 )
      Z = V_RAN( 3 )

      CALL VPACK ( X, Y, Z, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )

C
C     Cases to test VUPACK
C
      CALL TCASE ( 'VUPACK: random vector' )

      CALL VUPACK ( V_RAN, X, Y, Z )

      V_EXPT( 1 ) = X
      V_EXPT( 2 ) = Y
      V_EXPT( 3 ) = Z

      CALL CHCKAD ( 'V_EXPT', V_EXPT, '~', V_RAN, N, 0.0D0, OK )

C
C     Cases to test VSCL
C
      CALL TCASE ( 'VSCL: scaling an index vector' )

      SCALE = T_RANDD( -1.0D0, 1.0D0, SEED )

      CALL VSCL ( SCALE, V_INDX, V_OUT )

      DO I = 1, N
         V_EXPT(I) = SCALE * V_INDX(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, 0.0D0, OK )


      CALL TCASE ( 'VSCL: scaling back to the index vector' )

      SCALE = 1.0D0/SCALE

      CALL VSCL ( SCALE, V_OUT, V_TEMP )

      CALL CHCKAD ( 'V_TEMP', V_TEMP, '~', V_INDX, N, TIGHT, OK )

C
C     Cases to test VSCLIP
C
      CALL TCASE ( 'VSCLIP: scaling an index vector' )

      SCALE = T_RANDD( -1.0D0, 1.0D0, SEED )

      CALL VEQU ( V_INDX, V_OUT )

      CALL VSCLIP ( SCALE, V_OUT )

      DO I = 1, N
         V_EXPT(I) = SCALE * V_INDX(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, 0.0D0, OK )

C
C     Cases to test VNORM
C
      CALL TCASE ( 'VNORM: zero vector' )

      EVAL = VNORM ( V_ZERO )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK)

      CALL TCASE ( 'Test VNORM with an index vector' )

      EVAL = VNORM ( V_INDX )

      EXPT = 0.0D0

      DO I = 1, N
         EXPT = EXPT + V_INDX(I) * V_INDX(I)
      END DO

      EXPT = SQRT( EXPT )

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, VTIGHT, OK )

C
C     Cases to test VMINUS
C
      CALL TCASE ( 'VMINUS: random vector' )

      CALL VMINUS ( V_RAN, V_OUT )

      CALL VADD ( V_RAN, V_OUT, V_TEMP )

      CALL CHCKAD ( 'V_TEMP', V_TEMP, '~', V_ZERO, N, 0.0D0, OK )

C
C     Cases to test UNORM
C
      CALL TCASE ( 'UNORM: zero vector' )

      CALL UNORM ( V_ZERO, V_OUT, VMAG )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )

      CALL CHCKSD ( 'VMAG', VMAG, '~', 0.0D0, 0.0D0, OK)


      CALL TCASE ( 'UNORM:  random vector.' )

      CALL UNORM ( V_RAN, V_OUT, VMAG )

      EXPT = VNORM ( V_RAN )
      CALL CHCKSD ( 'VMAG', VMAG, '~', EXPT, TIGHT, OK)

      EVAL = VNORM ( V_OUT )
      CALL CHCKSD ( 'EVAL', EVAL, '~', 1.0D0, TIGHT, OK)

      CALL VSCL ( VMAG, V_OUT, V_EXPT )
      CALL CHCKAD ( 'V_EXPT', V_EXPT, '~', V_RAN, N, TIGHT, OK )

C
C     Cases to test VHAT
C
      CALL TCASE ( 'VHAT: zero vector' )

      CALL VHAT ( V_ZERO, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )

      CALL TCASE ( 'VHAT: random vector.' )

      CALL VHAT ( V_RAN, V_OUT )

      EVAL = VNORM ( V_OUT )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 1.0D0, TIGHT, OK )

C
C     Cases to test VHATIP
C
      CALL TCASE ( 'VHATIP: zero vector' )

      CALL VEQU ( V_ZERO, V_OUT )

      CALL VHATIP ( V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, TIGHT, OK )

      CALL TCASE ( 'VHATIP: random vector' )

      CALL VEQU ( V_RAN, V_OUT )

      CALL VHATIP ( V_OUT )

      EVAL = VNORM ( V_OUT )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 1.0D0, TIGHT, OK )

C
C     Cases to test VDIST
C
      CALL TCASE ( 'VDIST: same vector' )

      EVAL = VDIST ( V_RAN, V_RAN )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK )

      CALL TCASE ( 'VDIST: index vector and random vector' )

      EVAL = VDIST ( V_INDX, V_RAN )

      CALL VSUB ( V_INDX, V_RAN, V_OUT )

      EXPT = VNORM ( V_OUT )

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, TIGHT, OK )

C
C     Cases to test VREL
C
      CALL TCASE ( 'VREL: two zero vectors' )

      EVAL = VREL ( V_ZERO, V_ZERO )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK )

      CALL TCASE ( 'VREL: index vector and random vector' )

      EVAL = VREL ( V_INDX, V_RAN )

      EXPT = VDIST ( V_INDX, V_RAN )
     .       / MAX( VNORM ( V_INDX ), VNORM ( V_RAN ) )

      CALL CHCKSD( 'EVAL', EVAL, '~', EXPT, TIGHT, OK )

C
C     Cases to test VSEP
C
      CALL TCASE ( 'VSEP: angle between '
     .//           'a zero vector and a random vector' )

      EVAL = VSEP ( V_ZERO, V_RAN )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, TIGHT, OK )


      CALL TCASE ( 'VSEP: angle between '
     .//           'a random vector and a zero vector' )

      EVAL = VSEP ( V_RAN, V_ZERO )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, TIGHT, OK)

C
C     Cases to test VLCOM
C
      CALL TCASE ( 'VLCOM: index vector and random vector' )

      SCALE = T_RANDD( -1.0D0, 1.0D0, SEED )

      DUMMY = T_RANDD( -1.0D0, 1.0D0, SEED )

      CALL VLCOM ( SCALE, V_INDX, DUMMY, V_RAN, V_OUT )

      DO I = 1, N
         V_EXPT(I) = SCALE * V_INDX(I) + DUMMY * V_RAN(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, TIGHT, OK )


      CALL TCASE ( 'VLCOM: zero vector and random vector' )

      CALL VLCOM ( SCALE, V_ZERO, 1.0D0, V_RAN, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )

C
C     Cases to test VLCOM3
C
      CALL TCASE ( 'VLCOM: index vector and random vector' )

      SCALE1 = T_RANDD( -1.0D0, 1.0D0, SEED )
      SCALE2 = T_RANDD( -1.0D0, 1.0D0, SEED )
      SCALE3 = T_RANDD( -1.0D0, 1.0D0, SEED )

      CALL VLCOM3 ( SCALE1, V_INDX, SCALE2, V_RAN, SCALE3, V_RAN,
     .                 V_OUT )

      DO I = 1, N
         V_EXPT(I) = SCALE1 * V_INDX(I) + SCALE2 * V_RAN(I)
     .               + SCALE3 * V_RAN(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, TIGHT, OK )

C
C     Cases to test VPROJ
C
      CALL TCASE ( 'VPROJ: projecting a random vector to '
     .//           'a zero vector' )

      CALL VPROJ ( V_RAN, V_ZERO, V_OUT )


      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )

      CALL TCASE ( 'VPROJ: projecting a random vector to '
     .//           'an index vector' )

      CALL VPROJ ( V_RAN, V_INDX, V_OUT )


C
C     Compute V_TEMP to be the unit vector along V_INDX.
C
      CALL VHAT ( V_INDX, V_TEMP )

C
C     Check ( V_RAN DOT V_TEMP ) .EQ. ( V_OUT DOT V_TEMP ).
C
      DUMMY  = VDOT ( V_RAN, V_TEMP )

      EVAL  = VDOT ( V_OUT, V_TEMP )

      CALL CHCKSD ( 'EVAL', EVAL, '~', DUMMY, TIGHT, OK )

C
C     Cases to test VPERP
C

      CALL TCASE ( 'VPERP: projecting a zero vector to '
     .//           'a random vector' )

      CALL VPERP( V_ZERO, V_RAN, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )


      CALL TCASE ( 'VPERP: projecting an index vector to '
     .//           'a random vector' )

      CALL VPERP( V_INDX, V_RAN, V_OUT )

      CALL VSUB ( V_INDX, V_OUT, V_TEMP )

      CALL VCRSS ( V_TEMP, V_RAN, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, TIGHT, OK )

C
C     Cases to test UCRSS
C
      CALL TCASE ( 'UCRSS: index vector and random vector' )

      CALL UCRSS ( V_INDX, V_RAN, V_OUT )

      EVAL = VDOT ( V_OUT, V_INDX )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, TIGHT, OK )

      EVAL = VDOT ( V_OUT, V_RAN )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, TIGHT, OK )

      EVAL = VDOT ( V_OUT, V_OUT )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 1.0D0, TIGHT, OK )


      CALL TCASE ( 'UCRSS: zero vector and random vector' )

      CALL UCRSS ( V_ZERO, V_RAN, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )

C
C     Cases to test VCRSS
C
      CALL TCASE ( 'VCRSS: index vector and random vector' )

      CALL VCRSS ( V_INDX, V_RAN, V_OUT )

      EVAL = VDOT ( V_OUT, V_INDX )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, TIGHT, OK )

      EVAL = VDOT ( V_OUT, V_RAN )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, TIGHT, OK )

      EVAL = ( VNORM ( V_INDX ) * VNORM ( V_RAN ) )**2
     .       - VNORM ( V_OUT )**2 - VDOT ( V_INDX , V_RAN ) **2

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, TIGHT, OK)


      CALL TCASE ( 'VCRSS: zero vector and random vector' )

      CALL VCRSS ( V_ZERO, V_RAN, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )

C
C     Cases to test VROTV
C
      CALL TCASE ( 'VROTV: rotating '
     .//           'a random vector wrt a zero vector' )

      THETA = T_RANDD( 0.0D0, TWOPI(), SEED )

      CALL VROTV ( V_RAN, V_ZERO, THETA, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )


      CALL TCASE ( 'VROTV: rotating '
     .//           'a random vector wrt an index vector' )

      THETA = T_RANDD( 0.0D0, TWOPI(), SEED )

      CALL VROTV ( V_RAN, V_INDX, THETA, V_OUT )

C
C     Angle between V_RAN and V_INDX should not be changed.
C
      EVAL = VSEP ( V_RAN, V_INDX ) - VSEP ( V_OUT, V_INDX )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, TIGHT, OK)

C
C     Cases to test ROTVEC
C
      CALL TCASE ( 'ROTVEC: rotating a random vector '
     .//           'with respect to the x-axis' )

      CALL ROTVEC( V_RAN, THETA, 1, V_OUT )

      V_TEMP( 1 ) = 1.0D0
      V_TEMP( 2 ) = 0.0D0
      V_TEMP( 3 ) = 0.0D0

      CALL VROTV ( V_RAN, V_TEMP, -THETA, V_EXPT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, TIGHT, OK )

      CALL ROTVEC( V_RAN, THETA, 2, V_OUT )

      V_TEMP( 1 ) = 0.0D0
      V_TEMP( 2 ) = 1.0D0
      V_TEMP( 3 ) = 0.0D0

      CALL VROTV ( V_RAN, V_TEMP, -THETA, V_EXPT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, TIGHT, OK )

      CALL ROTVEC ( V_RAN, THETA, 3, V_OUT )

      V_TEMP( 1 ) = 0.0D0
      V_TEMP( 2 ) = 0.0D0
      V_TEMP( 3 ) = 1.0D0

      CALL VROTV ( V_RAN, V_TEMP, -THETA, V_EXPT )
      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, TIGHT, OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
