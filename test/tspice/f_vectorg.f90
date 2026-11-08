!$Procedure F_VECTORG (Family of tests on general vector operations)

      SUBROUTINE F_VECTORG ( OK )

! $ Abstract
!
!     This routine performs rudimentary tests on a collection of
!     general vector operation routines.
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
!-    TSPICE Version 1.0.0, 26-SEP-2014 (MCK)
!
!        Initial code to test
!
!           UNORMG
!           VADDG
!           VDISTG
!           VDOTG
!           VEQUG
!           VHATG
!           VLCOMG
!           VNORMG
!           VPROJG
!           VRELG
!           VSCLG
!           VSEPG
!           VSUBG
!           VZEROG
!
!-&

! $ Index_Entries
!
!     test general vector operation routines
!
!-&

!
!     Test Utility Functions
!

      DOUBLE PRECISION      T_RANDD

!
!     SPICELIB Functions
!
      LOGICAL               VZEROG
      DOUBLE PRECISION      VDOTG
      DOUBLE PRECISION      VNORMG
      DOUBLE PRECISION      VDISTG
      DOUBLE PRECISION      VRELG
      DOUBLE PRECISION      VSEPG

!
!     Local parameters
!
      INTEGER               N
      PARAMETER           ( N     = 99       )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.0D-12  )

      DOUBLE PRECISION      TINY
      PARAMETER           ( TINY  = 1.0D-99  )

      INTEGER               SEED0
      PARAMETER           ( SEED0 = 19810518 )

!
!     Local Variables
!
      LOGICAL               VZERO

      DOUBLE PRECISION      V_ZERO ( N )
      DOUBLE PRECISION      V_INDX ( N )
      DOUBLE PRECISION      V_RAN  ( N )

      DOUBLE PRECISION      V_OUT  ( N )
      DOUBLE PRECISION      V_EXPT ( N )
      DOUBLE PRECISION      V_TMP  ( N )

      DOUBLE PRECISION      VECMAG
      DOUBLE PRECISION      EVAL
      DOUBLE PRECISION      EXPT
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      DUMMY

      INTEGER               I
      INTEGER               SEED

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_VECTORG' )

!
!     Form vectors to be used for more than one test case.
!

!
!     A zero vector
!
      DO I = 1, N
         V_ZERO(I) = 0.0D0
      END DO

!
!     An index vector, whose components are their indices.
!
      DO I = 1, N
         V_INDX(I) = DBLE(I)
      END DO

!
!     A random vector, whose components are
!     uniformly distributed on the interval [-1.0D0,+1.0D0].
!
      SEED = SEED0

      DO I = 1, N
         V_RAN(I) = T_RANDD ( -1.0D0, 1.0D0, SEED )
      END DO

!
!     Cases to test VZEROG
!
      CALL TCASE ( 'Test VZEROG with a zero vector' )

      VZERO = VZEROG ( V_ZERO, N )

      CALL CHCKSL ( 'VZERO', VZERO, .TRUE., OK )


      CALL TCASE ( 'Test VZEROG with an index vector' )

      VZERO = VZEROG ( V_INDX, N )

      CALL CHCKSL ( 'VZERO', VZERO, .FALSE., OK )


      CALL TCASE ( 'Test VZEROG with a zero dimension' )

      VZERO = VZEROG ( V_ZERO, 0 )

      CALL CHCKSL ( 'VZERO', VZERO, .FALSE., OK )


      CALL TCASE ( 'Test VZEROG with a negative dimension' )

      VZERO = VZEROG ( V_ZERO, -N )

      CALL CHCKSL ( 'VZERO', VZERO, .FALSE., OK )

!
!     Cases to test VEQUG
!
      CALL TCASE ( 'Test VEQUG with a zero vector' )

      CALL VEQUG ( V_ZERO, N, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )


      CALL TCASE ( 'Test VEQUG with an index vector' )

      CALL VEQUG ( V_INDX, N, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_INDX, N, 0.0D0, OK )


      CALL TCASE ( 'Test VEQUG with a random vector' )

      CALL VEQUG ( V_RAN, N, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )

!
!     Cases to test VADDG
!
      CALL TCASE ( 'Test VADDG by adding a zero vector and a random ' &
      //           'vector' )

      CALL VADDG ( V_ZERO, V_RAN, N, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )


      CALL TCASE ( 'Test VADDG by adding an index vector and a random ' &
      //           'vector' )

      CALL VADDG ( V_INDX, V_RAN, N, V_OUT )

      DO I = 1, N
         V_EXPT(I) = V_INDX(I) + V_RAN(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, 0.0D0, OK )

!
!     Cases to test VSUBG
!
      CALL TCASE ( 'Test VSUBG by subtracting a zero vector from a ' &
      //           'random vector')


      CALL VSUBG ( V_RAN, V_ZERO, N, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )


      CALL TCASE ( 'Test VADDG by subtracting an index vector from ' &
      //           'a random vector')

      CALL VSUBG ( V_RAN, V_INDX, N, V_OUT )

      DO I = 1, N
         V_EXPT(I) = V_RAN(I) - V_INDX(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, 0.0D0, OK )

!
!     Cases to test VDOTG
!
      CALL TCASE ( 'Test VDOTG with a zero vector and a random ' &
      //           'vector' )

      EVAL = VDOTG ( V_ZERO, V_RAN, N )

      EXPT = 0.0D0

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, 0.0D0, OK)


      CALL TCASE ( 'Test VDOTG with an index vector and a random ' &
      //           'vector' )

      EVAL   = VDOTG ( V_INDX, V_RAN, N )

      EXPT = 0.0D0

      DO I = 1, N
         EXPT = EXPT + V_INDX(I) * V_RAN(I)
      END DO

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, TINY, OK)

!
!     Cases to test VNORMG
!
      CALL TCASE ( 'Test VNORMG with a zero vector' )

      EVAL = VNORMG ( V_ZERO, N )

      EXPT = 0.0D0

      CALL CHCKSD( 'EVAL', EVAL, '~', EXPT, 0.0D0, OK)


      CALL TCASE ( 'Test VNORMG with an index vector' )

      EVAL = VNORMG ( V_INDX, N )

      DUMMY = 0.0D0

      DO I = 1, N
         DUMMY = DUMMY + V_INDX(I) * V_INDX(I)
      END DO

      EXPT = DSQRT( DUMMY )

      CALL CHCKSD ( 'EVAL', EVAL, '~/', EXPT, TIGHT, OK)

!
!     Cases to test UNORMG
!
      CALL TCASE ( 'Test UNORMG with a zero vector' )

      CALL UNORMG ( V_ZERO, N, V_OUT, VECMAG )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )

      CALL CHCKSD ( 'VECMAG', VECMAG, '~', 0.0D0, 0.0D0, OK)


      CALL TCASE ( 'Test UNORMG with a random vector' )

      CALL UNORMG ( V_RAN, N, V_OUT, VECMAG )

      EXPT = VNORMG ( V_RAN, N )

      CALL CHCKSD ( 'VECMAG', VECMAG, '~', EXPT, 0.0D0, OK)

      EXPT = 1.0D0

      DUMMY = VNORMG ( V_OUT, N )

      CALL CHCKSD ( 'DUMMY', DUMMY, '~', EXPT, TIGHT, OK)

      CALL VSCLG ( VECMAG, V_OUT, N, V_TMP )

      CALL CHCKAD ( 'V_TMP', V_TMP, '~', V_RAN, N, TIGHT, OK )

!
!     Cases to test VHATG
!
      CALL TCASE ( 'Test VHATG with a zero vector' )

      CALL VHATG ( V_ZERO, N, V_OUT )

!
!     VHATG returns a zero vector for a zero vector input.
!
      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )

      CALL TCASE ( 'Test VHATG with a random vector' )

      CALL VHATG ( V_RAN, N, V_OUT )

      EVAL = VNORMG ( V_OUT, N )

      CALL CHCKSD( 'EVAL', EVAL, '~', 1.0D0, TIGHT, OK)

!
!     Cases to test VSCLG
!
      CALL TCASE ( 'Test VSCLG by scaling an index vector' )

      SCALE = T_RANDD( -1.0D0, 1.0D0, SEED )

      CALL VSCLG ( SCALE, V_INDX, N, V_OUT )

      DO I = 1, N
         V_EXPT(I) = SCALE * V_INDX(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, TIGHT, OK )


      CALL TCASE ( 'Test VSCLG by scaling back to the index vector' )

      SCALE = 1.0D0/SCALE

      CALL VSCLG ( SCALE, V_OUT, N, V_TMP )

      CALL CHCKAD ( 'V_TMP', V_TMP, '~', V_INDX, N, TIGHT, OK )

!
!     Cases to test VDISTG
!
      CALL TCASE ( 'Test VDISTG using the same vector' )

      EVAL = VDISTG ( V_INDX, V_INDX, N )

!
!     Distance between same vectors should be zero.
!
      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK )


      CALL TCASE ( 'Test VDISTG using an index vector and a ' &
      //           'random vector' )

      EVAL = VDISTG ( V_INDX, V_RAN, N )

      CALL VSUBG ( V_INDX, V_RAN, N, V_TMP )

      EXPT = VNORMG ( V_TMP, N )

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, TIGHT, OK )

!
!     Cases to test VRELG
!
      CALL TCASE ( 'Test VRELG using two zero vectors' )

      EVAL = VRELG ( V_ZERO, V_ZERO, N )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK )


      CALL TCASE ( 'Test VRELG using an index vector ' &
      //           'and a random vector' )

      EVAL = VRELG ( V_INDX, V_RAN, N )

      EXPT = VDISTG ( V_INDX, V_RAN, N ) &
          / MAX( VNORMG ( V_INDX, N ), VNORMG ( V_RAN, N ) )

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, 0.0D0, OK )

!
!     Cases to test VSEPG
!
      CALL TCASE ( 'Test VSEPG for the angle between ' &
      //           'a zero vector and a random vector' )

      EVAL = VSEPG ( V_ZERO, V_RAN, N )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK )


      CALL TCASE ( 'Test VSEPG for the angle between ' &
      //           'a random vector and a zero vector' )

      EVAL = VSEPG ( V_RAN, V_ZERO, N )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK )


      CALL TCASE ( 'Test VSEPG for the angle between ' &
      //           'two same vectors' )

      EVAL = VSEPG ( V_INDX, V_INDX, N )

      CALL CHCKSD ( 'EVAL', EVAL, '~', 0.0D0, 0.0D0, OK )


      CALL TCASE ( 'Test VSEPG for the angle between ' &
      //           'a random vector and an index vector' )

      EVAL = VSEPG ( V_INDX, V_RAN, N )

      EXPT = DACOS ( VDOTG ( V_INDX, V_RAN, N ) / &
             ( VNORMG ( V_INDX, N ) * VNORMG ( V_RAN, N ) ) )

      CALL CHCKSD ( 'EVAL', EVAL, '~', EXPT, TIGHT, OK )

!
!     Cases to test VLCOMG
!
      CALL TCASE ( 'Test VLCOMG using an index vector and ' &
      //           'a random vector' )

      SCALE = T_RANDD ( -1.0D0, 1.0D0, SEED )
      DUMMY = T_RANDD ( -1.0D0, 1.0D0, SEED )

      CALL VLCOMG ( N, SCALE, V_INDX, DUMMY, V_RAN, V_OUT )

      DO I = 1, N
         V_EXPT(I) = SCALE * V_INDX(I) + DUMMY * V_RAN(I)
      END DO

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_EXPT, N, 0.0D0, OK )


      CALL TCASE ( 'Test VLCOMG using an zero vector and ' &
      //           'a random vector' )

      SCALE = T_RANDD( -1.0D0, 1.0D0, SEED )

      CALL VLCOMG ( N, SCALE, V_ZERO, 1.0D0, V_RAN, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_RAN, N, 0.0D0, OK )

!
!     Cases to test VPROJG
!
      CALL TCASE ( 'Test VPROJG by projecting a random vector to ' &
      //           'a zero vector' )

      CALL VPROJG ( V_RAN, V_ZERO, N, V_OUT )

      CALL CHCKAD ( 'V_OUT', V_OUT, '~', V_ZERO, N, 0.0D0, OK )


      CALL TCASE ( 'Test VPROJG by projecting a random vector to ' &
      //           'an index vector' )

      CALL VPROJG ( V_RAN, V_INDX, N, V_OUT )

!
!     Compute V_TMP to be the unit vector along V_INDX.
!
      CALL VHATG ( V_INDX, N, V_TMP )

!
!     Check ( V_RAN DOT V_TMP ) .EQ. ( V_OUT DOT V_TMP ).
!
      DUMMY  = VDOTG ( V_RAN, V_TMP, N )

      EVAL   = VDOTG ( V_OUT, V_TMP, N )

      CALL CHCKSD ( 'EVAL', EVAL, '~', DUMMY, TIGHT, OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
