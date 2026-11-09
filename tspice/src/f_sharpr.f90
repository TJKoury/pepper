!$Procedure F_SHARPR ( Test the SPICELIB routine SHARPR )

      SUBROUTINE F_SHARPR ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine SHARPR.
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
! $ Files
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
! $ Particulars
!
!     This routine tests the SPICELIB routine SHARPR.  SHARPR
!     "sharpens" a 3x3 matrix that is "nearly" a rotation matrix
!     by unitizing the columns and making the columns mutually
!     orthogonal.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 14-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI

      LOGICAL               ISROT

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local Parameters
!
      DOUBLE PRECISION      AMP
      PARAMETER           ( AMP    = 1.D-2 )

      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL   = 1.D-14 )

      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL   = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 1000 )


!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ANGLE  ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      NEARLY ( 3, 3 )
      DOUBLE PRECISION      NOISE  ( 3, 3 )
      DOUBLE PRECISION      Q      ( 3, 3 )
      DOUBLE PRECISION      QANGLE
      DOUBLE PRECISION      R      ( 3, 3 )

      INTEGER               CASE
      INTEGER               I
      INTEGER               J
      INTEGER               SEED

      LOGICAL               IS

!
!     Saved values
!

!
!     Initial values
!



!
!     Open the test family.
!
      CALL TOPEN ( 'F_SHARPR' )

      SEED = -1

      DO CASE = 1, NUMCAS
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Perturb a rotation matrix; then sharpen; case #.'

         CALL REPMI ( TITLE, '#', CASE, TITLE )
         CALL TCASE ( TITLE )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Construct a rotation matrix from three Euler angles.
!
         ANGLE(1) =  T_RANDD (  -PI(),       PI(),       SEED )
         ANGLE(2) =  T_RANDD (  -PI()/2.D0,  PI()/2.D0,  SEED )
         ANGLE(3) =  T_RANDD (  -PI(),       PI(),       SEED )


         CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 2, 3, R )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Construct a "noise" matrix with which to perturb the
!        rotation.
!
         DO I = 1, 3

            DO J = 1, 3
               NOISE(I,J) =  T_RANDD ( -AMP, AMP, SEED )
            END DO

         END DO

!
!        NEARLY is "nearly" a rotation.
!
         CALL VADDG ( R, NOISE, 9, NEARLY )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Now sharpen the rotation.
!
         CALL SHARPR ( NEARLY )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the result is a rotation.
!
         IS = ISROT ( NEARLY, NTOL, DTOL )

         CALL CHCKSL ( 'Is result a rotation?', IS, .TRUE., OK )

!
!        Make sure the result is not too different from R.
!        Express the quotient of R and NEARLY as a rotation;
!        measure the rotation angle.
!
         CALL MTXM ( NEARLY, R, Q )

         CALL RAXISA ( Q, AXIS, QANGLE )

         CALL CHCKSD ( 'QANGLE', QANGLE, '~', 0.D0, 4*AMP, OK )

      END DO

!
!     Now for some error handling tests.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SHARPR:  pass in a singular matrix.  All we '// &
                    'expect is that the routine doesn''t crash.'  )

      CALL CLEARD ( 9, R )

      CALL SHARPR ( R )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


