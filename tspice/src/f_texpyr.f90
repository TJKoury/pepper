!$Procedure F_TEXPYR ( Family of tests for TEXPYR )

      SUBROUTINE F_TEXPYR ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the routine TEXPYR and its companion
!     entry point TSETYR.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routines TEXPYR and TSETYR.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0, 07-DEC-2020 (JDR)
!
!        Added test case for non-positive inputs to TSETYR.
!
!        Edited the header and comments to comply with NAIF standard.
!
!-    TSPICE Version 2.0.0, 13-DEC-2001 (FST)
!
!        Updated this routine to restore and exercise the
!        default value of 1969 for TSETYR.
!
!-    TSPICE Version 1.0.0, 26-MAY-1998 (WLT)
!
!        Initial version.
!
!-&


! $ Index_Entries
!
!     Test family for TEXPYR and TSETYR routines
!
!-&


!
!     Local Variables
!
      INTEGER               YEAR
      INTEGER               EXPYR
      INTEGER               I
      INTEGER               J


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TEXPYR' )


!
!     *****************************************************************
!
!     Normal cases: TEXPYR and TSETYR
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure that in the default case two digit ' &
      //           'years range are mapped to the interval from ' &
      //           '1969 to 2068.')


      DO I = 0, 99

         YEAR = I

         CALL TEXPYR ( YEAR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .LE. 68 ) THEN
            EXPYR = 2000 + I
         ELSE
            EXPYR = 1900 + I
         END IF

         CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      END DO


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure that when we set the lower bound ' &
      //           'for the century that all two digit years are ' &
      //           'expanded appropriately. ' )


      CALL TSETYR ( 2025 )

      DO I = 0, 99

         YEAR = I

         CALL TEXPYR ( YEAR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .LE. 24 ) THEN
            EXPYR = 2100 + I
         ELSE
            EXPYR = 2000 + I
         END IF

         CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      END DO

!
!     Reset the lower bound and re-do the first set of tests.
!
      CALL TSETYR ( 1969 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 0, 99

         YEAR = I

         CALL TEXPYR ( YEAR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .LE. 68 ) THEN
            EXPYR = 2000 + I
         ELSE
            EXPYR = 1900 + I
         END IF

         CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure that non-positive years provided to ' &
      //           'TSETYR do not change the lower bound for the ' &
      //           'year expansion.' )

      DO J = -1, 0

         CALL TSETYR ( J )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO I = 0, 99

            YEAR = I

            CALL TEXPYR ( YEAR )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Lower bound should still be 1969.
!
            IF ( I .LE. 68 ) THEN
               EXPYR = 2000 + I
            ELSE
               EXPYR = 1900 + I
            END IF

            CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

         END DO

      END DO


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Make sure that years outside the inclusive ' &
      //           'range from 0 to 99 are not altered by ' &
      //           'TEXPYR. ' )

      YEAR  = 1928
      EXPYR = 1928

      CALL TEXPYR ( YEAR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      YEAR  = 100
      EXPYR = 100

      CALL TEXPYR ( YEAR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      YEAR  = -1
      EXPYR = -1

      CALL TEXPYR ( YEAR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
