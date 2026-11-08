!$Procedure F_DDHRMU ( ZZDDHRMU Test Family )

      SUBROUTINE F_DDHRMU ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHRMU
!     routine.
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

      INCLUDE              'zzddhman.inc'

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
!     This routine exercises ZZDDHRMU's logic.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 13-DEC-2011 (EDW)
!
!       All variables to SAVE for f2c'd version compiled with Borland.
!       Implemented a proper SPICE header.
!
!-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
!
!-&

!
!     Local Variables
!
      INTEGER               I
      INTEGER               LUN
      INTEGER               NFT
      INTEGER               NUT
      INTEGER               UINDEX
      LOGICAL               RESRVD

!
!     The unit table columns
!
      INTEGER               UTCST ( UTSIZE )
      INTEGER               UTHAN ( UTSIZE )
      INTEGER               UTLUN ( UTSIZE )
      LOGICAL               UTLCK ( UTSIZE )

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHRMU' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Empty unit table exceptional case.' )

!
!     Prepare the inputs and output default values.
!
      UINDEX = 1
      NUT    = 0
      NFT    = 1

!
!     Invoke the module.
!
      CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN, &
                      UTLCK,  UTLUN, NUT           )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'UINDEX out of range exception.' )

!
!     Prepare the inputs and output default values.
!
      NUT = 10

      DO I = 1, NUT

         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I

      END DO

      UINDEX = 5000
      NFT    = 12

!
!     Invoke the module.
!
      CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN, &
                      UTLCK,  UTLUN, NUT           )

!
!     Check for the presence of the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test table compression case.' )

!
!     Prepare the inputs and output default values.
!
      NUT = 4

      DO I = 1, NUT

         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I

      END DO

      UINDEX = 2
      NFT    = 3

!
!     Invoke the module.
!
      CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN, &
                      UTLCK,  UTLUN, NUT           )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'UTCST(1)', UTCST(1), '=', 10, 0, OK )
      CALL CHCKSI ( 'UTHAN(1)', UTHAN(1), '=', 1,  0, OK )
      CALL CHCKSL ( 'UTLCK(1)', UTLCK(1), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(1)', UTLUN(1), '=', 1,  0, OK )

      CALL CHCKSI ( 'UTCST(2)', UTCST(2), '=', 30, 0, OK )
      CALL CHCKSI ( 'UTHAN(2)', UTHAN(2), '=', 3,  0, OK )
      CALL CHCKSL ( 'UTLCK(2)', UTLCK(2), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(2)', UTLUN(2), '=', 3,  0, OK )

      CALL CHCKSI ( 'UTCST(3)', UTCST(3), '=', 40, 0, OK )
      CALL CHCKSI ( 'UTHAN(3)', UTHAN(3), '=', 4,  0, OK )
      CALL CHCKSL ( 'UTLCK(3)', UTLCK(3), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(3)', UTLUN(3), '=', 4,  0, OK )

      CALL CHCKSI ( 'NUT',      NUT,      '=', 3,  0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Zero row test case.' )

!
!     Prepare the inputs and output default values.
!
      NUT = 4

      DO I = 1, NUT

         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I

      END DO

      UINDEX = 2
      NFT    = 4

!
!     Setup row 2 with an actual logical unit.
!
      CALL GETLUN ( LUN )
      UTLUN(2) = LUN

!
!     Invoke the module.
!
      CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN, &
                      UTLCK,  UTLUN, NUT           )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs
!
      CALL CHCKSI ( 'UTCST(1)', UTCST(1), '=', 10, 0, OK )
      CALL CHCKSI ( 'UTHAN(1)', UTHAN(1), '=', 1,  0, OK )
      CALL CHCKSL ( 'UTLCK(1)', UTLCK(1), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(1)', UTLUN(1), '=', 1,  0, OK )

      CALL CHCKSI ( 'UTCST(2)', UTCST(2), '=', 0,   0, OK )
      CALL CHCKSI ( 'UTHAN(2)', UTHAN(2), '=', 0,   0, OK )
      CALL CHCKSL ( 'UTLCK(2)', UTLCK(2), .FALSE.,     OK )
      CALL CHCKSI ( 'UTLUN(2)', UTLUN(2), '=', LUN, 0, OK )

      CALL CHCKSI ( 'UTCST(3)', UTCST(3), '=', 30, 0, OK )
      CALL CHCKSI ( 'UTHAN(3)', UTHAN(3), '=', 3,  0, OK )
      CALL CHCKSL ( 'UTLCK(3)', UTLCK(3), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(3)', UTLUN(3), '=', 3,  0, OK )

      CALL CHCKSI ( 'UTCST(4)', UTCST(4), '=', 40, 0, OK )
      CALL CHCKSI ( 'UTHAN(4)', UTHAN(4), '=', 4,  0, OK )
      CALL CHCKSL ( 'UTLCK(4)', UTLCK(4), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(4)', UTLUN(4), '=', 4,  0, OK )

      CALL CHCKSI ( 'NUT',      NUT,      '=', 4,  0, OK )

!
!     Check to see if LUN is reserved.
!
      CALL T_TSTRLN ( LUN, RESRVD )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the value of RESRVD.
!
!     PEPPER: units come from NEWUNIT= when the file is opened, so a new
!     row of the unit table has no unit yet (-1), and the handle manager
!     does not reserve or free units (RESLUN/FRELUN).
      CALL CHCKSL ( 'RESRVD', RESRVD, .FALSE., OK )

!
!     Free up the logical unit.
!
      CALL FRELUN ( LUN )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
