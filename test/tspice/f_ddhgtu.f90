!$Procedure F_DDHGTU ( ZZDDHGTU Test Family )

      SUBROUTINE F_DDHGTU ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHGTU
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
!     This routine exercises ZZDDHGTU's logic.  Functionality it
!     does not properly test at the moment:
!
!     (1) When GETLUN signals an error, does this routine return
!         UINDEX into a row with a -1 value for UTLUN(UINDEX).
!
!     Point (1) could be tested by adding a pile of RESLUN calls
!     to this module.
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
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
!
!-&

!
!     Local Variables
!
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               LUN
      INTEGER               UINDEX

      LOGICAL               OPENED
      LOGICAL               RESRVD

!
!     Unit Table
!
      INTEGER               NUT

      INTEGER               UTCST ( UTSIZE )
      INTEGER               UTHAN ( UTSIZE )
      INTEGER               UTLUN ( UTSIZE )
      LOGICAL               UTLCK ( UTSIZE )

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHGTU' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Empty unit table insertion.' )

!
!     Prepare the inputs and output default values.
!
      UTCST (1) = 1
      UTHAN (1) = 1
      UTLCK (1) = .TRUE.
      UTLUN (1) = 1

      NUT       = 0

      UINDEX    = -1

!
!     Invoke the module.
!
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

!
!     Check for an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now check the values.
!
      CALL CHCKSI ( 'UTCST(1)', UTCST(1), '=',  0, 0, OK )
      CALL CHCKSI ( 'UTHAN(1)', UTHAN(1), '=',  0, 0, OK )

      CALL CHCKSL ( 'UTLCK(1)', UTLCK(1), .FALSE.,    OK )

!     PEPPER: units come from NEWUNIT= when the file is opened, so a new
!     row of the unit table has no unit yet (-1), and the handle manager
!     does not reserve or free units (RESLUN/FRELUN).
      CALL CHCKSI ( 'UTLUN(1)', UTLUN(1), '=', -1, 0, OK )
      CALL CHCKSI ( 'NUT',      NUT,      '=',  1, 0, OK )

      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=',  1, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Zero cost row exists in the unit table' )

!
!     Prepare the inputs and output default values.
!
      NUT = 10

      DO I = 1, 10

         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I

      END DO

!
!     Create the zero cost row.
!
      UTCST (5) = 0

!
!     Actually get a logical unit from GETLUN and lock it down.
!
      CALL GETLUN ( LUN )
      CALL RESLUN ( LUN )
      UTLUN (5) = LUN

      UINDEX    = -1

!
!     Exercise the module. (Note: It will 'FRELUN' UTLUN(5) )
!
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

!
!     Check for an exception, one should not have been signaled.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now check the relevant values.
!
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=',  5, 0, OK )

      CALL CHCKSI ( 'UTCST(UINDEX)', UTCST(UINDEX), '=',  0, 0, OK )
      CALL CHCKSI ( 'UTHAN(UINDEX)', UTHAN(UINDEX), '=',  5, 0, OK )

      CALL CHCKSL ( 'UTLCK(UINDEX)', UTLCK(UINDEX), .FALSE.,    OK )

      CALL CHCKSI ( 'NUT',      NUT,      '=',  10, 0, OK )

!
!     Check to see if LUN is still reserved with RESLUN.
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
      CALL CHCKSL ( 'RESRVD', RESRVD, .TRUE., OK )

!
!     Just to be safe, free LUN.
!
      CALL FRELUN ( LUN )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No zero cost rows, expand table case.' )

!
!     Prepare the inputs and output default values.
!
      NUT = 10

      DO I = 1, 10

         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I

      END DO

      UINDEX    = -1

!
!     Prepare the initial values for the row that will be returned.
!
      UTCST (11) = -1
      UTHAN (11) = -1
      UTLCK (11) = .TRUE.
      UTLUN (11) = -1

!
!     Exercise the module.
!
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

!
!     Check for an exception, one should not have been signaled.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now check the relevant values.
!
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=', 11, 0, OK )

      CALL CHCKSI ( 'UTCST(UINDEX)', UTCST(UINDEX), '=',  0, 0, OK )
      CALL CHCKSI ( 'UTHAN(UINDEX)', UTHAN(UINDEX), '=',  0, 0, OK )

      CALL CHCKSL ( 'UTLCK(UINDEX)', UTLCK(UINDEX), .FALSE.,    OK )

!     PEPPER: units come from NEWUNIT= when the file is opened, so a new
!     row of the unit table has no unit yet (-1), and the handle manager
!     does not reserve or free units (RESLUN/FRELUN).
      CALL CHCKSI ( 'UTLUN(UINDEX)', UTLUN(UINDEX), '=', -1, 0, OK )

      CALL CHCKSI ( 'NUT',      NUT,      '=',  11, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Full table, no zero-cost rows.' )

!
!     Prepare the inputs and output default values.
!
      NUT = UTSIZE

      DO I = 1, UTSIZE

         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I

      END DO

      UINDEX    = -1

!
!     Lock a few low cost rows to exercise that logic.
!
      UTLCK (1) = .TRUE.
      UTLCK (2) = .TRUE.
      UTLCK (3) = .TRUE.

!
!     Since the fourth row will be selected by ZZDDHGTU, retrieve a
!     logical unit for it.
!
      CALL GETLUN ( LUN )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Store this unit in the LUN column of the unit table.
!
      UTLUN(4) = LUN

!
!     Now open a scratch file to connect to the unit.
!
      OPEN ( UNIT   = LUN, &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'SCRATCH', &
             IOSTAT = IOSTAT     )

      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Exercise the module.
!
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

!
!     Check for an exception, one should not have been signaled.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now check the relevant values.
!
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=', 4, 0, OK )

!
!     Determine if ZZDDHGTU properly closed the unit.
!
      INQUIRE ( UNIT   = LUN, &
                OPENED = OPENED, &
                IOSTAT = IOSTAT         )

      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

      CALL CHCKSL ( 'OPENED', OPENED, .FALSE., OK )

      CALL CHCKSI ( 'UTCST(UINDEX)', UTCST(UINDEX), '=',   0, 0, OK )
      CALL CHCKSI ( 'UTHAN(UINDEX)', UTHAN(UINDEX), '=',   0, 0, OK )
      CALL CHCKSI ( 'UTLUN(UINDEX)', UTLUN(UINDEX), '=', LUN, 0, OK )

      CALL CHCKSL ( 'UTLCK(UINDEX)', UTLCK(UINDEX), .FALSE.,    OK )

      CALL CHCKSI ( 'NUT',      NUT,      '=',  UTSIZE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Full table, monotonic decreasing cost, no zero.' )

!
!     Prepare the inputs and output default values.
!
      NUT = UTSIZE

      DO I = 1, UTSIZE

         UTCST (I) = (UTSIZE + 1 - I)*10
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I

      END DO

      UINDEX    = -1

!
!     Lock a few low cost rows to exercise that logic.
!
      UTLCK (UTSIZE - 2) = .TRUE.
      UTLCK (UTSIZE - 1) = .TRUE.
      UTLCK (UTSIZE    ) = .TRUE.

!
!     Since the fourth row will be selected by ZZDDHGTU, retrieve a
!     logical unit for it and open a scratch file.
!
      CALL GETLUN ( LUN )

      CALL CHCKXC ( .FALSE., ' ', OK )

      UTLUN (UTSIZE - 3) = LUN

      OPEN ( UNIT   = LUN, &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'SCRATCH', &
             IOSTAT = IOSTAT     )

      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Exercise the module.
!
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

!
!     Check for an exception, one should not have been signaled.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now check the relevant values.
!
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=', UTSIZE-3, 0, OK )

!
!     Determine if ZZDDHGTU properly closed the unit.
!
      INQUIRE ( UNIT   = LUN, &
                OPENED = OPENED, &
                IOSTAT = IOSTAT         )

      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

      CALL CHCKSL ( 'OPENED', OPENED, .FALSE., OK )

      CALL CHCKSI ( 'UTCST(UINDEX)', UTCST(UINDEX), '=',  0,   0, OK )
      CALL CHCKSI ( 'UTHAN(UINDEX)', UTHAN(UINDEX), '=',  0,   0, OK )
      CALL CHCKSI ( 'UTLUN(UINDEX)', UTLUN(UINDEX), '=',  LUN, 0, OK )

      CALL CHCKSL ( 'UTLCK(UINDEX)', UTLCK(UINDEX), .FALSE.,    OK )

      CALL CHCKSI ( 'NUT',      NUT,      '=',  UTSIZE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Full table, all rows locked exception.' )

!
!     Prepare the inputs and output default values.
!
      NUT = UTSIZE

      DO I = 1, UTSIZE

         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .TRUE.
         UTLUN (I) = I

      END DO

      UINDEX    = -1

!
!     Exercise the module.
!
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

!
!     Check for an exception, SPICE(BUG) should have been signaled.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
