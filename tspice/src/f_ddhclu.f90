!$Procedure F_DDHCLU ( ZZDDHCLU Test Family )

      SUBROUTINE F_DDHCLU ( OK )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHCLU
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
!     This routine exercises ZZDDHCLU's logic.
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
!     SPICELIB Functions
!
      INTEGER               ZZDDHCLU

!
!     Local Variables
!
      INTEGER               I
      INTEGER               NUT
      INTEGER               OUTPUT

      LOGICAL               UTLCK ( UTSIZE )

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHCLU' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Empty table exceptional case.' )

!
!     Prepare the inputs and output default values.
!
      UTLCK(1) = .TRUE.
      NUT      = 0

      OUTPUT   = 10

!
!     Invoke the module.
!
      OUTPUT = ZZDDHCLU ( UTLCK, NUT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'ZZDDHCLU', OUTPUT, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-empty table, nominal case.' )

!
!     Prepare the inputs and output default values.
!
      NUT = 10

      DO I = 1, NUT

         UTLCK(I) = .FALSE.

      END DO

      UTLCK(NUT) = .TRUE.
      UTLCK(1)   = .TRUE.

      OUTPUT = 0

!
!     Invoke the module.
!
      OUTPUT = ZZDDHCLU ( UTLCK, NUT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'ZZDDHCLU', OUTPUT, '=', 2, 0, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
