!$Procedure F_DDHRCM ( ZZDDHRCM Test Family )

      SUBROUTINE F_DDHRCM ( OK )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHRCM
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
!     This routine exercises ZZDDHRCM's logic.
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
      INTEGER               INTMAX

!
!     Local Variables
!
      INTEGER               CKCST ( UTSIZE )
      INTEGER               I
      INTEGER               NUT
      INTEGER               REQCNT
      INTEGER               UTCST ( UTSIZE )

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHRCM' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Empty table with REQCNT less than INTMAX.' )

!
!     Prepare the inputs and output default values.
!
      UTCST(1) = 500
      NUT      = 0

      REQCNT   = 10000

!
!     Invoke the module.
!
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'REQCNT', REQCNT, '=', 10001, 0, OK )

      CALL CHCKSI ( 'NUT', NUT, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Empty table with REQCNT at INTMAX.' )

!
!     Prepare the inputs and output default values.
!
      UTCST(1) = 500
      NUT      = 0

      REQCNT   = INTMAX()

!
!     Invoke the module.
!
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'REQCNT', REQCNT, '=', INTMAX()/2 + 1, 0, OK )

      CALL CHCKSI ( 'NUT', NUT, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-empty table, REQCNT less than INTMAX.' )

!
!     Prepare the inputs and output default values.
!
      NUT = 10

      DO I = 1, NUT

         UTCST(I) = 10000*I
         CKCST(I) = 10000*I

      END DO

      REQCNT = 101000000

!
!     Invoke the module.
!
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'REQCNT', REQCNT, '=', 101000001, 0, OK )

      CALL CHCKSI ( 'UTCST(1)',  UTCST(1),  '=', CKCST(1),  0, OK )
      CALL CHCKSI ( 'UTCST(2)',  UTCST(2),  '=', CKCST(2),  0, OK )
      CALL CHCKSI ( 'UTCST(3)',  UTCST(3),  '=', CKCST(3),  0, OK )
      CALL CHCKSI ( 'UTCST(4)',  UTCST(4),  '=', CKCST(4),  0, OK )
      CALL CHCKSI ( 'UTCST(5)',  UTCST(5),  '=', CKCST(5),  0, OK )
      CALL CHCKSI ( 'UTCST(6)',  UTCST(6),  '=', CKCST(6),  0, OK )
      CALL CHCKSI ( 'UTCST(7)',  UTCST(7),  '=', CKCST(7),  0, OK )
      CALL CHCKSI ( 'UTCST(8)',  UTCST(8),  '=', CKCST(8),  0, OK )
      CALL CHCKSI ( 'UTCST(9)',  UTCST(9),  '=', CKCST(9),  0, OK )
      CALL CHCKSI ( 'UTCST(10)', UTCST(10), '=', CKCST(10), 0, OK )

      CALL CHCKSI ( 'NUT', NUT, '=', 10, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-empty table, REQCNT is INTMAX.' )

!
!     Prepare the inputs and output default values.
!
      NUT = 10

      DO I = 1, NUT

         UTCST(I) = 10000*I
         CKCST(I) =  5000*I

      END DO

!
!     Set REQCNT to INTMAX and UTCST(7) to 1 to test the
!     MAX(1,...) logic.
!
      REQCNT   = INTMAX()
      UTCST(7) = 1
      CKCST(7) = 1

!
!     Invoke the module.
!
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'REQCNT', REQCNT, '=', INTMAX()/2 + 1, 0, OK )

      CALL CHCKSI ( 'UTCST(1)',  UTCST(1),  '=', CKCST(1),  0, OK )
      CALL CHCKSI ( 'UTCST(2)',  UTCST(2),  '=', CKCST(2),  0, OK )
      CALL CHCKSI ( 'UTCST(3)',  UTCST(3),  '=', CKCST(3),  0, OK )
      CALL CHCKSI ( 'UTCST(4)',  UTCST(4),  '=', CKCST(4),  0, OK )
      CALL CHCKSI ( 'UTCST(5)',  UTCST(5),  '=', CKCST(5),  0, OK )
      CALL CHCKSI ( 'UTCST(6)',  UTCST(6),  '=', CKCST(6),  0, OK )
      CALL CHCKSI ( 'UTCST(7)',  UTCST(7),  '=', CKCST(7),  0, OK )
      CALL CHCKSI ( 'UTCST(8)',  UTCST(8),  '=', CKCST(8),  0, OK )
      CALL CHCKSI ( 'UTCST(9)',  UTCST(9),  '=', CKCST(9),  0, OK )
      CALL CHCKSI ( 'UTCST(10)', UTCST(10), '=', CKCST(10), 0, OK )

      CALL CHCKSI ( 'NUT', NUT, '=', 10, 0, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
