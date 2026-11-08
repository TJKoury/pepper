!$Procedure F_KPBUG ( UNLOAD bug demonstration )

      SUBROUTINE F_KPBUG ( OK )

! $ Abstract
!
!     Demonstrate the HANDLE bug in keeper
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
!     This routine tests the handle-changing bug in the following
!     call sequence to FURNSH and UNLOAD
!     type 20 SPK data:
!
!     CALL FURNSH(SPK0)
!     CALL KINFO(SPK0, KTYPE, SSRC, HAN0, FOUND0)
!     CALL FURNSH(SPK0)
!     CALL KINFO(SPK0, KTYPE, SSRC, HAN1, FOUND1)
!     CALL UNLOAD(SPK0)
!     CALL KINFO(SPK0, KTYPE, SSRC, HAN2, FOUND2)
!
!   Now HAN2 != HAN0 and HAN1 but FOUND<n> are
!   all TRUE.
!

!
!
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
!     N.J. Bachman     (JPL)
!     G.M. Hockney     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 27-JAN-2017 (NJB) (GMH)
!
!        This routine has been updated so that, prior to exit,
!        it deletes the SPK it creates.
!
!        Deleted declarations of unused functions.
!
!        Renamed George's routine from FF_KEEPBUG
!        to F_KPBUG.
!
!     Original Version 1.0.0, 19-MAY-2015 (GMH)
!
!
!-&

!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'test.bsp' )

      INTEGER              FILSIZ
      PARAMETER          ( FILSIZ = 255 )

      INTEGER              TYPSIZ
      PARAMETER          ( TYPSIZ = 4 )

!
!     Local Variables
!
      CHARACTER*(FILSIZ)    KTYPE
      CHARACTER*(TYPSIZ)    KSRC

      INTEGER               HAN0
      INTEGER               HAN1
      INTEGER               HAN2
      INTEGER               HAN3

      LOGICAL               FOUND0
      LOGICAL               FOUND1
      LOGICAL               FOUND2
      LOGICAL               FOUND3


!
!     Saved variables
!
!
!     Save all local variables to avoid stack problems on some
!     platforms.
!
      SAVE


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_KPBUG' )


!
!     Open a new SPK file for writing.
!
!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Setup: create kernels.' )


      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create SPK file.' )
!
!     Create a small SPK file containing 1 record. We'll
!     use the moon as the target and the earth-moon barycenter
!     as the center.
!

!
!     Load test SPK to provide data.
!
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK0, .TRUE., HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HAN0 )
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Demonstrate multiple load bug.' )

      CALL FURNSH(SPK0)
      CALL KINFO(SPK0, KTYPE, KSRC, HAN0, FOUND0)
      CALL CHCKSL ( 'KEEPBUG 0', FOUND0, .TRUE., OK )

      CALL FURNSH(SPK0)
      CALL KINFO(SPK0, KTYPE, KSRC, HAN1, FOUND1)
      CALL CHCKSL ( 'KEEPBUG 1', FOUND1, .TRUE., OK )
      CALL CHCKSI ( 'KEEPBUG 2', HAN0, '=', HAN1, 0, OK )

      CALL UNLOAD(SPK0)
      CALL KINFO(SPK0, KTYPE, KSRC, HAN2, FOUND2)
      CALL CHCKSL ( 'KEEPBUG 3', FOUND2, .TRUE., OK )
      CALL CHCKSI ( 'KEEPBUG 4', HAN0, '=', HAN2, 0, OK )

      CALL UNLOAD(SPK0)
      CALL KINFO(SPK0, KTYPE, KSRC, HAN3, FOUND3)
      CALL CHCKSL ( 'KEEPBUG 5', FOUND3, .FALSE., OK )


      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_SUCCESS ( OK )
      RETURN
      END
