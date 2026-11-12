!$Procedure F_ZZDDHNFC ( ZZDDHNFC tests )

      SUBROUTINE F_ZZDDHNFC ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the SPICELIB routine ZZDDHNFC.
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
!     This routine tests the SPICELIB routine ZZDDHNFC.
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
!
! $ Version
!
!-    TSPICE Version 1.0.0, 27-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )


!
!     Local Variables
!
      CHARACTER*(LNSIZE)    FMTSTR
      CHARACTER*(LNSIZE)    BFFSTR

      INTEGER               NATBFF

!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZDDHNFC' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fetch the integer BFF code for the host system.' )


!
!     Look up the BFF integer code.
!
      CALL ZZDDHNFC ( NATBFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up the equivalent string.
!
      CALL ZZDDHGSD ( 'BFF', NATBFF, BFFSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZPLATFM ( 'FILE_FORMAT', FMTSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSC ( 'BFFSTR', BFFSTR, '=', FMTSTR, OK )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!     None.
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


