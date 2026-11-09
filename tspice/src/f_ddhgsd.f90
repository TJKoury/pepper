!$Procedure F_DDHGSD ( ZZDDHGSD Test Family )

      SUBROUTINE F_DDHGSD ( OK )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHGSD
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
!     This routine exercises ZZDDHGSD's logic.
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
!     Local Parameters
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 32 )

!
!     Local Variables
!
      CHARACTER*(LINLEN)    OUTARY ( 4 )
      CHARACTER*(LINLEN)    OUTPUT

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHGSD' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unknown Class and ID Exceptions' )

!
!     Prepare the inputs and output default values.
!
      OUTPUT = ' '

!
!     Invoke the module.
!
      CALL ZZDDHGSD ( 'UNKNOWN', 0, OUTPUT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSC ( 'OUTPUT', OUTPUT, '=', ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Abnormal Input (case) and (justification)' )

!
!     Prepare the inputs and output default values.
!
      OUTPUT = ' '

!
!     Invoke the module.
!
      CALL ZZDDHGSD ( '   MEthOd', READ, OUTPUT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSC ( 'OUTPUT', OUTPUT, '=', 'READ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Method Label Lookups' )

!
!     Prepare the inputs and output default values.
!
      OUTARY(1) = ' '
      OUTARY(2) = ' '
      OUTARY(3) = ' '
      OUTARY(4) = ' '

!
!     Invoke the module.
!
      CALL ZZDDHGSD ( 'METHOD', READ,   OUTARY(1) )
      CALL ZZDDHGSD ( 'METHOD', WRITE,  OUTARY(2) )
      CALL ZZDDHGSD ( 'METHOD', SCRTCH, OUTARY(3) )
      CALL ZZDDHGSD ( 'METHOD', NEW,    OUTARY(4) )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSC ( 'METHOD(READ)',   OUTARY(1), '=', 'READ',    OK )
      CALL CHCKSC ( 'METHOD(WRITE)',  OUTARY(2), '=', 'WRITE',   OK )
      CALL CHCKSC ( 'METHOD(SCRTCH)', OUTARY(3), '=', 'SCRATCH', OK )
      CALL CHCKSC ( 'METHOD(NEW)',    OUTARY(4), '=', 'NEW',     OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Architecture Label Lookups' )

!
!     Prepare the inputs and output default values.
!
      OUTARY(1) = ' '
      OUTARY(2) = ' '

!
!     Invoke the module.
!
      CALL ZZDDHGSD ( 'ARCH', DAF,  OUTARY(1) )
      CALL ZZDDHGSD ( 'ARCH', DAS,  OUTARY(2) )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSC ( 'ARCH(DAF)',  OUTARY(1), '=', 'DAF', OK )
      CALL CHCKSC ( 'ARCH(DAS)',  OUTARY(2), '=', 'DAS', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Binary File Format Label Lookups' )

!
!     Prepare the inputs and output default values.
!
      OUTARY(1) = ' '
      OUTARY(2) = ' '
      OUTARY(3) = ' '
      OUTARY(4) = ' '


!
!     Invoke the module.
!
      CALL ZZDDHGSD ( 'BFF', BIGI3E,  OUTARY(1) )
      CALL ZZDDHGSD ( 'BFF', LTLI3E,  OUTARY(2) )
      CALL ZZDDHGSD ( 'BFF', VAXGFL,  OUTARY(3) )
      CALL ZZDDHGSD ( 'BFF', VAXDFL,  OUTARY(4) )


!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSC ( 'BFF(BIGI3E)',  OUTARY(1), '=', 'BIG-IEEE', OK )
      CALL CHCKSC ( 'BFF(LTLI3E)',  OUTARY(2), '=', 'LTL-IEEE', OK )
      CALL CHCKSC ( 'BFF(VAXGFL)',  OUTARY(3), '=', 'VAX-GFLT', OK )
      CALL CHCKSC ( 'BFF(VAXDFL)',  OUTARY(4), '=', 'VAX-DFLT', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
