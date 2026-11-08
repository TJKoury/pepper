!$Procedure ZZDDHRCM ( Private --- DDH Request Count )

      SUBROUTINE ZZDDHRCM ( NUT, UTCST, REQCNT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Manage augmentation of the handle to logical unit request counter
!     and the cost column in the unit table.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               NUT
      INTEGER               UTCST  ( * )
      INTEGER               REQCNT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUT        I   Number of entries in the unit table.
!     UTCST     I/O  Cost column of the unit table.
!     REQCNT    I/O  Value of the HLU request counter.
!
! $ Detailed_Input
!
!     NUT        is the number of entries in the unit table.
!
!     UTCST      is the current cost column of the unit table.
!
!     REQCNT     is the current value of the HLU request counter to
!                adjust.
!
! $ Detailed_Output
!
!     UTCST      is the updated cost column of the unit table.  In
!                the nominal case, UTCST will not be adjusted, but
!                if REQCNT overflows, then adjustments will be made
!                to approximately preserve the priority.
!
!     REQCNT     is the updated value of the request counter.
!                Nominally this will be 1 more than the input
!                value.  However, in the case where REQCNT will
!                exceed INTMAX it will be assigned to INTMAX()/2 + 1.
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
!     Error free.
!
!     1) If REQCNT on input is INTMAX(), then REQCNT on output will
!        be assigned INTMAX()/2 + 1 and the UTCST column will be
!        recomputed.
!
! $ Particulars
!
!     This module manages the request counter and the cost column of
!     the unit table which is used to determine the expense of
!     disconnecting a handle from its logical unit.
!
!     In the nominal mode of operation, the request counter is simply
!     incremented by one and the cost column remains untouched.
!     However, when the request counter passed into the routine is
!     INTMAX, then REQCNT is not incremented.  In an attempt to preserve
!     the relationships between costs, all entries in the cost column
!     are halved and REQCNT is set to INTMAX()/2 + 1.  This has the
!     effect of preserving the cost relationships between rows, except
!     in half the cases where subsequent cost values are present.
!
!     The occurrence of rollover is rare, and thus the destruction of
!     relative cost relationships as well.
!
! $ Examples
!
!     See ZZDDHHLU for sample usage.
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
!-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               INTMAX

!
!     Local Variables
!
      INTEGER               I

!
!     Check to see if REQCNT is INTMAX, otherwise just increment
!     REQCNT.
!
      IF ( REQCNT .EQ. INTMAX() ) THEN

         REQCNT = INTMAX()/2 + 1

         DO I = 1,NUT
            UTCST(I) = MAX(1, UTCST(I)/2 )
         END DO

      ELSE

         REQCNT = REQCNT + 1

      END IF

      RETURN
      END
