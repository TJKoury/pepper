!$Procedure F_GFREFN ( GFREFN family tests )

      SUBROUTINE F_GFREFN ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFREFN
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
!     None.
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
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.1, 19-DEC-2010 (EDW)
!
!        Corrected typo in CHCKSD call, "~" to "~/".
!
!-    TSPICE Version 1.0.0, 03-MAR-2009 (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )

      DOUBLE PRECISION      T
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      TEXP
      DOUBLE PRECISION      RLOG

      INTEGER               I
      INTEGER               SEED1

      CHARACTER*(128)        TXT

      LOGICAL                S1
      LOGICAL                S2

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFREFN' )

      SEED1  = -82763653

!
!     Note, 5000 is an arbitrary value.
!
      DO I = 1, 5000

!
!        Set T1 and T2 so that TEXP = (T1 + T2)/2.
!
         RLOG   = T_RANDD( -6.D0, 14.D0, SEED1 )
         T1     = 10**RLOG * 1.D0
         T2     = 10**RLOG * 3.D0
         TEXP   = 10**RLOG * 2.D0

         TXT = 'T1 = +/- #1, T2 = +/-#2, random test'

         CALL REPMD ( TXT, '#1', T1, 12, TXT )
         CALL REPMD ( TXT, '#2', T2, 12, TXT )

         CALL TCASE ( TXT )

!
!        T1 < T2.
!
         CALL GFREFN ( T1, T2, S1, S2, T )
         CALL CHCKSD ( TXT, T, '~/',  TEXP, TIGHT, OK )

!
!        Negate the lot; T2 < T1
!
         CALL GFREFN ( -T1, -T2, S1, S2, T )
         CALL CHCKSD ( TXT, T, '~/',  -TEXP, TIGHT, OK )

      END DO


      CALL T_SUCCESS ( OK )

      RETURN
      END


