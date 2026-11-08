!$Procedure      CSTART ( Clean Start )

      SUBROUTINE CSTART
      IMPLICIT NONE

! $ Abstract
!
!     Clear the kernel pool and unload all SPICE binary kernels
!     so that the SPICE subsystems are in a "clean" state.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

!     None.

! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
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
! $ Particulars
!
!     This routine clears the SPICE kernel pool and unloads all
!     SPK, CK, binary PCK, an EK files.  In addition it closes any
!     other open DAFs or DASs.
!
!     Finally, the entry point KFILES of FILREG is called so that
!     all test files created by the user's test program will
!     automatically be removed from the user's disk.
!
! $ Examples
!
!     This routine is primarily intended as a utility for use of
!     the Test Utilities routine TOPEN.  See TOPEN for an example
!     of usage of this routine.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TESTUTIL Version 1.2.0, 26-AUG-2002 (FST)
!
!-    TESTUTIL Version 1.1.0, 14-DEC-2001 (FST) (NJB)
!
!        Increased MAXHND to 1000 to accomodate the changes to the
!        DAF system as a result of the handle manager integration.
!
!        Two incorrect calls to DAFHOF were replaced with the
!        intended calls to DASHOF.
!
!-    TESTUTIL Version 1.0.0, 27-JUL-1999 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Prepare all systems for a clean start
!
!-&


!
!     Spicelib Functions
!
      INTEGER               CARDI

!
!     Local Variables
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXHND
      PARAMETER           ( MAXHND = 1000 )

      INTEGER               HANDLS  ( LBCELL : MAXHND )
      INTEGER               I
      INTEGER               N
      INTEGER               TRIES



!
!     First, clear the kernel pool
!
      CALL CLPOOL

!
!     Clear the built-in body list.
!
      CALL ZZBODRST

!
!     Next unload every SPK, CK, and binary PCK file.
!
      CALL SSIZEI ( MAXHND, HANDLS )
      CALL DAFHOF ( HANDLS )
      N = CARDI   ( HANDLS )

      DO I = 1, N
         CALL SPKUEF ( HANDLS(I) )
         CALL CKUPF  ( HANDLS(I) )
         CALL PCKUOF ( HANDLS(I) )
      END DO
!
!     We have probably now closed every DAF, but just in case, we
!     get the remaining open handles and continue closing them until
!     all DAFs are closed.
!
      CALL SSIZEI ( MAXHND, HANDLS )
      CALL DAFHOF ( HANDLS )
      N = CARDI   ( HANDLS )

      TRIES = 0

      DO WHILE ( N .GT. 0 .AND. TRIES .LT. 100 )

         DO I = 1, N
            CALL DAFCLS ( HANDLS(I) )
         END DO

         CALL SSIZEI ( MAXHND, HANDLS )
         CALL DAFHOF ( HANDLS )
         N = CARDI   ( HANDLS )
         TRIES = TRIES + 1

      END DO


!
!     Finally, unload any EK's or DAS's
!
      CALL DASHOF ( HANDLS )
      N = CARDI   ( HANDLS )

      DO I = 1, N
         CALL EKUEF  ( HANDLS(I) )
      END DO
!
!     We have probably now closed every DAS, but just in case, we
!     get the remaining open HANDLS(I)s and continue closing them until
!     all DAS files are closed.  But first, unload the scratch file
!     used by the EK scratch area system.
!
      CALL ZZEKSCLN

!
!     Ok, now clean up the other DAS files.
!
      TRIES = 0
      CALL SSIZEI ( MAXHND, HANDLS )
      CALL DASHOF ( HANDLS )
      N =  CARDI  ( HANDLS )

      DO WHILE ( N .GT. 0 .AND. TRIES .LT. 100 )

         DO I = 1, N
            CALL DASCLS ( HANDLS(I) )
         END DO

         TRIES = TRIES + 1
         CALL SSIZEI ( MAXHND, HANDLS )
         CALL DASHOF ( HANDLS )
         N = CARDI   ( HANDLS )

      END DO

!
!     Wipe out all test files.
!
      CALL KFILES ()

      RETURN
      END
