!$Procedure      TSTVRS ( Test Utility version strings )

      SUBROUTINE TSTVRS (  VERSTR )
      use pepper_legacy

! $ Abstract
!
!     Get the current Test Utility Version Number
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

      CHARACTER*(*)         VERSTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VERSTR     O   Version string.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     VERSTR     is the latest version string.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error Free
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
!     Suppose you want to find out the recent version line for a given
!     routine in SPICELIB, SPKEZ. Using the code fragment below:
!
!     CHARACTER*(80)        VERSN
!
!     CALL TSTVRS ( VERSN )
!
!
! $ Restrictions
!
!     Do not call this routine! At this time, it is for use exclusively
!     by NAIF for a version utility program.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 2-JAN-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     Return version strings
!-&



!
!     At the current time only the TOOLKIT version number is specified.
!
      VERSTR = 'TST001'

      RETURN
      END
