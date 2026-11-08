! $ Abstract
!
!     This file contains the number of inertial reference
!     frames that are currently known by the SPICE toolkit
!     software.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      integer               ninert
      parameter           ( ninert = 21 )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NINERT     P   Number of known inertial reference frames.
!
! $ Parameters
!
!     NINERT     is the number of recognized inertial reference
!                frames.  This value is needed by both CHGIRF
!                ZZFDAT, and FRAMEX.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT)
!
!-&
