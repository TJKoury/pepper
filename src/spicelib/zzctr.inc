! $ Abstract
!
!     This include file defines the dimension of the counter
!     array used by various SPICE subsystems to uniquely identify
!     changes in their states.
!
! $ Parameters
!
!     CTRSIZ      is the dimension of the counter array used by
!                 various SPICE subsystems to uniquely identify
!                 changes in their states.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!
!-&

      integer               ctrsiz
      parameter           ( ctrsiz = 2 )

!
!     End of include file.
!
