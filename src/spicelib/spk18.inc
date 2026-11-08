! $ Abstract
!
!     Declare parameters specific to SPK type 18.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     SPK
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB)
!
!-&

!
!     SPK type 18 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 12-element packets, order
!                 reduction at boundaries to preceding number
!                 equivalent to 3 mod 4.
!
      INTEGER               S18TP0
      PARAMETER           ( S18TP0 = 0 )

!
!     Subtype 1:  Lagrange interpolation, 6-element packets, order
!                 reduction at boundaries to preceding odd number.
!
      INTEGER               S18TP1
      PARAMETER           ( S18TP1 = S18TP0 + 1 )

!
!     Packet sizes associated with the various subtypes:
!
      INTEGER               S18PS0
      PARAMETER           ( S18PS0 = 12 )

      INTEGER               S18PS1
      PARAMETER           ( S18PS1 =  6 )

!
!     End of include file spk18.inc.
!
