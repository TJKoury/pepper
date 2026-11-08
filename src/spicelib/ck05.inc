! $ Abstract
!
!     Declare parameters specific to CK type 05.
!
! $ Required_Reading
!
!     CK
!
! $ Keywords
!
!     CK
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
!-    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB)
!
!-&

!
!     CK type 5 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion
!                 and quaternion derivatives only, no angular velocity
!                 vector provided. Quaternion elements are listed
!                 first, followed by derivatives. Angular velocity is
!                 derived from the quaternions and quaternion
!                 derivatives.
!
      INTEGER               C05TP0
      PARAMETER           ( C05TP0 = 0 )


!
!     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion
!                 only. Angular velocity is derived by differentiating
!                 the interpolating polynomials.
!
      INTEGER               C05TP1
      PARAMETER           ( C05TP1 = C05TP0 + 1 )


!
!     Subtype 2:  Hermite interpolation, 14-element packets.
!                 Quaternion and angular angular velocity vector, as
!                 well as derivatives of each, are provided. The
!                 quaternion comes first, then quaternion derivatives,
!                 then angular velocity and its derivatives.
!
      INTEGER               C05TP2
      PARAMETER           ( C05TP2 = C05TP1 + 1 )

!
!     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion
!                 and angular velocity vector provided.  The quaternion
!                 comes first.
!
      INTEGER               C05TP3
      PARAMETER           ( C05TP3 = C05TP2 + 1 )

!
!     Packet sizes associated with the various subtypes:
!
      INTEGER               C05PS0
      PARAMETER           ( C05PS0 = 8 )

      INTEGER               C05PS1
      PARAMETER           ( C05PS1 = 4 )

      INTEGER               C05PS2
      PARAMETER           ( C05PS2 = 14 )

      INTEGER               C05PS3
      PARAMETER           ( C05PS3 = 7 )

!
!     End of file ck05.inc.
!
