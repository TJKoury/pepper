!> Declare parameters specific to CK type 06.
!>
!>## Restrictions
!>
!>     None.
!>
!>## Author and Institution
!>
!>     N.J. Bachman      (JPL)
!>     B.V. Semenov      (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, 10-MAR-2014 (NJB) (BVS)
module pepper_inc_ck06

   use pepper_kinds, only: ip

   implicit none

   public

!
!     Maximum polynomial degree supported by the current
!     implementation of this CK type.
!
   integer(ip), parameter :: maxdeg = 23

!
!     Integer code indicating `true':
!
   integer(ip), parameter :: itrue = 1

!
!     Integer code indicating `false':
!
   integer(ip), parameter :: ifalse = -1

!
!     CK type 6 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion
!                 and quaternion derivatives only, no angular velocity
!                 vector provided. Quaternion elements are listed
!                 first, followed by derivatives. Angular velocity is
!                 derived from the quaternions and quaternion
!                 derivatives.
!
   integer(ip), parameter :: c06tp0 = 0

!
!     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion
!                 only. Angular velocity is derived by differentiating
!                 the interpolating polynomials.
!
   integer(ip), parameter :: c06tp1 = c06tp0 + 1

!
!     Subtype 2:  Hermite interpolation, 14-element packets.
!                 Quaternion and angular angular velocity vector, as
!                 well as derivatives of each, are provided. The
!                 quaternion comes first, then quaternion derivatives,
!                 then angular velocity and its derivatives.
!
   integer(ip), parameter :: c06tp2 = c06tp1 + 1

!
!     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion
!                 and angular velocity vector provided.  The quaternion
!                 comes first.
!
   integer(ip), parameter :: c06tp3 = c06tp2 + 1

!
!     Number of subtypes:
!
   integer(ip), parameter :: c06nst = 4

!
!     Packet sizes associated with the various subtypes:
!
   integer(ip), parameter :: c06ps0 = 8

   integer(ip), parameter :: c06ps1 = 4

   integer(ip), parameter :: c06ps2 = 14

   integer(ip), parameter :: c06ps3 = 7

!
!     Maximum packet size for type 6:
!
   integer(ip), parameter :: c06mxz = c06ps2

!
!     Minimum packet size for type 6:
!
   integer(ip), parameter :: c06mnz = c06ps1

!
!     The CKPFS record size declared in ckparam.inc must be at least as
!     large as the maximum possible size of a CK type 6 record.
!
!     The largest possible CK type 6 record has subtype 3 (note that
!     records of subtype 2 have half as many epochs as those of subtype
!     3, for a given polynomial degree). A subtype 3 record contains
!
!        - The evaluation epoch
!        - The subtype and packet count
!        - MAXDEG+1 packets of size C06PS3
!        - MAXDEG+1 time tags
!

   integer(ip), parameter :: maxrsz = 4 + (maxdeg+1)*(c06ps3+1)

end module pepper_inc_ck06
