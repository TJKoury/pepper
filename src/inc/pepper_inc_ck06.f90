!> The parameters of SPICELIB's include file ck06.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ck06

   implicit none

   public

! $ Abstract
!
!     Declare parameters specific to CK type 06.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!     B.V. Semenov      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 10-MAR-2014 (NJB) (BVS)
!

!
!     Maximum polynomial degree supported by the current
!     implementation of this CK type.
!
   integer               maxdeg
   parameter           ( maxdeg = 23 )

!
!     Integer code indicating `true':
!
   integer               itrue
   parameter           ( itrue  =  1 )

!
!     Integer code indicating `false':
!
   integer               ifalse
   parameter           ( ifalse = -1 )

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
   integer               c06tp0
   parameter           ( c06tp0 = 0 )

!
!     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion
!                 only. Angular velocity is derived by differentiating
!                 the interpolating polynomials.
!
   integer               c06tp1
   parameter           ( c06tp1 = c06tp0 + 1 )

!
!     Subtype 2:  Hermite interpolation, 14-element packets.
!                 Quaternion and angular angular velocity vector, as
!                 well as derivatives of each, are provided. The
!                 quaternion comes first, then quaternion derivatives,
!                 then angular velocity and its derivatives.
!
   integer               c06tp2
   parameter           ( c06tp2 = c06tp1 + 1 )

!
!     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion
!                 and angular velocity vector provided.  The quaternion
!                 comes first.
!
   integer               c06tp3
   parameter           ( c06tp3 = c06tp2 + 1 )

!
!     Number of subtypes:
!
   integer               c06nst
   parameter           ( c06nst = 4 )

!
!     Packet sizes associated with the various subtypes:
!
   integer               c06ps0
   parameter           ( c06ps0 = 8 )

   integer               c06ps1
   parameter           ( c06ps1 = 4 )

   integer               c06ps2
   parameter           ( c06ps2 = 14 )

   integer               c06ps3
   parameter           ( c06ps3 = 7 )

!
!     Maximum packet size for type 6:
!
   integer               c06mxz
   parameter           ( c06mxz = c06ps2 )

!
!     Minimum packet size for type 6:
!
   integer               c06mnz
   parameter           ( c06mnz = c06ps1 )

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

   integer               maxrsz
   parameter           ( maxrsz =  4 + (maxdeg+1)*(c06ps3+1) )

!
!     End of file ck06.inc.
!

end module pepper_inc_ck06
