!> Declare parameters specific to SPK type 19.
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
!>-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
!>
!>        Updated to support subtype 2.
!>
!>-    SPICELIB Version 1.0.0, 07-MAR-2014 (NJB) (BVS)
module pepper_inc_spk19

   use pepper_kinds, only: ip

   implicit none

   public

!
!     Maximum polynomial degree supported by the current
!     implementation of this SPK type.
!
!     The degree is compatible with the maximum degrees
!     supported by types 13 and 21.
!
   integer(ip), parameter :: maxdeg = 27

!
!     Integer code indicating `true':
!
   integer(ip), parameter :: itrue = 1

!
!     Integer code indicating `false':
!
   integer(ip), parameter :: ifalse = -1

!
!     SPK type 19 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 12-element packets.
!
   integer(ip), parameter :: s19tp0 = 0

!
!     Subtype 1:  Lagrange interpolation, 6-element packets.
!
   integer(ip), parameter :: s19tp1 = s19tp0 + 1

!
!     Subtype 2:  Hermite interpolation, 6-element packets.
!
   integer(ip), parameter :: s19tp2 = s19tp1 + 1

!
!     Packet sizes associated with the various subtypes:
!
   integer(ip), parameter :: s19ps0 = 12

   integer(ip), parameter :: s19ps1 = 6

   integer(ip), parameter :: s19ps2 = 6

!
!     Number of subtypes:
!
   integer(ip), parameter :: s19nst = 3

!
!     Maximum packet size for type 19:
!
   integer(ip), parameter :: s19mxz = s19ps0

!
!     Minimum packet size for type 19:
!
   integer(ip), parameter :: s19mnz = s19ps1

!
!     The SPKPVN record size declared in spkrec.inc must be at least as
!     large as the maximum possible size of an SPK type 19 record.
!
!     The largest possible SPK type 19 record has subtype 1 (note that
!     records of subtype 0 have half as many epochs as those of subtype
!     1, for a given polynomial degree). A type 1 record contains
!
!        - The subtype and packet count
!        - MAXDEG+1 packets of size S19PS1
!        - MAXDEG+1 time tags
!
   integer(ip), parameter :: maxrsz = 2 + (maxdeg+1)*(s19ps1+1)

end module pepper_inc_spk19
