!> The parameters of SPICELIB's include file spk19.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_spk19

   implicit none

   public

! $ Abstract
!
!     Declare parameters specific to SPK type 19.
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
!     B.V. Semenov      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
!
!        Updated to support subtype 2.
!
!-    SPICELIB Version 1.0.0, 07-MAR-2014 (NJB) (BVS)
!
!-&


!
!     Maximum polynomial degree supported by the current
!     implementation of this SPK type.
!
!     The degree is compatible with the maximum degrees
!     supported by types 13 and 21.
!
   integer               maxdeg
   parameter           ( maxdeg = 27 )

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
!     SPK type 19 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 12-element packets.
!
   integer               s19tp0
   parameter           ( s19tp0 = 0 )

!
!     Subtype 1:  Lagrange interpolation, 6-element packets.
!
   integer               s19tp1
   parameter           ( s19tp1 = s19tp0 + 1 )

!
!     Subtype 2:  Hermite interpolation, 6-element packets.
!
   integer               s19tp2
   parameter           ( s19tp2 = s19tp1 + 1 )


!
!     Packet sizes associated with the various subtypes:
!
   integer               s19ps0
   parameter           ( s19ps0 = 12 )

   integer               s19ps1
   parameter           ( s19ps1 =  6 )

   integer               s19ps2
   parameter           ( s19ps2 =  6 )

!
!     Number of subtypes:
!
   integer               s19nst
   parameter           ( s19nst = 3 )

!
!     Maximum packet size for type 19:
!
   integer               s19mxz
   parameter           ( s19mxz = s19ps0 )

!
!     Minimum packet size for type 19:
!
   integer               s19mnz
   parameter           ( s19mnz = s19ps1 )

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
   integer               maxrsz
   parameter           ( maxrsz =  2 + (maxdeg+1)*(s19ps1+1) )

!
!     End of include file spk19.inc.
!

end module pepper_inc_spk19
