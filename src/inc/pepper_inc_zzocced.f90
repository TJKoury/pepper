!> The parameters of SPICELIB's include file zzocced.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_zzocced

   implicit none

   public

! $ Abstract
!
!     Declare ZZOCCED return code parameters, comparison strings
!     and other parameters.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     ELLIPSOID
!     GEOMETRY
!     GF
!     OCCULTATION
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
!-    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB)
!
!-&


!     The function returns an integer code indicating the geometric
!     relationship of the three bodies.
!
!     Codes and meanings are:
!
!        -3                    Total occultation of first target by
!                              second.
!
   integer               total1
   parameter           ( total1 = -3 )

!
!        -2                    Annular occultation of first target by
!                              second.  The second target does not
!                              block the limb of the first.
!
   integer               annlr1
   parameter           ( annlr1 = -2 )

!
!        -1                    Partial occultation of first target by
!                              second target.
!
   integer               partl1
   parameter           ( partl1 = -1 )

!
!         0                    No occultation or transit:  both objects
!                              are completely visible to the observer.
!
   integer               noocc
   parameter           ( noocc  =  0 )

!
!         1                    Partial occultation of second target by
!                              first target.
!
   integer               partl2
   parameter           ( partl2 =  1 )

!
!         2                    Annular occultation of second target by
!                              first.
!
   integer               annlr2
   parameter           ( annlr2 =  2 )

!
!         3                    Total occultation of second target by
!                              first.
!
   integer               total2
   parameter           ( total2 =  3 )

!
!     End include file zzocced.inc
!

end module pepper_inc_zzocced
