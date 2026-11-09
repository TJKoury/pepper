!> The parameters of SPICELIB's include file spkrec.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_spkrec

   implicit none

   public

! $ Abstract
!
!     Declare SPK data record size.  This record is declared in
!     SPKPVN and is passed to SPK reader (SPKRxx) and evaluator
!     (SPKExx) routines.
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
!     1) If new SPK types are added, it may be necessary to
!        increase the size of this record.  The header of SPKPVN
!        should be updated as well to show the record size
!        requirement for each data type.
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
!-    SPICELIB Version 2.0.0, 05-OCT-2012 (NJB)
!
!        Updated to support increase of maximum degree to 27 for types
!        2, 3, 8, 9, 12, 13, 18, and 19. See SPKPVN for a list
!        of record size requirements as a function of data type.
!
!-    SPICELIB Version 1.0.0, 16-AUG-2002 (NJB)
!
!-&


   integer               maxrec
   parameter           ( maxrec = 198 )


!
!     End include file spkrec.inc
!

end module pepper_inc_spkrec
