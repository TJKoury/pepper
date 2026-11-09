!> The parameters of SPICELIB's include file spk21.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_spk21

   implicit none

   public


! $ Abstract
!
!     Declare parameters specific to SPK type 21.
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
!-    SPICELIB Version 1.0.0, 25-DEC-2013 (NJB)
!
!-&

!
!     MAXTRM      is the maximum number of terms allowed in each
!                 component of the difference table contained in a type
!                 21 SPK difference line. MAXTRM replaces the fixed
!                 table parameter value of 15 used in SPK type 1
!                 segments.
!
!                 Type 21 segments have variable size. Let MAXDIM be
!                 the dimension of each component of the difference
!                 table within each difference line. Then the size
!                 DLSIZE of the difference line is
!
!                    ( 4 * MAXDIM ) + 11
!
!                 MAXTRM is the largest allowed value of MAXDIM.
!
!
   integer               maxtrm
   parameter           ( maxtrm = 25 )

!
!     End of include file spk21.inc.
!

end module pepper_inc_spk21
