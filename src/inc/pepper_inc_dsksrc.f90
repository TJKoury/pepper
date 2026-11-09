!> The parameters of SPICELIB's include file dsksrc.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_dsksrc

   implicit none

   public


!
!     File: dsksrc.inc
!
!
!     Version 1.0.0 19-FEB-2016 (NJB)

!
!     This file declares sizes of certain output array arguments
!     returned by DSKXSI. These arrays contain information on the
!     source of surface data on which an intercept was found.
!
!     See the include files
!
!        dla.inc
!        dskdsc.inc
!
!     for the declarations of DLA and DSK descriptor sizes,
!     respectively.
!
!     Caution: the sizes declared here may be increased
!     in a future SPICE Toolkit version. These sizes
!     are correct for the N0066 Toolkit.
!
!

!
!     Size of double precision source information component:
!
   integer               dcsize
   parameter           ( dcsize = 1 )

!     Size of integer source information component:
!
   integer               icsize
   parameter           ( icsize = 1 )

!
!     The contents of the components are DSK data type-dependent.
!
!         Type 2
!         ======
!
!                    Integer component        Double Precision component
!
!         Element    Contents
!         -------    ---------------------------------------------------
!            1       Plate ID                 Unused
!
!
!
!     End of include file dsksrc.inc
!

end module pepper_inc_dsksrc
