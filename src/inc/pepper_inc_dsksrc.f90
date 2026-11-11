!> This file declares sizes of certain output array arguments returned by DSKXSI. These arrays contain information on the source of surface data on which an intercept was found.
!>
!>     See the include files
!>
!>     for the declarations of DLA and DSK descriptor sizes,
!>     respectively.
!>
!>     Caution: the sizes declared here may be increased
!>     in a future SPICE Toolkit version. These sizes
!>     are correct for the N0066 Toolkit.
!>
!>## Version
!>
!>     Version 1.0.0 19-FEB-2016 (NJB)
!>
!>        dla.inc
!>        dskdsc.inc
module pepper_inc_dsksrc

   use pepper_kinds, only: ip

   implicit none

   public

!     Size of double precision source information component:
!
   integer(ip), parameter :: dcsize = 1

!     Size of integer source information component:
!
   integer(ip), parameter :: icsize = 1

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

end module pepper_inc_dsksrc
