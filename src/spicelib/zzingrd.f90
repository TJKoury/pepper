!$Procedure  ZZINGRD  ( is a voxel inside the grid? )

       logical function zzingrd ( nvox, voxel )
       use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return true if voxel is inside the voxel grid. This routine
!     operates on integer voxel coordinates.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     VOXEL
!
! $ Declarations

      implicit              none

      integer(ip)           nvox  ( 3 )
      integer(ip)           voxel ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NVOX       I   Dimensions of voxel grid.
!     VOXEL      I   Coordinates of voxel in voxel grid units.
!
!     The function returns .TRUE. if the voxel is inside the grid.
!
! $ Detailed_Input
!
!     NVOX       Dimensions of voxel grid.
!
!     VOXEL      Coordinates of voxel in voxel grid units. The
!                coordinates are 1-based integers.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if the voxel is inside the grid.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports the SPICELIB routine XDDA.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     J.A. Bytof      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-FEB-2016 (NJB) (JAB)
!
!        Renamed routine.
!
!        Based on DSKLIB Version 1.0.0, 03-FEB-1999 (JAB)
!
!-&

! $ Index_Entries
!
!      voxel inside grid
!
!-&


      integer(ip)           i


      zzingrd = .false.

!
!     Determine if voxel is outside the voxel grid
!     in any direction.
!
      do i = 1, 3

         if (      ( voxel(i) < 1       ) &
              .or. ( voxel(i) > nvox(i) )  ) then

            return

         end if

      end do

      zzingrd = .true.

      return
      end
