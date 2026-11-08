!$Procedure ZZTOGRID ( Model coordinates to voxel grid coordinates )

       subroutine zztogrid ( modxyz, voxori, voxsiz, grdxyz )
       use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Shift and scale plate model coordinates to zero-based, double
!     precision voxel grid coordinates.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DSK
!     PLATE
!     TOPOGRAPHY
!     VOXEL
!
! $ Declarations

      implicit none

      real(wp)              voxsiz
      real(wp)              modxyz  ( 3 )
      real(wp)              voxori  ( 3 )
      real(wp)              grdxyz  ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MODXYZ     I   Coordinates of point in model coordinates.
!     VOXORI     I   Origin of voxel grid in model coordinates.
!     VOXSIZ     I   Voxel size in model coordinates.
!     GRDXYZ     O   Coordinates of point in voxel grid coordinates.
!
! $ Detailed_Input
!
!     MODXYZ     Coordinates of point in model coordinates. The
!                point is expressed as an offset from the center
!                of a body-fixed reference frame associated with
!                a target body. Units are km.
!
!
!     VOXORI     Origin of voxel grid in model coordinates. VOXORI
!                is expressed in the same reference frame as MODXYZ.
!                Units are km.
!
!
!     VOXSIZ     Voxel size in model coordinates. VOXSIZ is the
!                voxel's edge length. Units are km.
!
!
! $ Detailed_Output
!
!     GRDXYZ     Coordinates of the input point, scaled to voxel grid
!                coordinates, and expressed as an offset from the voxel
!                grid origin. These coordinates are zero-based, double
!                precision values. The units are the voxel edge length
!                multiplied by km.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If VOXSIZ is less than or equal to zero, the error
!         SPICE(NONPOSITIVEVALUE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports DSKX02.
!
! $ Examples
!
!   C
!   C     Some position in model coordinates, i.e. the body fixed
!   C     units and frame coordinates.
!   C
!         DOUBLE PRECISION       POS(3)
!
!               ...
!
!   C
!   C     Retrieve the voxel grid geometry description.
!   C
!         CALL VOXDIM ( NVOX, VOXSIZ, VOXORI)
!
!   C
!   C     Convert the coordinate POS to voxel grid coordinates.
!   C
!         CALL ZZTOGRID ( POS, VOXORI, VOXSIZ, POSVOX )
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
!-    SPICELIB Version 1.0.0, 18-MAY-2016 (NJB)
!
!        Added error check for non-positive voxel size.
!
!        02-FEB-2016 (NJB) (JAB)
!
!           Renamed routine to ZZTOGRID.
!
!        08-OCT-2009 (NJB)
!
!           Updated header.
!
!        19-OCT-2004 (EDW)
!
!           Added Examples section, edits to comments.
!
!        03-FEB-1999 (JAB)
!
!           Original version.
!
!-&

! $ Index_Entries
!
!     scale plate model coordinates to voxel grid coordinates
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           i

!
!     Use discovery check-in.
!
      if ( return() ) return

      if ( voxsiz <= 0.0_wp ) then

         call setmsg ( 'Voxel size was #; must be positive.' )
         call errdp  ( '#',  voxsiz                          )
         call sigerr ( 'SPICE(NONPOSITIVEVALUE)'             )
         return

      end if

!
!     Convert model coordinates to voxel grid coordinates
!     via a Galilean transform.
!
      do i = 1, 3
         grdxyz(i) = (modxyz(i) - voxori(i)) / voxsiz
      end do

      return
      end
