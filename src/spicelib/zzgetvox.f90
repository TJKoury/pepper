!$Procedure ZZGETVOX ( Coordinates of voxel containing a point )

      subroutine zzgetvox ( voxsiz, voxori, nvox, xyz, inbox, voxcor )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This subroutine returns the 1-based voxel coordinates of the
!     voxel that contains a given point.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PLATE
!
! $ Declarations

      implicit none

      real(wp)              voxsiz
      real(wp)              voxori    ( 3 )
      integer(ip)           nvox      ( 3 )
      real(wp)              xyz       ( 3 )
      logical               inbox
      integer(ip)           voxcor     ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VOXSIZ     I   Voxel size in model units.
!     VOXORI     I   3-Vector location of the voxel grid origin
!                    in model units.
!     NVOX       I   Number of voxel in each dimension.
!     XYZ        I   Model grid coordinates of point.
!     INBOX      O   Logical value is true if point is inside voxel
!                    grid, false if not.
!     VOXCOR     O   3-Index of voxel location with the voxel grid.
!
! $ Detailed_Input
!
!     VOXSIZ     is a double precision scalar value giving the size of
!                each cubical voxel in model units.
!
!     VOXORI     is a double precision 3-vector indicating the location
!                of the voxel grid origin in model units.
!
!     NVOX       is an integer 3-vector giving the length of each
!                side of the voxel grid in voxel units.
!
!     XYZ        is a double precision 3-vector containing the location
!                of the point in question in model units.
!
! $ Detailed_Output
!
!     INBOX      is a logical scalar indicating whether or not the
!                point XYZ lies within the extent of the voxel grid.
!
!     VOXCOR     is an integer 3-vector containing the x, y, and z
!                voxel indices of the voxel containing the point.
!
!                VOXCOR is valid only if INBOX is .TRUE.
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
!     This is not a general coordinate conversion routine. This routine
!     returns valid voxel coordinates only for voxels inside the voxel
!     grid.
!
!     Points on the outer surface of the grid, as defined by the voxel
!     origin and the grid dimensions, are considered to be inside the
!     grid.
!
!     Points outside of the grid are not mapped to voxel coordinates.
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
!   C     Convert the coordinate POS to voxel grid indices.
!   C
!         CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, POS, INBOX, VOXCOR )
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
!     E.D. Wright     (JPL
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 1.0.0, 18-MAR-2016 (NJB)
!
!        Added error check for non-positive voxel size.
!        Made small changes to header.
!
!        17-JAN-2016 (NJB)
!
!           Functional change: routine returns valid voxel coordinates
!           only for voxels inside the voxel grid. Points on the outer
!           surface of the grid are considered to be inside the grid.
!
!           Changed check-in to discovery style.
!
!           Corrected rounding for voxels on outer edge of grid.
!           Updated header.
!
!        08-OCT-2009 (NJB)
!
!           Re-ordered header sections.
!
!        14-SEP-2004 (EDW)
!
!           Corrected VOXID(I) functions to allow for
!           negative voxel coordinates. Original functions
!           returned values GE 1.
!
!           Added Examples section.
!
!        28-JAN-1999 (JAB)
!
!           Original version.
!
!-&

! $ Index_Entries
!
!     returns the voxel grid indices containing a given point
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables.
!
      real(wp)               term
      integer(ip)            i

      if ( return() ) return

      if ( voxsiz <= 0.0_wp ) then

         call setmsg ( 'Voxel size was #; must be positive.' )
         call errdp  ( '#',  voxsiz                          )
         call sigerr ( 'SPICE(NONPOSITIVEVALUE)'             )
         return

      end if

!
!     Initialize 'point in box' flag and voxel coordinates. The
!     coordinates are assigned out-of-range values.
!
      inbox     = .false.

      voxcor(1) = 0
      voxcor(2) = 0
      voxcor(3) = 0

!
!     Scale the point's coordinates to voxel grid space
!     and determine the indices of the voxel that contains it.
!
      do i = 1, 3
!
!        A Galilean transform. Calculate the voxel coordinate
!        corresponding to the body centered coordinate. This
!        operation performs the same task as TOGRID, but
!        including the operation here improves ZZGETVOX's
!        runtime performance.
!
         term = ( xyz(i) - voxori(i) ) / voxsiz

!
!        Calculate the voxel index for each degree of freedom
!        corresponding to the voxel coordinate.
!
!        If the point is outside of the grid, return now.
!
         if ( ( term < 0.0_wp ) .or. ( term > nvox(i) ) ) return

!
!        Assign a 1-based value to the Ith component of the voxel's
!        coordinates. The outer surface of the grid is considered part
!        of the grid.
!
!        Note that TERM is non-negative at this point.
!
         if ( int(term) < nvox(i) ) then

            voxcor(i) =  1 + int(term)

         else
!
!           TERM is NVOX(I), since the cases
!
!              TERM > NVOX(I)
!              TERM < NVOX(I)
!
!           have been ruled out.
!
            voxcor(i) = nvox(i)

         end if

      end do

      inbox = .true.

      return
      end
