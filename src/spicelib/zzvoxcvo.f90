!$Procedure  ZZVOXCVO ( Voxel to coarse voxel offset )

      subroutine zzvoxcvo ( vixyz, nvox, cgrscl, cgxyz, cgoff, cgof1d )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Map voxel coordinates to coarse voxel coordinates and offset
!     relative to coarse voxel. Offset coordinates are 1-based.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     PRIVATE
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           vixyz  ( 3 )
      integer(ip)           nvox   ( 3 )
      integer(ip)           cgrscl
      integer(ip)           cgxyz  ( 3 )
      integer(ip)           cgoff  ( 3 )
      integer(ip)           cgof1d

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VIXYX      I   Fine voxel integer coordinates.
!     NVOX       I   Fine voxel grid dimensions.
!     CGRSCL     I   Coarse voxel scale.
!     CGXYZ      O   Coarse voxel coordinates.
!     CGOFF      O   3-D voxel offset relative to coarse voxel.
!     CGOF1D     O   1-D voxel offset relative to coarse voxel.
!
! $ Detailed_Input
!
!     VIXYZ      is an array containing integer Cartesian coordinates
!                of a voxel in the fine voxel grid. Coordinates are
!                1-based.
!
!     NVOX       is an array containing the integer extents of the fine
!                voxel grid. Units are voxels.
!
!     CGRSCL     is the coarse voxel scale. This is the integer ratio
!                of the coarse voxel edge length to the fine voxel
!                edge length.
!
! $ Detailed_Output
!
!     CGXYZ      is an array containing integer Cartesian coordinates
!                of the coarse voxel that contains the fine voxel
!                indexed by VIXYZ. Coordinates are 1-based.
!
!     CGOFF      is an array containing the integer Cartesian
!                coordinates of the fine voxel relative to the coarse
!                voxel that contains it. Coordinates are 1-based.
!
!     CGOF1D     is the 1-dimensional offset of the input fine voxel
!                relative to the coarse voxel that contains it. The
!                offset is 1-based.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If any input fine voxel grid dimension is non-positive,
!        the error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2) If the coarse voxel scale is non-positive, the error
!        SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine encapsulates a frequently used type 2 DSK address
!     calculation. The output offset CGOF1D is useful for locating
!     a fine voxel's pointer into the voxel-plate list.
!
! $ Examples
!
!     See usage in the routine ZZMKSPIN.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 23-AUG-2016 (NJB)
!
!        $Exceptions section was updated.
!
!     Based on DSKLIB Version 1.0.0, 20-MAR-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     map voxel to coarse voxel offset
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


      if ( return() ) then
         return
      end if

      do i = 1, 3

         if ( nvox(i) .lt. 1 ) then

            call chkin  ( 'ZZVOXCVO' )

            call setmsg ( 'Voxel grid dimensions must be positive ' &
            //            'but were # # #.'                        )
            call errint ( '#', nvox(1)                             )
            call errint ( '#', nvox(2)                             )
            call errint ( '#', nvox(3)                             )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )

            call chkout ( 'ZZVOXCVO' )
            return

         end if

      end do


      do i = 1, 3

         if ( ( vixyz(i) .lt. 1 ) .or. ( vixyz(i) .gt. nvox(i) ) ) then

            call chkin  ( 'ZZVOXCVO' )

            call setmsg ( 'Voxel grid coordinates must be inside ' &
            //            'grid having dimensions # x # x # ' &
            //            'but were # # #.'                        )
            call errint ( '#', nvox(1)                             )
            call errint ( '#', nvox(2)                             )
            call errint ( '#', nvox(3)                             )
            call errint ( '#', vixyz(1)                            )
            call errint ( '#', vixyz(2)                            )
            call errint ( '#', vixyz(3)                            )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )

            call chkout ( 'ZZVOXCVO' )
            return

         end if

      end do


      if ( cgrscl .lt. 1 ) then

         call chkin  ( 'ZZVOXCVO' )

         call setmsg ( 'Coarse voxel grid scale must be positive ' &
         //            'but was #.'                               )
         call errint ( '#', nvox(1)                               )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                   )

         call chkout ( 'ZZVOXCVO' )
         return

      end if

      do i = 1, 3
!
!        Set the Ith coarse grid coordinate. Recall these coordinates
!        are 1-based.
!
         cgxyz(i) = ( ( vixyz(i) - 1 ) /  cgrscl  )  +  1

!
!        Set the Ith coarse grid coordinate offset. These offsets
!        are 1-based as well.
!
         cgoff(i) =  vixyz(i)  -  cgrscl*( cgxyz(i) - 1 )

      end do

!
!     Convert the coarse grid-relative offset to a relative
!     ID. The ID is a one-dimensional offset.
!
      cgof1d =    (cgoff(3)-1) * cgrscl * cgrscl &
                + (cgoff(2)-1) * cgrscl &
                +  cgoff(1)

      end
