!$Procedure      ZZINILNK ( Initialize an AB cell linked-list )

      subroutine zzinilnk ( maxp, maxc, ncell, pntrs, cells )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize an AB cell linked-list structure.
!
! $ Required_Reading
!
!     None
!
! $ Keywords
!
!     DSK
!     Utility
!
! $ Declarations

      implicit none

      integer(ip)           maxp
      integer(ip)           maxc
      integer(ip)           ncell
      integer(ip)           pntrs  ( * )
      integer(ip)           cells  ( 2, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MAXP       I   Length of A-value array.
!     MAXC       I   Number of cells.
!     NCELL      O   Number of cells in use; initialized to zero.
!     PNTRS      O   Initialized pointer array.
!     CELLS      O   Initialized cell array.
!
! $ Detailed_Input
!
!     MAXP       is the length of the pointer array PNTRS.
!
!     MAXC       is the length of the cell array CELLS. The array has
!                dimensions
!
!                   ( 2, MAXC )
!
!                Elements
!
!                   ( 1, * )
!
!                normally contain "A-values"; elements
!
!                   ( 2, * )
!
!                normally contain pointers.
!
!
! $ Detailed_Output
!
!     NCELL      is the number of cells in use; this argument is
!                initialized to zero. This is done as a convenience
!                for the calling routine, which usually must keep
!                track of the number of cells in use.
!
!     PNTRS      is the pointer array, initialized so that each
!                element contains -1, which represents a null
!                pointer.
!
!     CELLS      is a cell array, initialized so that each pointer
!                element contains -1, and so that each value
!                element contains 0.
!
! $ Parameters
!
!     See the DSK type 2 include file
!
!        dsk02.inc
!
! $ Exceptions
!
!     1)  If the pointer array size MAXP is non-positive, the
!         error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2)  If the cell array size MAXC is non-positive, the
!         error SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports creation of DSK type 2 segments.
!     It is used for creation of both voxel-plate association
!     data structures and of vertex-plate association data
!     structures.
!
! $ Examples
!
!     See usage in ZZMKSPIN.
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
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        05-JAN-2016 (NJB)
!
!           Added error checks on input dimension arguments. Re-wrote
!           some of the argument descriptions. Added private routine
!           warning to header abstract.
!
!        08-OCT-2009 (NJB)
!
!           Re-ordered header sections.
!
!        03-FEB-1999 (JAB)
!
!-&

! $ Index_Entries
!
!     initialize an AB cell linked-list
!
!-&

      integer(ip)           i
      logical               return

      if ( return () ) then
         return
      end if

      call chkin ( 'ZZINILNK' )

      if ( maxp < 1 ) then

         call setmsg ( 'Pointer array size MAXP = #; size must ' &
         //            'be positive.'                           )
         call errint ( '#',  maxp                               )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         call chkout ( 'ZZINILNK'                               )
         return

      end if

      if ( maxc < maxp ) then

         call setmsg ( 'Cell array size MAXC = #; size must ' &
         //            'be at least as large as pointer array ' &
         //            'size #.'                                )
         call errint ( '#',  maxc                               )
         call errint ( '#',  maxp                               )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         call chkout ( 'ZZINILNK'                               )
         return

      end if

!
!     Initialize pointer array and cells.
!
      do i = 1, maxp
         pntrs(i)   = -1
      end do

      do i = 1, maxc
         cells(1,i) =  0
         cells(2,i) = -1
      end do

!
!     Set count of cells in use to 0, for the convenience
!     of the calling routine.
!
      ncell = 0

      call chkout ( 'ZZINILNK' )
      return
      end
