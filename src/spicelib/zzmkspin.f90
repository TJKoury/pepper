!$Procedure ZZMKSPIN ( Make spatial index of plates )

      subroutine zzmkspin ( np,     plates, vrtces, voxscl, cgscal, &
                            maxptr, mxcell, maxvxl, cells,  nvox, &
                            voxsiz, voxori, nvxtot, nvxptr, vxptr, &
                            nvxlst, vxlist, extent, cgrptr         )
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Create voxel grid data structure and voxel-plate mapping.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     plate voxel index
!
! $ Declarations

      include               'dsk02.inc'

      integer               np
      integer               plates ( 3, * )
      double precision      vrtces ( 3, * )
      double precision      voxscl
      integer               cgscal
      integer               maxptr
      integer               mxcell
      integer               maxvxl
      integer               cells   ( 2, mxcell )
      integer               nvox   ( 3 )
      double precision      voxsiz
      double precision      voxori ( 3 )
      integer               nvxtot
      integer               nvxptr
      integer               vxptr  ( * )
      integer               nvxlst
      integer               vxlist ( * )
      double precision      extent ( 6 )
      integer               cgrptr ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NP         I   Number of plates.
!     PLATES     I   Array of plates.
!     VRTCES     I   Array of vertices.
!     VOXSCL     I   Voxel scaling factor.
!     CGSCAL     I   Coarse voxel grid scaling factor.
!     MAXPTR     I   Maximum voxel pointer list size.
!     MXCELL     I   Cell array column dimension.
!     MAXVXL     I   Maximum voxel-plate list size.
!     CELLS     I-O  Workspace array for vertex list construction.
!     NVOX       O   Dimensions of voxel grid.
!     VOXSIZ     O   Size of a voxel in model units.
!     VOXORI     O   Origin of voxel grid in model units.
!     NVXTOT     O   Total number of voxels in grid.
!     NVXPTR     O   Number of pointers in VXPTR array.
!     VXPTR      O   Pointer index to the voxel to plates list.
!     NVXLST     O   Length of voxel to plates list.
!     VXLIST     O   Voxel to plates list.
!     EXTENT     O   Vertex coordinate extents in km.
!     CGRPTR     O   Coarse voxel grid pointer array.
!
! $ Detailed_Input
!
!     NP         Total number of plates in model.
!
!     PLATES     Array containing indices in the array VRTCES of
!                the vertices corresponding to each plate. The
!                elements (1:3,I) of PLATES are the vertex indices
!                of the Ith plate.
!
!     VRTCES     Array containing cartesian coordinates for all
!                vertices in the model reference frame. Elements
!                (1:3,I) of VRTCES are the coordinates of the Ith
!                vertex.
!
!     VOXSCL     The voxel size is determined by the average extent of
!                each plate in XYZ. VOXSCL is a scaling factor that
!                may be used to adjust final voxel size.
!
!     CGSCAL     The coarse voxel grid has voxel edge length
!                larger by factor CGSCAL than the fine voxel
!                edge length.
!
!     MAXPTR     is the maximum number of elements in the output
!                list of voxel pointers.
!
!     MXCELL     is the number of cells in the input cell array.
!                This is the second dimension of the array.
!
!     MAXVXL     is the maximum number of elements in the output
!                voxel-plate list.
!
!     CELLS      workspace array used to construct the voxel-plate
!                mapping.
!
! $ Detailed_Output
!
!     CELLS      workspace array used to construct the voxel-plate
!                mapping.
!
!     NVOX       Dimensions of the voxel grid in voxel units.
!
!     VOXSIZ     Size of each voxel in model units (km).
!
!     VOXORI     Origin of voxel grid in model units (km).
!
!     NVXTOT     Total number of voxels in grid.
!
!     NVXPTR     Number of pointers in VXPTR array.
!
!     VXPTR      Array of pointers that map voxels to their associated
!                plated IDs in the voxel to plates list. The Nth
!                element of this array contains a pointer to the Mth
!                element of VXLIST, or -1. If VXPTR(N) is -1, the Nth
!                voxel is empty.
!
!     NVXLST     The total number of elements in the plate list.
!
!     VXLIST     The plates list. For the Nth voxel, VXPTR(N) points to
!                VXLIST(M). VXLIST(M) contains the number plates that
!                intersect voxel N. The IDs of those plates are stored
!                in the following elements.
!
!     EXTENT     is an array of extents of vertex coordinates. Elements
!                indexed (2*I)-1 and 2*I are the minimum and maximum
!                values of the Ith coordinate, taken over all vertices.
!                Units are km.
!
!     CGRPTR     is an of array coarse voxel grid pointers. Null
!                pointers have the value -1. Non-null pointers indicate
!                locations in the voxel pointer array: each non-empty
!                coarse voxel points to a list of pointers for each of
!                the fine voxels it contains.
!
! $ Parameters
!
!     See the include file dsk02.inc.
!
! $ Exceptions
!
!     1)  If NP is less than 1 or greater than MAXPLT, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2)  If the number of coarse voxels exceeds the grid size MAXCGR,
!         the error SPICE(COARSEGRIDOVERFLOW) is signaled.
!
!     3)  If the voxel count is greater than MAXVOX, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     4)  If the coarse voxel count is less than 1 or greater than
!         MAXCGR, the error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     5)  If the coarse voxel scale is less than 1 or more than
!         the cube root of the fine voxel count, the error
!         SPICE(VALUEOUTOFRANGE) will be signaled.
!
!     6)  If the cube of the coarse voxel scale does not divide the
!         fine voxel count evenly, the error SPICE(INCOMPATIBLESCALE)
!         will be signaled.
!
!     7)  If the workspace overflows while this routine accumulates
!         voxel-plate associations, the error will be signaled by
!         a routine in the call tree of this routine.
!
!     8)  If the voxel-plate association list array overflows while
!         this routine accumulates voxel-plate associations, the error
!         will be signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports creation of spatial indexes for DSK type 2
!     segments. This routine determines the fine and coarse voxel
!     grid dimensions and parameters. It also builds the voxel-plate
!     association data structures.
!
! $ Examples
!
!     See usage in DSKMI2.
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
!     N.J. Bachman  (JPL)
!     J.A. Bytof    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 17-FEB-2017 (NJB)
!
!        Added new error checks.
!
!        29-JAN-2016 (NJB)
!
!           Removed reference to ZZVOXPAD. Renamed argument MAXCEL to
!           MXCELL to accommodate declaration of MAXCEL in dsk02.inc.
!
!           Updated long error messages and header comments.
!
!        26-MAR-2015 (NJB)
!
!           Functional change: now fills in the coarse voxel grid
!           pointer array with valid pointers; this is done in one
!           pass. The old version of the routine only marked the
!           elements of this array as null or non-null.
!
!           The algorithm for computing the set of voxels containing
!           the bounding box of each plate was re-written to improve
!           efficiency.
!
!        30-JUN-2014 (NJB)
!
!           Argument list change: removed work space array PNTRS. The
!           pointer array for the voxel-to-plate list mapping is now
!           constructed in place.
!
!           Changed argument list to include sizes of arrays. Changed
!           error handling to make use of array sizes. Changed call to
!           UNTNGL to accommodate argument list change in that routine.
!           Updated header I/O descriptions.
!
!        13-MAY-2010 (NJB)
!
!           Now accepts input workspace arrays PNTRS and CELLS.
!           Replaced argument VERT with arguments VRTCES and PLATES;
!           this was done to greatly reduce the memory needed by this
!           program.
!
!           Changed INCLUDE file to dsk02.inc.
!
!           Bug fix: determination of empty voxels is now based on
!           voxel pointers, not their target values.
!
!
!        08-OCT-2009 (NJB)
!
!           Re-ordered header sections.
!
!        12-SEP-2004 (EDW)
!
!           Improve/expand comments and descriptions.
!
!        03-FEB-1999 (JAB)
!
!           Original version.
!
!-&

! $ Index_Entries
!
!     spatial index plates voxels
!
!-&

! $ Revisions
!
!        20-MAR-2015 (NJB)
!
!        Functional change: now fills in the coarse voxel grid pointer
!        array with valid pointers; this is done in one pass. The old
!        version of the routine only marked the elements of this array
!        as null or non-null.
!
!        The maximum size of the voxel-plate pointer array has been
!        reduced to MAXVOX/2.
!
!        The array mapping voxels to the voxel-plate pointer array is
!        no longer used. Previously, voxel IDs were used as the
!        "A list" elements with which plate lists were associated.
!        Now, indices in the voxel-plate pointer array are used as
!        "A" values.
!
!-&

!
!     SPICELIB functions
!
      double precision      brcktd
      double precision      dpmax
      double precision      dpmin

      logical               failed
      logical               return

!
!     Other functions
!
      integer               zzvox2id

!
!     Local parameters
!

!
!     Fraction of voxel edge length used a tolerance for plate
!     inclusion in voxels:
!
      double precision      tol
      parameter           ( tol  = 1.d-3 )

!
!     Local Variables
!
      double precision      avext
      double precision      bxmax
      double precision      bxmin
      double precision      bymax
      double precision      bymin
      double precision      bzmax
      double precision      bzmin
      double precision      cvxsiz
      double precision      mdltol
      double precision      vmod   ( 3 )
      double precision      xextnt ( 6 )
      double precision      xmax
      double precision      xmin
      double precision      xp     ( 3 )
      double precision      xvmax
      double precision      xvmin
      double precision      ymax
      double precision      ymin
      double precision      yp     ( 3 )
      double precision      yvmax
      double precision      yvmin
      double precision      zmax
      double precision      zmin
      double precision      zp     ( 3 )
      double precision      zvmax
      double precision      zvmin

      integer               cgof1d
      integer               cgoff  ( 3 )
      integer               cgrdim ( 3 )
      integer               cgxyz  ( 3 )
      integer               cvid
      integer               gxmax
      integer               gxmin
      integer               gymax
      integer               gymin
      integer               gzmax
      integer               gzmin
      integer               i
      integer               ix
      integer               ixptr
      integer               iy
      integer               iz
      integer               j
      integer               ncell
      integer               ncgflg
      integer               npcg
      integer               nx
      integer               ny
      integer               nz
      integer               q
      integer               r
      integer               to
      integer               vixyz  ( 3 )
      integer               vcoord ( 3 )

      logical               inbox

!
!     Saved variables
!
!
!     Required for f2c use on Linux, all local variables
!     to static.
!
      save

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZMKSPIN' )

!
!     Check NP.
!
      if (  ( np .lt. 1 ) .or. ( np .gt. maxplt )  ) then

         call setmsg ( 'Plate count NP = #; count must ' &
         //            'be in the range 1:#.'            )
         call errint ( '#',  np                          )
         call errint ( '#',  maxplt                      )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'          )
         call chkout ( 'ZZMKSPIN'                        )
         return

      end if

!
!     Make sure the coarse voxel scale is positive. We'll
!     perform additional checks later on. Those checks
!     require computations that can't be done if the coarse
!     scale is zero.
!
      if ( cgscal .lt. 1 ) then

         call setmsg ( 'Coarse voxel scale = #; scale must ' &
         //            'be positive.'                       )
         call errint ( '#',  cgscal                         )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'             )
         call chkout ( 'ZZMKSPIN'                           )
         return

      end if

!
!     Get the average extents of all plates and the
!     overall model extent. The extents have model units; in
!     other words km.
!
      avext = 0.d0

      xmin =  dpmax()
      xmax =  dpmin()
      ymin =  dpmax()
      ymax =  dpmin()
      zmin =  dpmax()
      zmax =  dpmin()

      do i = 1, np

         bxmin = dpmax()
         bxmax = dpmin()
         bymin = dpmax()
         bymax = dpmin()
         bzmin = dpmax()
         bzmax = dpmin()

         xp(1) = vrtces( 1, plates(1,i) )
         xp(2) = vrtces( 1, plates(2,i) )
         xp(3) = vrtces( 1, plates(3,i) )

         yp(1) = vrtces( 2, plates(1,i) )
         yp(2) = vrtces( 2, plates(2,i) )
         yp(3) = vrtces( 2, plates(3,i) )

         zp(1) = vrtces( 3, plates(1,i) )
         zp(2) = vrtces( 3, plates(2,i) )
         zp(3) = vrtces( 3, plates(3,i) )

         do j = 1, 3
!
!           Determine plate extents.
!
            bxmin = dmin1( bxmin, xp(j) )
            bxmax = dmax1( bxmax, xp(j) )

            bymin = dmin1( bymin, yp(j) )
            bymax = dmax1( bymax, yp(j) )

            bzmin = dmin1( bzmin, zp(j) )
            bzmax = dmax1( bzmax, zp(j) )

!
!           Determine model extent.
!
            xmin = dmin1 ( xmin, bxmin )
            xmax = dmax1 ( xmax, bxmax )

            ymin = dmin1 ( ymin, bymin )
            ymax = dmax1 ( ymax, bymax )

            zmin = dmin1 ( zmin, bzmin )
            zmax = dmax1 ( zmax, bzmax )

         end do

         extent(1) = xmin
         extent(2) = xmax
         extent(3) = ymin
         extent(4) = ymax
         extent(5) = zmin
         extent(6) = zmax

!
!        Calculate the cumulative extent of all plates for
!        each degree of freedom.
!
         avext = avext + dabs(bxmax-bxmin) + &
                         dabs(bymax-bymin) + &
                         dabs(bzmax-bzmin)

      end do

!
!     Calculate the average extent of all plates for
!     and the voxel size, i.e the length of one side
!     of a voxel cube.
!
      avext  = avext / dble(3*np)
      voxsiz = voxscl * avext
      mdltol = voxsiz * tol

!
!     Produce a set of vertex extents, extended by MDLTOL,
!     to be used later.
!
      do i = 1, 5, 2

         xextnt(i)   = extent(i)   - mdltol
         xextnt(i+1) = extent(i+1) + mdltol

      end do

!
!     Determine the size of the coarse voxels.
!
      cvxsiz = voxsiz * cgscal

!
!     Determine the minima and maxima of the body centered
!     vertex coordinates expressed in coarse voxel units. Scale the
!     vertices coord values by CVXSIZ: this scales the
!     axis in the voxel model space producing cubic voxels
!     with length 1 along each edge in voxel space,
!     CVXSIZ along an edge in model space.
!
      xvmin = xmin / cvxsiz
      yvmin = ymin / cvxsiz
      zvmin = zmin / cvxsiz
      xvmax = xmax / cvxsiz
      yvmax = ymax / cvxsiz
      zvmax = zmax / cvxsiz

!
!     Extend the coarse voxel grid by at least 1/2
!     coarse voxel length along each degree of freedom.
!
      xvmin = dnint ( xvmin - 1.d0 )
      yvmin = dnint ( yvmin - 1.d0 )
      zvmin = dnint ( zvmin - 1.d0 )
      xvmax = dnint ( xvmax + 1.d0 )
      yvmax = dnint ( yvmax + 1.d0 )
      zvmax = dnint ( zvmax + 1.d0 )

!
!     Calculate the coarse voxel grid origin in model units.
!
      voxori(1) = xvmin * cvxsiz
      voxori(2) = yvmin * cvxsiz
      voxori(3) = zvmin * cvxsiz

!
!     Calculate the dimension of the voxel grid in
!     units of (regular) voxels.
!
      nx = nint( xvmax - xvmin ) * cgscal
      ny = nint( yvmax - yvmin ) * cgscal
      nz = nint( zvmax - zvmin ) * cgscal

      nvox(1) = nx
      nvox(2) = ny
      nvox(3) = nz

      nvxtot = nx*ny*nz
!
!     Make sure the number of voxels NVXTOT is within range.
!
      if ( nvxtot .gt. maxvox ) then

         call setmsg ( 'Fine voxel count NVXTOT = #; count must ' &
         //            'be in the range 1:#.'                    )
         call errint ( '#',  nvxtot                              )
         call errint ( '#',  maxvox                              )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                  )
         call chkout ( 'ZZMKSPIN'                                )
         return

      end if


!
!     Check the coarse voxel scale. It must be at least 1, and its
!     cube must not exceed the fine voxel count.
!
      if (      ( cgscal .lt. 1                ) &
           .or. ( cgscal .gt. nvxtot**(1.d0/3) )  ) then

         call setmsg ( 'Coarse voxel scale = #; scale must ' &
         //            'be in the range 1:NVXTOT**3, where ' &
         //            'NVXTOT is the total fine voxel count. ' &
         //            'In this case, NVXTOT = #.'               )
         call errint ( '#',  cgscal                              )
         call errint ( '#',  nvxtot                              )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                  )
         call chkout ( 'ZZMKSPIN'                                )
         return

      end if

!
!     The cube of the coarse scale must divide the total voxel count
!     evenly. This is a consistency check: the code that derives the
!     voxel grid dimensions should ensure this condition is met.
!
      q = nvxtot /   (cgscal**3)
      r = nvxtot - q*(cgscal**3)

      if ( r .ne. 0 ) then

         call setmsg ( 'Coarse voxel scale = #; the cube of ' &
         //            'the scale must divide NVXTOT evenly, ' &
         //            'where NVXTOT is the total  fine voxel ' &
         //            'count. In this case, NVXTOT = #.'        )
         call errint ( '#',  cgscal                              )
         call errint ( '#',  nvxtot                              )
         call sigerr ( 'SPICE(INCOMPATIBLESCALE)'                )
         call chkout ( 'ZZMKSPIN'                                )
         return

      end if

!
!     Check the number of coarse voxels.
!
      npcg   = cgscal ** 3

      ncgflg = nvxtot / npcg

      if ( ncgflg .gt. maxcgr ) then

         call setmsg ( 'Number of coarse voxels # exceeds ' &
         //            'limit #. Increase coarse voxel scale, ' &
         //            'fine voxel scale, or both.'            )
         call errint ( '#', ncgflg                             )
         call errint ( '#', maxcgr                             )
         call sigerr ( 'SPICE(COARSEGRIDOVERFLOW)'             )
         call chkout ( 'ZZMKSPIN'                              )
         return

      end if

!
!     Enumerate all voxels that each plate might intersect.
!
      call zzinilnk ( maxptr, mxcell, ncell, vxptr, cells )

!
!     Set the dimensions of the coarse grid.
!
      cgrdim(1) = nx / cgscal
      cgrdim(2) = ny / cgscal
      cgrdim(3) = nz / cgscal

      call cleari ( ncgflg, cgrptr )


!
!     TO points to the first free location in the VXPTR array.
!
      to = 1

      do i = 1, np
!
!        Find the extents of the Ith plate, where the extents
!        are expanded by TOL in each direction. We truncate
!        the expanded box at a distance of MDLTOL beyond the
!        extents of the vertex set, if necessary.
!
         xp(1) = vrtces( 1, plates(1,i) )
         xp(2) = vrtces( 1, plates(2,i) )
         xp(3) = vrtces( 1, plates(3,i) )

         yp(1) = vrtces( 2, plates(1,i) )
         yp(2) = vrtces( 2, plates(2,i) )
         yp(3) = vrtces( 2, plates(3,i) )

         zp(1) = vrtces( 3, plates(1,i) )
         zp(2) = vrtces( 3, plates(2,i) )
         zp(3) = vrtces( 3, plates(3,i) )



         bxmin = brcktd( min( xp(1), xp(2), xp(3) ) - mdltol, &
                         xextnt(1),  xextnt(2)                )

         bxmax = brcktd( max( xp(1), xp(2), xp(3) ) + mdltol, &
                         xextnt(1),  xextnt(2)                )

         bymin = brcktd( min( yp(1), yp(2), yp(3) ) - mdltol, &
                         xextnt(3),  xextnt(4)                )

         bymax = brcktd( max( yp(1), yp(2), yp(3) ) + mdltol, &
                         xextnt(3),  xextnt(4)                )

         bzmin = brcktd( min( zp(1), zp(2), zp(3) ) - mdltol, &
                         xextnt(5),  xextnt(6)                )

         bzmax = brcktd( max( zp(1), zp(2), zp(3) ) + mdltol, &
                         xextnt(5),  xextnt(6)                )

!
!        Find the range of voxel coordinates that contain the bounding
!        box of the plate. All we need look at are the coordinates
!        of the two corners having minimum and maximum coordinates.
!
!        Start with the corner having minimum coordinates:
!
         call vpack ( bxmin, bymin, bzmin, vmod )

         call zzgetvox ( voxsiz, voxori, nvox, vmod, inbox, vcoord )

         if ( .not. inbox ) then
!
!           A corner of the bounding box lies outside the voxel grid.
!           This should never occur.
!
            call setmsg ( 'BUG: bounding box of plate is outside of ' &
            //            'voxel grid. Input coordinates were ' &
            //            '(#, #, #). Plate ID = #.'                  )
            call errdp  ( '#', vmod(1)                                )
            call errdp  ( '#', vmod(2)                                )
            call errdp  ( '#', vmod(3)                                )
            call errint ( '#', i                                      )
            call sigerr ( 'SPICE(BUG)'                                )
            call chkout ( 'ZZMKSPIN'                                  )
            return

         end if

!
!        Unpack minimum voxel coordinates from VCOORD.
!
         gxmin = vcoord(1)
         gymin = vcoord(2)
         gzmin = vcoord(3)

!
!        Now handle the corner having maximum coordinates:
!
         call vpack ( bxmax, bymax, bzmax, vmod )

         call zzgetvox ( voxsiz, voxori, nvox, vmod, inbox, vcoord )

         if ( .not. inbox ) then
!
!           A corner of the bounding box lies outside the voxel grid.
!           This should never occur.
!
            call setmsg ( 'BUG: bounding box of plate is outside of ' &
            //            'voxel grid. Input coordinates were ' &
            //            '(#, #, #). Plate ID = #.'                  )
            call errdp  ( '#', vmod(1)                                )
            call errdp  ( '#', vmod(2)                                )
            call errdp  ( '#', vmod(3)                                )
            call errint ( '#', i                                      )
            call sigerr ( 'SPICE(BUG)'                                )
            call chkout ( 'ZZMKSPIN'                                  )
            return

         end if

!
!        Unpack maximum voxel coordinates from VCOORD.
!
         gxmax = vcoord(1)
         gymax = vcoord(2)
         gzmax = vcoord(3)

!
!        Determine voxels that the bounding box of the plate
!        intersects.
!
         do iz = gzmin, gzmax

             do iy = gymin, gymax

                 do ix = gxmin, gxmax

                    vixyz(1) = ix
                    vixyz(2) = iy
                    vixyz(3) = iz
!
!                   Find the coarse voxel containing this voxel, and
!                   compute the offset of this voxel within the coarse
!                   voxel. The output CGXYZ contains the 3-dimensional
!                   coordinates of the coarse voxel within the coarse
!                   grid. The output CGOF1D is the 1-based,
!                   1-dimensional offset of the current voxel (having
!                   coordinates VIXYZ) from the start of the coarse
!                   voxel.
!
                    call zzvoxcvo ( vixyz, nvox,  cgscal, &
                                    cgxyz, cgoff, cgof1d  )

                    if ( failed() ) then
                       call chkout ( 'ZZMKSPIN' )
                       return
                    end if

                    cvid = zzvox2id ( cgxyz, cgrdim )

                    if ( cgrptr(cvid) .eq. 0 ) then
!
!                      The coarse voxel at index CVID is empty so far.
!                      Allocate CGSCAL pointers for it in the VXPTR
!                      array; make the coarse voxel point to the first
!                      element of this sub-array.
!
                       cgrptr(cvid) = to
                       to           = to + npcg

                    end if
!
!                   Let IXPTR be the index in the VXPTR array of the
!                   pointer for the current voxel.
!
                    ixptr = ( cgrptr(cvid) - 1 )  +  cgof1d

                    call zzaddlnk ( ixptr,  i,     maxptr, &
                                    mxcell, vxptr, ncell, cells )

                    if ( failed() ) then
                       call chkout ( 'ZZMKSPIN' )
                       return
                    end if


                 end do

              end do

          end do

      end do

      nvxptr = to - 1

!
!     Generate two linked lists mapping voxel ID to the plates enclosed
!     within that voxel (if any).
!
!     VXPTR : An array, indexed by voxel ID. For an array element,
!             VXPTR(VOX_ID), greater than zero, the value identifies an
!             index in VXLIST, the value of that VXLIST array element
!             equaling the number of plates contained in the voxel
!             specified by the ID. The condition VXPTR(VOX_ID) = -1
!             indicates the voxel contains no plates.
!
!     VXLIST: An array, indexed by the positive entries in VXPTR. The
!             element, N, identified by a VXPTR value describes the
!             number of plates contained in the corresponding voxel.
!
!                 N = VXLIST( VXPTR(VOX_ID) )
!
!             The N elements following VXLIST( VXPTR(VOX_ID) ),
!             contain the IDs of those plates within the voxel.
!
      call zzuntngl ( nvxptr, mxcell, cells, &
                      maxvxl, vxptr,  nvxlst, vxlist )


      call chkout ( 'ZZMKSPIN' )
      return
      end
