!$Procedure DSKI02 ( DSK, fetch integer type 2 data )

      subroutine dski02 ( handle, dladsc, item, start, room, n, values )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Fetch integer data from a type 2 DSK segment.
!
! $ Required_Reading
!
!     DAS
!     DSK
!
! $ Keywords
!
!     DAS
!     DSK
!     FILES
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'dla.inc'
      include 'dskdsc.inc'
      include 'dsk02.inc'

      integer(ip)           handle
      integer(ip)           dladsc ( * )
      integer(ip)           item
      integer(ip)           start
      integer(ip)           room
      integer(ip)           n
      integer(ip)           values ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DSK file handle.
!     DLADSC     I   DLA descriptor.
!     ITEM       I   Keyword identifying item to fetch.
!     START      I   Start index.
!     ROOM       I   Amount of room in output array.
!     N          O   Number of values returned.
!     VALUES     O   Array containing requested item.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DSK file containing a type 2
!              segment from which data are to be fetched.
!
!     DLADSC   is the DLA descriptor associated with the segment
!              from which data are to be fetched.
!
!     ITEM     is an integer "keyword" parameter designating the
!              item to fetch. In the descriptions below, note
!              that "model" refers to the model represented by
!              the designated segment. This model may be a
!              subset of a larger model.
!
!              Names and meanings of parameters supported by this
!              routine are:
!
!                 KWNV       Number of vertices in model.
!
!                 KWNP       Number of plates in model.
!
!                 KWNVXT     Total number of voxels in fine grid.
!
!                 KWVGRX     Voxel grid extent. This extent is
!                            an array of three integers
!                            indicating the number of voxels in
!                            the X, Y, and Z directions in the
!                            fine voxel grid.
!
!                 KWCGSC     Coarse voxel grid scale. The extent
!                            of the fine voxel grid is related to
!                            the extent of the coarse voxel grid
!                            by this scale factor.
!
!                 KWVXPS     Size of the voxel-to-plate pointer
!                            list.
!
!                 KWVXLS     Voxel-plate correspondence list size.
!
!                 KWVTLS     Vertex-plate correspondence list
!                            size.
!
!                 KWPLAT     Plate array. For each plate, this
!                            array contains the indices of the
!                            plate's three vertices. The ordering
!                            of the array members is:
!
!                               Plate 1 vertex index 1
!                               Plate 1 vertex index 2
!                               Plate 1 vertex index 3
!                               Plate 2 vertex index 1
!                                       ...
!
!                 KWVXPT     Voxel-plate pointer list. This list
!                            contains pointers that map fine
!                            voxels to lists of plates that
!                            intersect those voxels. Note that
!                            only fine voxels belonging to
!                            non-empty coarse voxels are in the
!                            domain of this mapping.
!
!                 KWVXPL     Voxel-plate correspondence list.
!                            This list contains lists of plates
!                            that intersect fine voxels. (This
!                            list is the data structure into
!                            which the voxel-to-plate pointers
!                            point.) This list can contain
!                            empty lists.
!
!                 KWVTPT     Vertex-plate pointer list. This list
!                            contains pointers that map vertices
!                            to lists of plates to which those
!                            vertices belong.
!
!                            Note that the size of this list is
!                            always NV, the number of vertices.
!                            Hence there's no need for a separate
!                            keyword for the size of this list.
!
!                 KWVTPL     Vertex-plate correspondence list.
!                            This list contains, for each vertex,
!                            the indices of the plates to which
!                            that vertex belongs.
!
!                 KWCGPT     Coarse voxel grid pointers. This is
!                            an array of pointers mapping coarse
!                            voxels to lists of pointers in the
!                            voxel-plate pointer list. Each
!                            non-empty coarse voxel maps to a
!                            list of pointers; every fine voxel
!                            contained in a non-empty coarse voxel
!                            has its own pointers. Grid elements
!                            corresponding to empty coarse voxels
!                            have null (non-positive) pointers.
!
!              See the INCLUDE file dsk.inc for values
!              associated with the keyword parameters.
!
!     START    is the start index within the specified data item
!              from which data are to be fetched. The index of
!              the first element of each data item is 1. START
!              has units of integers; for example, the start
!              index of the second plate is 4, since each plate
!              occupies three integers.
!
!     ROOM     is the amount of room in the output array. It is
!              permissible to provide an output array that has
!              too little room to fetch an item in one call. ROOM
!              has units of integers: for example, the room
!              required to fetch one plate is 3.
!
! $ Detailed_Output
!
!     N        is the number of elements fetched to the output
!              array VALUES. N is normally in the range
!              1:ROOM; if an error occurs on the call, N is
!              undefined.
!
!     VALUES   is a contiguous set of elements of the item
!              designated by ITEM. The correspondence of
!              VALUES with the elements of the data item is:
!
!                 VALUES(1)      ITEM(START)
!                   ...             ...
!                 VALUES(N)      ITEM(START+N-1)
!
!              If an error occurs on the call, VALUES is
!              undefined.
!
! $ Parameters
!
!     See the include file
!
!        dla.inc
!
!     for declarations of DLA descriptor sizes and documentation of the
!     contents of DLA descriptors.
!
!     See the include file
!
!        dskdsc.inc
!
!     for declarations of DSK descriptor sizes and documentation of the
!     contents of DSK descriptors.
!
!     See the include file
!
!        dsk02.inc
!
!     for declarations of DSK data type 2 (plate model) parameters.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If a file read error occurs, the error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  If the input DLA descriptor is invalid, the effect of this
!         routine is undefined. The error *may* be diagnosed by
!         routines in the call tree of this routine, but there are no
!         guarantees.
!
!     4)  If ROOM is non-positive, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     5)  If the coarse voxel scale read from the designated segment
!         is less than 1, the error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     6)  If the input keyword parameter is not recognized, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     7)  If START is less than 1 or greater than the size of the
!         item to be fetched, the error SPICE(INDEXOUTOFRANGE) is
!         signaled.
!
! $ Files
!
!     See input argument HANDLE.
!
! $ Particulars
!
!     Most SPICE applications will not need to call this routine. The
!     routines DSKV02, DSKP02, and DSKZ02 provide a higher-level
!     interface for fetching DSK type 2 vertex and plate data.
!
!     DSK files are built using the DLA low-level format and
!     the DAS architecture; DLA files are a specialized type of DAS
!     file in which data are organized as a doubly linked list of
!     segments. Each segment's data belong to contiguous components of
!     character, double precision, and integer type.
!
!     Note that the DSK descriptor for the segment is not needed by
!     this routine; the DLA descriptor contains the base address and
!     size information for the integer, double precision, and character
!     components of the segment, and these suffice for the purpose of
!     fetching data.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Look up all the vertices associated with each plate
!        of the model contained in a specified type 2 segment.
!        For this example, we'll show the context of this look-up:
!        opening the DSK file for read access, traversing a trivial,
!        one-segment list to obtain the segment of interest.
!
!        Example code begins here.
!
!              PROGRAM DSKI02_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!              INCLUDE 'dskdsc.inc'
!              INCLUDE 'dsk02.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT
!              PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    DSK
!
!              DOUBLE PRECISION      VRTCES ( 3, 3 )
!
!              INTEGER               DLADSC ( DLADSZ )
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!              INTEGER               K
!              INTEGER               N
!              INTEGER               NP
!              INTEGER               START
!              INTEGER               VRTIDS ( 3 )
!
!              LOGICAL               FOUND
!
!        C
!        C     Prompt for the name of the DSK to read.
!        C
!              CALL PROMPT ( 'Enter DSK name > ', DSK )
!        C
!        C     Open the DSK file for read access.
!        C     We use the DAS-level interface for
!        C     this function.
!        C
!              CALL DASOPR ( DSK, HANDLE )
!
!        C
!        C     Begin a forward search through the
!        C     kernel, treating the file as a DLA.
!        C     In this example, it's a very short
!        C     search.
!        C
!              CALL DLABFS ( HANDLE, DLADSC, FOUND )
!
!              IF ( .NOT. FOUND ) THEN
!        C
!        C        We arrive here only if the kernel
!        C        contains no segments.  This is
!        C        unexpected, but we're prepared for it.
!        C
!                 CALL SETMSG ( 'No segments found '
!             .   //            'in DSK file #.'    )
!                 CALL ERRCH  ( '#',  DSK           )
!                 CALL SIGERR ( 'SPICE(NODATA)'     )
!
!              END IF
!
!        C
!        C     If we made it this far, DLADSC is the
!        C     DLA descriptor of the first segment.
!        C
!        C     Find the number of plates in the model.
!        C
!              CALL DSKI02 ( HANDLE, DLADSC, KWNP, 1, 1, N, NP )
!              WRITE (*,*) 'Number of plates: ', NP
!
!        C
!        C     For the first 5 plates, look up the desired data.
!        C
!              K = MIN(5, NP)
!              DO I = 1, K
!        C
!        C        For the Ith plate, find the associated
!        C        vertex IDs.  We must take into account
!        C        the fact that each plate has three
!        C        vertices when we compute the start
!        C        index.
!        C
!                 START = 3*(I-1)+1
!
!                 CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, START,
!             .                 3,      N,      VRTIDS        )
!
!                 DO J = 1, 3
!        C
!        C            Fetch the vertex associated with
!        C            the Jth vertex ID.  Again, each
!        C            vertex is a 3-vector.  Note that
!        C            the vertices are double-precision
!        C            data, so we fetch them using
!        C            DSKD02.
!        C
!                     START = 3*( VRTIDS(J) - 1 ) + 1
!
!                     CALL DSKD02 ( HANDLE, DLADSC, KWVERT,  START,
!             .                     3,      N,      VRTCES(1,J)    )
!                 END DO
!
!        C
!        C        Display the vertices of the Ith plate:
!        C
!                 WRITE (*,*)   ' '
!                 WRITE (*,*)   'Plate number: ', I
!                 WRITE (*,FMT) '   Vertex 1: ', (VRTCES(J,1), J=1,3)
!                 WRITE (*,FMT) '   Vertex 2: ', (VRTCES(J,2), J=1,3)
!                 WRITE (*,FMT) '   Vertex 3: ', (VRTCES(J,3), J=1,3)
!
!              END DO
!
!        C
!        C     Close the kernel.  This isn't necessary in a stand-
!        C     alone program, but it's good practice in subroutines
!        C     because it frees program and system resources.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos512.bds, the output
!        was:
!
!        Enter DSK name > phobos512.bds
!         Number of plates:      3145728
!
!         Plate number:            1
!            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
!            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
!            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
!
!         Plate number:            2
!            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
!            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
!            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
!
!         Plate number:            3
!            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
!            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
!            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
!
!         Plate number:            4
!            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
!            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
!            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
!
!         Plate number:            5
!            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
!            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
!            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
!
!        Note that only the vertex information for first 5 plates is
!        provided.
!
! $ Restrictions
!
!     1)  This routine uses discovery check-in to boost
!         execution speed. However, this routine is in
!         violation of NAIF standards for use of discovery
!         check-in:  routines called from this routine may
!         signal errors. If errors are signaled in called
!         routines, this routine's name will be missing
!         from the traceback message.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 02-JUL-2021 (JDR) (BVS)
!
!        Edited the header to comply with NAIF standard. Extended the
!        $Keywords section. Modified code example to reduce the output.
!
!-    SPICELIB Version 1.0.0, 22-NOV-2016 (NJB)
!
!        Added FAILED check after segment attribute fetch calls.
!        Re-ordered code so that values are saved only after
!        all error checks have passed. Simplified base address
!        comparisons.
!
!        15-JAN-2016 (NJB)
!
!           Updated header $Examples and $Particulars sections.
!
!        DSKLIB Version 1.0.2, 11-JUL-2014 (NJB)
!
!           Fixed a trivial header comment typo.
!
!        DSKLIB Version 1.0.1, 13-MAY-2010 (NJB)
!
!           Updated header.
!
!        DSKLIB Version 1.0.0, 27-OCT-2006 (NJB)
!
!-&

! $ Index_Entries
!
!     fetch integer data from a type 2 DSK segment
!
!-&

!
!     SPICELIB functions
!
      logical              failed

!
!     Local parameters
!
!     IBFSIZ is the size of an integer buffer used to
!     read parameters from the segment.
!
      integer(ip), parameter :: ibfsiz = 10

!
!     Local variables
!
      integer(ip)           b
      integer(ip)           cgscal
      integer(ip)           e
      integer(ip)           ibase
      integer(ip)           ibuff  ( ibfsiz )
      integer(ip)           ncgr
      integer(ip)           np
      integer(ip)           nv
      integer(ip)           nvxtot
      integer(ip)           prvbas
      integer(ip)           prvhan
      integer(ip)           size
      integer(ip)           voxnpl
      integer(ip)           voxnpt
      integer(ip)           vtxnpl

      logical               first

!
!     Saved variables
!
      save                  cgscal
      save                  first
      save                  np
      save                  nv
      save                  nvxtot
      save                  prvbas
      save                  prvhan
      save                  voxnpl
      save                  voxnpt
      save                  vtxnpl

!
!     Initial values
!
      data                  first  / .true. /

!
!  This is done for efficiency; note
!     however that this routine does not meet SPICE standards for
!     discovery check-in eligibility.
!
      if ( first ) then
!
!        Make sure we treat the input handle as new on the first pass.
!        Set PRVHAN to an invalid handle value.
!
         prvhan = 0

!
!        Set the previous segment base integer address to an invalid
!        value as well.
!
         prvbas = -1

         first  = .false.

      end if

      if ( room <= 0 ) then

         call setmsg ( 'ROOM was #; must be positive.' )
         call errint ( '#',  room                      )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'        )
         return

      end if

      ibase  = dladsc ( ibsidx )
!
!     Either a new file or new segment in the same file will require
!     looking up the segment parameters. To determine whether the
!     segment is new, we don't need to compare the entire DLA
!     descriptor: just comparing the integer base address of the
!     descriptor against the saved integer base address is sufficient.
!
!     DSK type 2 segments always have a non-empty integer component, so
!     each type 2 segment in a given file will have a distinct integer
!     base address. Segments of other types might not contain integers,
!     but they can't share an integer base address with a type 2
!     segment.
!
      if (      ( handle /= prvhan ) &
           .or. ( ibase  /= prvbas )  ) then
!
!        Treat the input file and segment as new.
!
!        Read the integer parameters first.  These are located at the
!        beginning of the integer component of the segment.
!
         call dasrdi ( handle, ibase+1, ibase+ibfsiz, ibuff )

         if ( failed() ) return

!
!        Check the coarse voxel scale.
!
         if ( ibuff(ixcgsc) < 1 ) then

            call setmsg ( 'Coarse voxel grid scale is #; ' // &
                          'this scale should be an '       // &
                          'integer > 1'                    )
            call errint ( '#',  cgscal                     )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'         )
            return

         end if

!
!        All checks have passed. We can safely store the segment
!        parameters.
!
         nv     = ibuff ( ixnv   )
         np     = ibuff ( ixnp   )
         nvxtot = ibuff ( ixnvxt )
         cgscal = ibuff ( ixcgsc )
         vtxnpl = ibuff ( ixvtls )
         voxnpt = ibuff ( ixvxps )
         voxnpl = ibuff ( ixvxls )

!
!        Update the saved handle value.
!
         prvhan = handle

!
!        Update the saved base integer address.
!
         prvbas = ibase

      end if

!
!     Branch based on the item to be returned.
!
!     Note that we haven't checked the validity of START; we'll do this
!     after the IF block.
!
      if ( item == kwnv ) then
!
!        Return the number of vertices.
!
         n         = 1
         values(1) = nv

!
!        As long as START is valid, we can return. Otherwise,
!        let control pass to the error handling block near
!        the end of this routine.
!
         if ( start == 1 ) return

      else if ( item == kwnp ) then
!
!        Return the number of plates.
!
         n         = 1
         values(1) = np

         if ( start == 1 ) return

      else if ( item == kwnvxt ) then
!
!        Return the total number of voxels.
!
         n         = 1
         values(1) = nvxtot

         if ( start == 1 ) return

      else if ( item == kwvgrx ) then
!
!        Return the voxel grid extents.
!
         size = 3
         b    = ibase + ixvgrx + start - 1

      else if ( item == kwcgsc ) then
!
!        Return the coarse voxel grid scale.
!
         n         = 1
         values(1) = cgscal

         if ( start == 1 ) return

      else if ( item == kwvxps ) then
!
!        Return the voxel-plate pointer list size.
!
         n         = 1
         values(1) = voxnpt

         if ( start == 1 ) return

      else if ( item == kwvxls ) then
!
!        Return the voxel-plate list size.
!
         n         = 1
         values(1) = voxnpl

         if ( start == 1 ) return

      else if ( item == kwvtls ) then
!
!        Return the vertex-plate list size.
!
         n         = 1
         values(1) = vtxnpl

         if ( start == 1 ) return

      else if ( item == kwplat ) then
!
!        Return plate data.  There are 3*NP values in all.  First
!        locate the data.
!
         size = 3*np
         b    = ( ibase + ixplat ) + start - 1

      else if ( item == kwvxpt )  then
!
!        Return voxel pointer data.  There are VOXNPT values in all.
!        First locate the data.
!
         size = voxnpt
         b    = ( ibase + ixplat + 3*np ) + start - 1

      else if ( item == kwvxpl )  then
!
!        Return voxel-plate list data.  There are VOXNPL values in all.
!        First locate the data.
!
         size = voxnpl
         b    = ( ibase + ixplat + 3*np + voxnpt ) + start - 1

      else if ( item == kwvtpt )  then
!
!        Return vertex-plate pointer data.  There are NV values in all.
!        First locate the data.
!
         size =    nv
         b    =  ( ibase + ixplat + 3*np + voxnpt + voxnpl ) &
                 + start - 1

      else if ( item == kwvtpl )  then
!
!        Return vertex-plate list data.  There are VTXNPL values in
!        all. First locate the data.
!
         size = vtxnpl

         b =  ( ibase + ixplat + 3*np + voxnpt + voxnpl + nv ) &
              + start - 1

      else if ( item == kwcgpt )  then
!
!        Compute the coarse grid size.
!
         ncgr = nvxtot / (cgscal**3)

!
!        Return the coarse voxel grid occupancy pointers.  There are
!
!           NCGR
!
!        values in all. First locate the data.
!
         size = ncgr

         b =  (   ibase  + ixplat + 3*np   + voxnpt &
                + voxnpl + nv     + vtxnpl          ) &
                + start  - 1

      else

         call setmsg ( 'Keyword parameter # was not recognized.' )
         call errint ( '#',  item                                )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                     )
         return

      end if

!
!     The valid range for START is 1:SIZE.
!
      if (  ( start < 1 ) .or. ( start > size )  ) then

         call setmsg ( 'START must be in the range defined ' // &
                       'by the size of the data associated ' // &
                       'with the keyword parameter #, '      // &
                       'namely 1:#.  Actual value of START ' // &
                       'was #.'                              )
         call errint ( '#',  item                            )
         call errint ( '#',  size                            )
         call errint ( '#',  start                           )
         call sigerr ( 'SPICE(INDEXOUTOFRANGE)'              )
         return

      end if

!
!     Read the requested data.  We already have the start address B.
!
      n  =  min ( room,  size - start + 1 )
      e  =  b + n - 1

      call dasrdi ( handle, b, e, values )

      return
      end
