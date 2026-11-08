!$Procedure DSKD02 ( DSK, fetch d.p. type 2 data )

      subroutine dskd02 ( handle, dladsc, item, start, room, n, values )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Fetch double precision data from a type 2 DSK segment.
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
      real(wp)              values ( * )

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
!                 KWDSC      DSK descriptor of segment. See the
!                            INCLUDE file dskdsc.inc for a
!                            discussion of the contents of DSK
!                            descriptors. Note that DSK
!                            descriptors are not to be confused
!                            with DLA descriptors, which contain
!                            segment component base address and
!                            size information.
!
!                 KWVTBD     Vertex bounds. This is an array of
!                            six values giving the minimum and
!                            maximum values of each component of
!                            the vertex set.
!
!                 KWVXOR     Voxel grid origin. This is the
!                            location of the voxel grid origin in
!                            the body-fixed frame associated with
!                            the target body.
!
!                 KWVXSZ     Voxel size. DSK voxels are cubes;
!                            the edge length of each cube is
!                            given by the voxel size. This
!                            size applies to the fine voxel grid.
!                            Units are km.
!
!
!     START    is the start index within specified data item from
!              which data are to be fetched. The index of the
!              first element of each data item is 1.
!
!     ROOM     is the amount of room in the output array. It
!              is permissible to provide an output array
!              that has too little room to fetch an item in
!              one call.
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
!
!        Example code begins here.
!
!
!              PROGRAM DSKD02_EX1
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
!                 WRITE (*,*) ' '
!                 WRITE (*,*) 'Plate number: ', I
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
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos512.bds, the output
!        was:
!
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
!-    SPICELIB Version 1.0.1, 13-AUG-2021 (JDR) (BVS)
!
!        Edited the header to comply with NAIF standard. Modified code
!        example to reduce the output.
!
!-    SPICELIB Version 1.0.0, 04-FEB-2017 (NJB)
!
!        Fixed typo in version description.
!
!        23-AUG-2016
!
!           Now saves NV and updates it only when the current
!           segment changes.
!
!        15-JAN-2016 (NJB)
!
!           Removed code involving parameter NP.
!           Updated header $Examples section.
!
!        DSKLIB Version 3.0.0, 13-MAY-2010 (NJB)
!
!           Updated for compatibility with new DSK type 2
!           segment design.
!
!        DSKLIB Version 2.1.0, 20-APR-2010 (NJB)
!
!           Bug fix: changed declaration of output argument
!           VALUES to double precision.
!
!        DSKLIB Version 2.0.0, 27-DEC-2006 (NJB)
!
!           Updated to remove support for min, max radius
!           lookup. These values are now stored in DSK
!           descriptors.
!
!        DSKLIB Version 1.0.0, 27-OCT-2006 (NJB)
!
!-&


! $ Index_Entries
!
!     fetch d.p. data from a type 2 DSK segment
!
!-&


!
!     SPICELIB functions
!
      logical               failed

!
!     Local parameters
!

!
!     Local variables
!
      integer(ip)           b
      integer(ip)           dbase
      integer(ip)           e
      integer(ip)           ibase
      integer(ip)           nv
      integer(ip)           prvhan
      integer(ip)           prvbas
      integer(ip)           size

      logical               first

!
!     Saved variables
!
      save                  first
      save                  prvbas
      save                  prvhan
      save                  nv

!
!     Initial values
!
      data                  first  / .true. /

!
!     Use discovery check-in.  This is done for efficiency; note
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
!        Set previous segment base integer address to an invalid value
!        as well.
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
      dbase  = dladsc ( dbsidx )

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
!        Read NV.
!
         call dasrdi ( handle, ibase+ixnv, ibase+ixnv, nv )

         if ( failed() ) return
!
!        Update the saved handle value.
!
         prvhan = handle

!
!        Update the saved integer base address.
!
         prvbas = ibase

      end if

!
!     Branch based on the item to be returned.
!
!     Note that we haven't checked the validity of START; we'll do this
!     after the IF block.
!
      if ( item == kwvert ) then
!
!        Return vertices.  There are 3*NV values in all.
!        First locate the data.
!
         size =   3*nv
         b    = ( dbase + ixvert ) + start - 1


      else if ( item == kwdsc ) then
!
!        Return DSK descriptor.
!
         size =   dskdsz
         b    = ( dbase + ixdscr ) + start - 1


      else if ( item == kwvtbd ) then
!
!        Return vertex bounds.  There are 6 elements.
!
         size =   6
         b    = ( dbase + ixvtbd ) + start - 1


      else if ( item == kwvxor ) then
!
!        Return voxel grid origin.  There are 3 elements.
!
         size =   3
         b    = ( dbase + ixvxor ) + start - 1


      else if ( item == kwvxsz ) then
!
!        Return voxel size.  This is a scalar.
!
         size =   1
         b    = ( dbase + ixvxsz ) + start - 1

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
      n  =   min ( room, size - start + 1 )
      e  =   b + n - 1


      call dasrdd ( handle, b, e, values )


      return
      end
