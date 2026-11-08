!$Procedure DSKN02 ( DSK, type 2, compute normal vector for plate )

      subroutine dskn02 ( handle, dladsc, plid, normal )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Compute the unit normal vector for a specified plate from a type
!     2 DSK segment.
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
      integer(ip)           plid
      real(wp)              normal ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DSK file handle.
!     DLADSC     I   DLA descriptor.
!     PLID       I   Plate ID.
!     NORMAL     O   Plate's unit normal vector.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DSK file containing a type 2
!              segment from which data are to be fetched.
!
!     DLADSC   is the DLA descriptor associated with the segment
!              from which data are to be fetched.
!
!     PLID     is the plate ID. Plate IDs range from 1 to NP
!              (the number of plates).
!
! $ Detailed_Output
!
!     NORMAL   is the normal vector associated with the plate
!              designated by PLID. The direction of NORMAL is
!              determined by the order of the plate's vertices;
!              the vertices are presumed to be ordered in the
!              right-handed (counterclockwise) sense about the
!              normal direction. If the plate's vertices are
!              V1, V2, V3, then NORMAL points in the direction
!
!                 (V2 - V1) x ( V3 - V2 )
!
!              where "x" represents the cross product operator.
!
!              The vector NORMAL is expressed in the body-fixed
!              reference frame of the segment designated by DLADSC.
!              The center of this frame is the origin of the cartesian
!              coordinate system in which the vertices are expressed.
!              Note that the frame center need not coincide with the
!              central body of the segment. Units are km.
!
!              The vector has magnitude 1.
!
!              If an error occurs on the call, NORMAL is undefined.
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
!     4)  If PLID is less than 1 or greater than the number of plates
!         in the segment, the error SPICE(INDEXOUTOFRANGE) is signaled.
!
!     5)  This routine does not check for linear independence of the
!         plate's edges. The plate model is assumed to be geometrically
!         valid.
!
! $ Files
!
!     See input argument HANDLE.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Look up all the vertices associated with each plate
!        of the model contained in a specified type 2 segment. For each
!        of the first 5 plates, display the plate's vertices and normal
!        vector.
!
!        For this example, we'll show the context of this look-up:
!        opening the DSK file for read access, traversing a trivial,
!        one-segment list to obtain the segment of interest.
!
!
!        Example code begins here.
!
!
!              PROGRAM DSKN02_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!              INCLUDE 'dsk02.inc'
!
!
!              CHARACTER*(*)         FMT
!              PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
!
!
!              INTEGER               BUFSIZ
!              PARAMETER           ( BUFSIZ = 10000 )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!
!              CHARACTER*(FILSIZ)    DSK
!
!              DOUBLE PRECISION      NORMAL ( 3 )
!              DOUBLE PRECISION      VERTS  ( 3, BUFSIZ )
!
!              INTEGER               DLADSC ( DLADSZ )
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!              INTEGER               N
!              INTEGER               NNORM
!              INTEGER               NP
!              INTEGER               NREAD
!              INTEGER               NV
!              INTEGER               NVTX
!              INTEGER               PLATES  ( 3, BUFSIZ )
!              INTEGER               PLIX
!              INTEGER               REMAIN
!              INTEGER               START
!
!              LOGICAL               FOUND
!
!        C
!        C     Prompt for name of DSK and open file for reading.
!        C
!              CALL PROMPT ( 'Enter DSK name > ', DSK )
!
!              CALL DASOPR ( DSK, HANDLE )
!
!              CALL DLABFS ( HANDLE, DLADSC, FOUND )
!
!              IF ( .NOT. FOUND ) THEN
!
!                 CALL SETMSG ( 'No segment found in file #.' )
!                 CALL ERRCH  ( '#',  DSK                     )
!                 CALL SIGERR ( 'SPICE(NOSEGMENT)'            )
!
!              END IF
!
!        C
!        C     Get segment vertex and plate counts.
!        C
!              CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Number of vertices: ', NV
!              WRITE (*,*) 'Number of plates:   ', NP
!        C
!        C     Display the vertices of the first 5 plates.
!        C
!              REMAIN = MIN(5, NP)
!              START  = 1
!
!              DO WHILE ( REMAIN .GT. 0 )
!        C
!        C        NREAD is the number of plates we'll read on this
!        C        loop pass.
!        C
!                 NREAD  = MIN ( BUFSIZ, REMAIN )
!
!                 CALL DSKP02 ( HANDLE, DLADSC, START, NREAD, N,
!             .                 PLATES                          )
!
!                 DO I = 1, N
!
!                    PLIX = START + I - 1
!        C
!        C           Read the vertices of the current plate.
!        C
!                    DO J = 1, 3
!                       CALL DSKV02 ( HANDLE, DLADSC, PLATES(J,I),
!             .                       1,      NVTX,   VERTS (1,J)  )
!                    END DO
!        C
!        C           Display the vertices of the current plate:
!        C
!                    WRITE (*,*  ) ' '
!                    WRITE (*,*  ) 'Plate number: ', PLIX
!                    WRITE (*,FMT) '   Vertex 1: ', (VERTS(J,1), J=1,3)
!                    WRITE (*,FMT) '   Vertex 2: ', (VERTS(J,2), J=1,3)
!                    WRITE (*,FMT) '   Vertex 3: ', (VERTS(J,3), J=1,3)
!
!        C
!        C           Display the normal vector of the current plate:
!        C
!                    CALL DSKN02 ( HANDLE, DLADSC, PLIX, NORMAL )
!
!                    WRITE (*,FMT) '   Normal:   ', (NORMAL(J), J=1,3)
!
!                 END DO
!
!                 START  = START  + NREAD
!                 REMAIN = REMAIN - NREAD
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
!
!         Number of vertices:      1579014
!         Number of plates:        3145728
!
!         Plate number:            1
!            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
!            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
!            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
!            Normal:    -0.58197377E+00  0.32128561E+00  0.74704892E+00
!
!         Plate number:            2
!            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
!            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
!            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
!            Normal:    -0.58145695E+00  0.32198831E+00  0.74714881E+00
!
!         Plate number:            3
!            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
!            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
!            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
!            Normal:    -0.58159707E+00  0.32264196E+00  0.74675767E+00
!
!         Plate number:            4
!            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
!            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
!            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
!            Normal:    -0.58312901E+00  0.32056070E+00  0.74645924E+00
!
!         Plate number:            5
!            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
!            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
!            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
!            Normal:    -0.58366405E+00  0.32306020E+00  0.74496200E+00
!
!
!        Note that only the vertex information for first 5 plates is
!        provided.
!
! $ Restrictions
!
!     See $Exceptions section.
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
!-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR) (BVS)
!
!        Edited the header to comply with NAIF standard. Modified code
!        example to reduce the output.
!
!        Extended detailed description of NORMAL output argument and
!        added an additional record to $Index_Entries.
!
!-    SPICELIB Version 1.0.0, 17-MAR-2016 (NJB)
!
!        Now calls ZZDDHHLU.
!
!        Deleted references to unused parameter. Updated
!        $Examples section.
!
!        DSKLIB Version 1.0.0, 02-JUN-2010 (NJB)
!
!-&


! $ Index_Entries
!
!     compute normal vector for a type 2 DSK plate
!     compute normal vector from DSK type 2 plate id
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return
!
!     Local variables
!
      real(wp)              dskdsc ( dskdsz )
      real(wp)              edge1  ( 3 )
      real(wp)              edge2  ( 3 )
      real(wp)              verts  ( 3, 3 )

      integer(ip)           i
      integer(ip)           n
      integer(ip)           np
      integer(ip)           nv
      integer(ip)           plate  ( 3 )
      integer(ip)           start
      integer(ip)           unit

      if ( return() ) then
         return
      end if

      call chkin ( 'DSKN02' )

!
!     Look up the DSK descriptor for this segment.
!
      call dskgd ( handle, dladsc, dskdsc )

!
!     Get the plate model size parameters for this segment.
!     Note that we get a segment data type check for free from
!     DSKZ02.
!
      call dskz02 ( handle, dladsc, nv, np )

!
!     Check START.
!
      if (  ( plid < 1 ) .or. ( plid > np )  ) then

         call zzddhhlu ( handle, 'DAS', .false., unit )

         call setmsg ( 'Segment in DSK file # with DAS base ' &
         //            'addresses INT = #, DP = #, CHR = # ' &
         //            'contains # plates, so PLID must ' &
         //            'be in the range 1:#; actual value ' &
         //            'was #.'                              )
         call errfnm ( '#', unit                             )
         call errint ( '#', dladsc(ibsidx)                   )
         call errint ( '#', dladsc(dbsidx)                   )
         call errint ( '#', dladsc(cbsidx)                   )
         call errint ( '#', np                               )
         call errint ( '#', np                               )
         call errint ( '#', plid                             )
         call sigerr ( 'SPICE(INDEXOUTOFRANGE)'              )
         call chkout ( 'DSKN02'                              )
         return

      end if

!
!     Look up the plate and its vertices.
!
      start = (plid-1)*3  +  1

      call dski02 ( handle, dladsc, kwplat, start, 3, n, plate )

      do i = 1, 3

         start = ( plate(i) - 1 )*3  + 1

         call dskd02 ( handle, dladsc, kwvert, start, 3, n, verts(1,i) )

      end do

      if ( failed() ) then
         call chkout ( 'DSKN02' )
         return
      end if

!
!     Use the right-handed order of the vertices to determine the
!     correct choice of normal direction.
!
      call vsub ( verts(1,2), verts(1,1), edge1 )
      call vsub ( verts(1,3), verts(1,1), edge2 )

      call ucrss ( edge1, edge2, normal )

      call chkout ( 'DSKN02' )
      return
      end
