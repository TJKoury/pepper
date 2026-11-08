!$Procedure DSKZ02 ( DSK, fetch type 2 model size parameters )

      subroutine dskz02 ( handle, dladsc, nv, np )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return plate model size parameters---plate count and
!     vertex count---for a type 2 DSK segment.
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
      integer(ip)           nv
      integer(ip)           np

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DSK file handle.
!     DLADSC     I   DLA descriptor.
!     NV         O   Number of vertices.
!     NP         O   Number of plates.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DSK file containing a type 2
!              segment from which data are to be fetched.
!
!     DLADSC   is the DLA descriptor associated with the segment
!              from which data are to be fetched.
!
! $ Detailed_Output
!
!     NV       is the number of vertices belonging to
!              the specified plate model.
!
!     NP       is the number of plates belonging to the
!              specified plate model.
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
! $ Files
!
!     See input argument HANDLE.
!
! $ Particulars
!
!     This routine enables SPICE-based user applications to
!     conveniently fetch the plate and vertex counts of a type 2 DSK
!     segment.
!
!     See the routine DSKB02 (DSK, fetch type 2 bookkeeping data)
!     for an interface that returns all type 2 DSK segment
!     bookkeeping data in a single call.
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
!        plate, display the plate's vertices and normal vector.
!
!        For this example, we'll show the context of this look-up:
!        opening the DSK file for read access, traversing a trivial,
!        one-segment list to obtain the segment of interest.
!
!
!        Example code begins here.
!
!
!              PROGRAM DSKZ02_EX1
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR) (BVS)
!
!        Edited the header to comply with NAIF standard. Modified code
!        example to reduce the output.
!
!-    SPICELIB Version 1.0.0, 02-JUN-2010 (NJB)
!
!-&


! $ Index_Entries
!
!     fetch model size parameters from a type 2 DSK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           n



      if ( return() ) then
         return
      end if

      call chkin ( 'DSKZ02' )

      call dski02 ( handle, dladsc, kwnv, 1, 1, n, nv )
      call dski02 ( handle, dladsc, kwnp, 1, 1, n, np )

      call chkout ( 'DSKZ02' )
      return
      end
