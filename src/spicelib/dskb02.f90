!$Procedure DSKB02 ( DSK, fetch type 2 bookkeeping data )

      subroutine dskb02 ( handle, dladsc, nv,     np,     nvxtot, &
                          vtxbds, voxsiz, voxori, vgrext, &
                          cgscal, vtxnpl, voxnpt, voxnpl          )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return bookkeeping data from a DSK type 2 segment.
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
      integer(ip)           nvxtot
      real(wp)              vtxbds ( 2, 3 )
      real(wp)              voxsiz
      real(wp)              voxori ( 3 )
      integer(ip)           vgrext ( 3 )
      integer(ip)           cgscal
      integer(ip)           vtxnpl
      integer(ip)           voxnpt
      integer(ip)           voxnpl

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DSK file handle.
!     DLADSC     I   DLA descriptor.
!     NV         O   Number of vertices in model.
!     NP         O   Number of plates in model.
!     NVXTOT     O   Number of voxels in fine grid.
!     VTXBDS     O   Vertex bounds.
!     VOXSIZ     O   Fine voxel edge length.
!     VOXORI     O   Fine voxel grid origin.
!     VGREXT     O   Fine voxel grid extent.
!     CGSCAL     O   Coarse voxel grid scale.
!     VTXNPL     O   Size of vertex-plate correspondence list.
!     VOXNPT     O   Size of voxel-plate pointer list.
!     VOXNPL     O   Size of voxel-plate correspondence list.
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
!     NV       is the number of vertices in model.
!
!     NP       is the number of plates in model.
!
!     NVXTOT   is the total number of voxels in fine grid.
!
!     VTXBDS   are the vertex bounds. This is an array of six values
!              giving the minimum and maximum values of each component
!              of the vertex set. VTXBDS has dimensions ( 2, 3 ).
!              Units are km.
!
!     VOXSIZ   is the fine grid voxel size. DSK voxels are cubes; the
!              edge length of each cube is given by the voxel size.
!              This size applies to the fine voxel grid. Units are km.
!
!     VOXORI   is the voxel grid origin. This is the location of the
!              voxel grid origin in the body-fixed frame associated
!              with the target body. Units are km.
!
!     VGREXT   is the voxel grid extent. This extent is an array of
!              three integers indicating the number of voxels in the
!              X, Y, and Z directions in the fine voxel grid.
!
!     CGSCAL   is the coarse voxel grid scale. The extent of the fine
!              voxel grid is related to the extent of the coarse voxel
!              grid by this scale factor.
!
!     VTXNPL   is the vertex-plate correspondence list size.
!
!     VOXNPT   is the size of the voxel-to-plate pointer list.
!
!     VOXNPL   is the voxel-plate correspondence list size.
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
!     This routine supports computations involving bookkeeping
!     information stored in DSK type 2 segments. User applications
!     typically will not need to call this routine.
!
!     DSK files are built using the DLA low-level format and
!     the DAS architecture; DLA files are a specialized type of DAS
!     file in which data are organized as a doubly linked list of
!     segments. Each segment's data belong to contiguous components of
!     character, double precision, and integer type.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Dump several parameters from the first DLA segment of
!        a DSK file. The segment is assumed to be of type 2.
!
!
!        Example code begins here.
!
!
!              PROGRAM DSKB02_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!              INCLUDE 'dskdsc.inc'
!              INCLUDE 'dsk02.inc'
!
!        C
!        C     Local parameters
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               LNSIZE
!              PARAMETER           ( LNSIZE = 80 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    DSK
!              CHARACTER*(LNSIZE)    OUTLIN
!
!              DOUBLE PRECISION      VOXORI ( 3 )
!              DOUBLE PRECISION      VOXSIZ
!              DOUBLE PRECISION      VTXBDS ( 2, 3 )
!
!              INTEGER               CGSCAL
!              INTEGER               DLADSC ( DLADSZ )
!              INTEGER               HANDLE
!              INTEGER               NP
!              INTEGER               NV
!              INTEGER               NVXTOT
!              INTEGER               VGREXT ( 3 )
!              INTEGER               VOXNPL
!              INTEGER               VOXNPT
!              INTEGER               VTXNPL
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
!        C     Read and display type 2 bookkeeping data.
!        C
!              CALL DSKB02 ( HANDLE, DLADSC, NV,     NP,     NVXTOT,
!             .              VTXBDS, VOXSIZ, VOXORI, VGREXT, CGSCAL,
!             .              VTXNPL, VOXNPT, VOXNPL                 )
!
!        C
!        C     Show vertex and plate counts.
!        C
!              OUTLIN = 'Number of vertices:                 #'
!              CALL REPMI  ( OUTLIN, '#', NV, OUTLIN )
!              CALL TOSTDO ( OUTLIN )
!
!              OUTLIN = 'Number of plates:                   #'
!              CALL REPMI  ( OUTLIN, '#', NP, OUTLIN )
!              CALL TOSTDO ( OUTLIN )
!
!              OUTLIN = 'Voxel edge length (km):             #'
!              CALL REPMF  ( OUTLIN, '#', VOXSIZ, 6, 'E', OUTLIN )
!              CALL TOSTDO ( OUTLIN )
!
!              OUTLIN = 'Number of voxels:                   #'
!              CALL REPMI  ( OUTLIN, '#', NVXTOT, OUTLIN )
!              CALL TOSTDO ( OUTLIN )
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
!        Number of vertices:                 1579014
!        Number of plates:                   3145728
!        Voxel edge length (km):             1.04248E-01
!        Number of voxels:                   11914500
!
!
! $ Restrictions
!
!     1)  The caller must verify that the segment associated with
!         the input DLA descriptor is a DSK type 2 segment.
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
!        Edited the header to comply with NAIF standard. Added
!        solution for code example.
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        23-JAN-2016 (NJB)
!
!           Removed references to unneeded variables.
!           Updated header comments.
!
!        DSKLIB Version 2.0.0, 05-MAY-2010 (NJB)
!
!           Renamed routine from DSKP02 to DSKB02.
!
!        DSKLIB Version 1.0.1, 08-OCT-2009 (NJB)
!
!           Updated header.
!
!        Beta Version 1.0.0, 30-OCT-2006 (NJB)
!
!-&


! $ Index_Entries
!
!     fetch parameters from a type 2 DSK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer(ip)           dbfsiz
      parameter           ( dbfsiz = 10 )

      integer(ip)           ibfsiz
      parameter           ( ibfsiz = 10 )

      integer(ip)           vtbsiz
      parameter           ( vtbsiz = 6 )

!
!     Local variables
!
      real(wp)              dbuff  ( dbfsiz )

      integer(ip)           b
      integer(ip)           e
      integer(ip)           dpbase
      integer(ip)           ibase
      integer(ip)           ibuff  ( ibfsiz )


      if ( return() ) then
         return
      end if

      call chkin ( 'DSKB02' )

      dpbase = dladsc ( dbsidx )
      ibase  = dladsc ( ibsidx )

!
!     Read the integer parameters first.  These are located at the
!     beginning of the integer component of the segment.
!
      call dasrdi ( handle, ibase+1, ibase+ibfsiz, ibuff )

      nv     = ibuff ( ixnv   )
      np     = ibuff ( ixnp   )
      nvxtot = ibuff ( ixnvxt )
      cgscal = ibuff ( ixcgsc )
      voxnpt = ibuff ( ixvxps )
      voxnpl = ibuff ( ixvxls )
      vtxnpl = ibuff ( ixvtls )

      call movei ( ibuff(ixvgrx), 3, vgrext )

!
!     Read the d.p. parameters.
!
      b = dpbase + ixvtbd
      e = dpbase + ixvtbd + dbfsiz - 1

      call dasrdd( handle, b, e, dbuff )

      call moved ( dbuff,   vtbsiz,  vtxbds )
      call vequ  ( dbuff(vtbsiz+1),  voxori )

      voxsiz = dbuff ( ixvxsz - dskdsz )

      call chkout ( 'DSKB02' )
      return
      end
