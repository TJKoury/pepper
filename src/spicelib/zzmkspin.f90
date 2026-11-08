!$Procedure ZZMKSPIN ( Make spatial index of plates )

      SUBROUTINE ZZMKSPIN ( NP,     PLATES, VRTCES, VOXSCL, CGSCAL, &
                            MAXPTR, MXCELL, MAXVXL, CELLS,  NVOX, &
                            VOXSIZ, VOXORI, NVXTOT, NVXPTR, VXPTR, &
                            NVXLST, VXLIST, EXTENT, CGRPTR         )
      IMPLICIT NONE

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

      INCLUDE               'dsk02.inc'

      INTEGER               NP
      INTEGER               PLATES ( 3, * )
      DOUBLE PRECISION      VRTCES ( 3, * )
      DOUBLE PRECISION      VOXSCL
      INTEGER               CGSCAL
      INTEGER               MAXPTR
      INTEGER               MXCELL
      INTEGER               MAXVXL
      INTEGER               CELLS   ( 2, MXCELL )
      INTEGER               NVOX   ( 3 )
      DOUBLE PRECISION      VOXSIZ
      DOUBLE PRECISION      VOXORI ( 3 )
      INTEGER               NVXTOT
      INTEGER               NVXPTR
      INTEGER               VXPTR  ( * )
      INTEGER               NVXLST
      INTEGER               VXLIST ( * )
      DOUBLE PRECISION      EXTENT ( 6 )
      INTEGER               CGRPTR ( * )

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
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Other functions
!
      INTEGER               ZZVOX2ID

!
!     Local parameters
!

!
!     Fraction of voxel edge length used a tolerance for plate
!     inclusion in voxels:
!
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL  = 1.D-3 )

!
!     Local Variables
!
      DOUBLE PRECISION      AVEXT
      DOUBLE PRECISION      BXMAX
      DOUBLE PRECISION      BXMIN
      DOUBLE PRECISION      BYMAX
      DOUBLE PRECISION      BYMIN
      DOUBLE PRECISION      BZMAX
      DOUBLE PRECISION      BZMIN
      DOUBLE PRECISION      CVXSIZ
      DOUBLE PRECISION      MDLTOL
      DOUBLE PRECISION      VMOD   ( 3 )
      DOUBLE PRECISION      XEXTNT ( 6 )
      DOUBLE PRECISION      XMAX
      DOUBLE PRECISION      XMIN
      DOUBLE PRECISION      XP     ( 3 )
      DOUBLE PRECISION      XVMAX
      DOUBLE PRECISION      XVMIN
      DOUBLE PRECISION      YMAX
      DOUBLE PRECISION      YMIN
      DOUBLE PRECISION      YP     ( 3 )
      DOUBLE PRECISION      YVMAX
      DOUBLE PRECISION      YVMIN
      DOUBLE PRECISION      ZMAX
      DOUBLE PRECISION      ZMIN
      DOUBLE PRECISION      ZP     ( 3 )
      DOUBLE PRECISION      ZVMAX
      DOUBLE PRECISION      ZVMIN

      INTEGER               CGOF1D
      INTEGER               CGOFF  ( 3 )
      INTEGER               CGRDIM ( 3 )
      INTEGER               CGXYZ  ( 3 )
      INTEGER               CVID
      INTEGER               GXMAX
      INTEGER               GXMIN
      INTEGER               GYMAX
      INTEGER               GYMIN
      INTEGER               GZMAX
      INTEGER               GZMIN
      INTEGER               I
      INTEGER               IX
      INTEGER               IXPTR
      INTEGER               IY
      INTEGER               IZ
      INTEGER               J
      INTEGER               NCELL
      INTEGER               NCGFLG
      INTEGER               NPCG
      INTEGER               NX
      INTEGER               NY
      INTEGER               NZ
      INTEGER               Q
      INTEGER               R
      INTEGER               TO
      INTEGER               VIXYZ  ( 3 )
      INTEGER               VCOORD ( 3 )

      LOGICAL               INBOX

!
!     Saved variables
!
!
!     Required for f2c use on Linux, all local variables
!     to static.
!
      SAVE

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZMKSPIN' )

!
!     Check NP.
!
      IF (  ( NP .LT. 1 ) .OR. ( NP .GT. MAXPLT )  ) THEN

         CALL SETMSG ( 'Plate count NP = #; count must ' &
         //            'be in the range 1:#.'            )
         CALL ERRINT ( '#',  NP                          )
         CALL ERRINT ( '#',  MAXPLT                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'          )
         CALL CHKOUT ( 'ZZMKSPIN'                        )
         RETURN

      END IF

!
!     Make sure the coarse voxel scale is positive. We'll
!     perform additional checks later on. Those checks
!     require computations that can't be done if the coarse
!     scale is zero.
!
      IF ( CGSCAL .LT. 1 ) THEN

         CALL SETMSG ( 'Coarse voxel scale = #; scale must ' &
         //            'be positive.'                       )
         CALL ERRINT ( '#',  CGSCAL                         )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'             )
         CALL CHKOUT ( 'ZZMKSPIN'                           )
         RETURN

      END IF

!
!     Get the average extents of all plates and the
!     overall model extent. The extents have model units; in
!     other words km.
!
      AVEXT = 0.D0

      XMIN =  DPMAX()
      XMAX =  DPMIN()
      YMIN =  DPMAX()
      YMAX =  DPMIN()
      ZMIN =  DPMAX()
      ZMAX =  DPMIN()

      DO I = 1, NP

         BXMIN = DPMAX()
         BXMAX = DPMIN()
         BYMIN = DPMAX()
         BYMAX = DPMIN()
         BZMIN = DPMAX()
         BZMAX = DPMIN()

         XP(1) = VRTCES( 1, PLATES(1,I) )
         XP(2) = VRTCES( 1, PLATES(2,I) )
         XP(3) = VRTCES( 1, PLATES(3,I) )

         YP(1) = VRTCES( 2, PLATES(1,I) )
         YP(2) = VRTCES( 2, PLATES(2,I) )
         YP(3) = VRTCES( 2, PLATES(3,I) )

         ZP(1) = VRTCES( 3, PLATES(1,I) )
         ZP(2) = VRTCES( 3, PLATES(2,I) )
         ZP(3) = VRTCES( 3, PLATES(3,I) )

         DO J = 1, 3
!
!           Determine plate extents.
!
            BXMIN = DMIN1( BXMIN, XP(J) )
            BXMAX = DMAX1( BXMAX, XP(J) )

            BYMIN = DMIN1( BYMIN, YP(J) )
            BYMAX = DMAX1( BYMAX, YP(J) )

            BZMIN = DMIN1( BZMIN, ZP(J) )
            BZMAX = DMAX1( BZMAX, ZP(J) )

!
!           Determine model extent.
!
            XMIN = DMIN1 ( XMIN, BXMIN )
            XMAX = DMAX1 ( XMAX, BXMAX )

            YMIN = DMIN1 ( YMIN, BYMIN )
            YMAX = DMAX1 ( YMAX, BYMAX )

            ZMIN = DMIN1 ( ZMIN, BZMIN )
            ZMAX = DMAX1 ( ZMAX, BZMAX )

         END DO

         EXTENT(1) = XMIN
         EXTENT(2) = XMAX
         EXTENT(3) = YMIN
         EXTENT(4) = YMAX
         EXTENT(5) = ZMIN
         EXTENT(6) = ZMAX

!
!        Calculate the cumulative extent of all plates for
!        each degree of freedom.
!
         AVEXT = AVEXT + DABS(BXMAX-BXMIN) + &
                         DABS(BYMAX-BYMIN) + &
                         DABS(BZMAX-BZMIN)

      END DO

!
!     Calculate the average extent of all plates for
!     and the voxel size, i.e the length of one side
!     of a voxel cube.
!
      AVEXT  = AVEXT / DBLE(3*NP)
      VOXSIZ = VOXSCL * AVEXT
      MDLTOL = VOXSIZ * TOL

!
!     Produce a set of vertex extents, extended by MDLTOL,
!     to be used later.
!
      DO I = 1, 5, 2

         XEXTNT(I)   = EXTENT(I)   - MDLTOL
         XEXTNT(I+1) = EXTENT(I+1) + MDLTOL

      END DO

!
!     Determine the size of the coarse voxels.
!
      CVXSIZ = VOXSIZ * CGSCAL

!
!     Determine the minima and maxima of the body centered
!     vertex coordinates expressed in coarse voxel units. Scale the
!     vertices coord values by CVXSIZ: this scales the
!     axis in the voxel model space producing cubic voxels
!     with length 1 along each edge in voxel space,
!     CVXSIZ along an edge in model space.
!
      XVMIN = XMIN / CVXSIZ
      YVMIN = YMIN / CVXSIZ
      ZVMIN = ZMIN / CVXSIZ
      XVMAX = XMAX / CVXSIZ
      YVMAX = YMAX / CVXSIZ
      ZVMAX = ZMAX / CVXSIZ

!
!     Extend the coarse voxel grid by at least 1/2
!     coarse voxel length along each degree of freedom.
!
      XVMIN = DNINT ( XVMIN - 1.D0 )
      YVMIN = DNINT ( YVMIN - 1.D0 )
      ZVMIN = DNINT ( ZVMIN - 1.D0 )
      XVMAX = DNINT ( XVMAX + 1.D0 )
      YVMAX = DNINT ( YVMAX + 1.D0 )
      ZVMAX = DNINT ( ZVMAX + 1.D0 )

!
!     Calculate the coarse voxel grid origin in model units.
!
      VOXORI(1) = XVMIN * CVXSIZ
      VOXORI(2) = YVMIN * CVXSIZ
      VOXORI(3) = ZVMIN * CVXSIZ

!
!     Calculate the dimension of the voxel grid in
!     units of (regular) voxels.
!
      NX = NINT( XVMAX - XVMIN ) * CGSCAL
      NY = NINT( YVMAX - YVMIN ) * CGSCAL
      NZ = NINT( ZVMAX - ZVMIN ) * CGSCAL

      NVOX(1) = NX
      NVOX(2) = NY
      NVOX(3) = NZ

      NVXTOT = NX*NY*NZ
!
!     Make sure the number of voxels NVXTOT is within range.
!
      IF ( NVXTOT .GT. MAXVOX ) THEN

         CALL SETMSG ( 'Fine voxel count NVXTOT = #; count must ' &
         //            'be in the range 1:#.'                    )
         CALL ERRINT ( '#',  NVXTOT                              )
         CALL ERRINT ( '#',  MAXVOX                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                  )
         CALL CHKOUT ( 'ZZMKSPIN'                                )
         RETURN

      END IF


!
!     Check the coarse voxel scale. It must be at least 1, and its
!     cube must not exceed the fine voxel count.
!
      IF (      ( CGSCAL .LT. 1                ) &
           .OR. ( CGSCAL .GT. NVXTOT**(1.D0/3) )  ) THEN

         CALL SETMSG ( 'Coarse voxel scale = #; scale must ' &
         //            'be in the range 1:NVXTOT**3, where ' &
         //            'NVXTOT is the total fine voxel count. ' &
         //            'In this case, NVXTOT = #.'               )
         CALL ERRINT ( '#',  CGSCAL                              )
         CALL ERRINT ( '#',  NVXTOT                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                  )
         CALL CHKOUT ( 'ZZMKSPIN'                                )
         RETURN

      END IF

!
!     The cube of the coarse scale must divide the total voxel count
!     evenly. This is a consistency check: the code that derives the
!     voxel grid dimensions should ensure this condition is met.
!
      Q = NVXTOT /   (CGSCAL**3)
      R = NVXTOT - Q*(CGSCAL**3)

      IF ( R .NE. 0 ) THEN

         CALL SETMSG ( 'Coarse voxel scale = #; the cube of ' &
         //            'the scale must divide NVXTOT evenly, ' &
         //            'where NVXTOT is the total  fine voxel ' &
         //            'count. In this case, NVXTOT = #.'        )
         CALL ERRINT ( '#',  CGSCAL                              )
         CALL ERRINT ( '#',  NVXTOT                              )
         CALL SIGERR ( 'SPICE(INCOMPATIBLESCALE)'                )
         CALL CHKOUT ( 'ZZMKSPIN'                                )
         RETURN

      END IF

!
!     Check the number of coarse voxels.
!
      NPCG   = CGSCAL ** 3

      NCGFLG = NVXTOT / NPCG

      IF ( NCGFLG .GT. MAXCGR ) THEN

         CALL SETMSG ( 'Number of coarse voxels # exceeds ' &
         //            'limit #. Increase coarse voxel scale, ' &
         //            'fine voxel scale, or both.'            )
         CALL ERRINT ( '#', NCGFLG                             )
         CALL ERRINT ( '#', MAXCGR                             )
         CALL SIGERR ( 'SPICE(COARSEGRIDOVERFLOW)'             )
         CALL CHKOUT ( 'ZZMKSPIN'                              )
         RETURN

      END IF

!
!     Enumerate all voxels that each plate might intersect.
!
      CALL ZZINILNK ( MAXPTR, MXCELL, NCELL, VXPTR, CELLS )

!
!     Set the dimensions of the coarse grid.
!
      CGRDIM(1) = NX / CGSCAL
      CGRDIM(2) = NY / CGSCAL
      CGRDIM(3) = NZ / CGSCAL

      CALL CLEARI ( NCGFLG, CGRPTR )


!
!     TO points to the first free location in the VXPTR array.
!
      TO = 1

      DO I = 1, NP
!
!        Find the extents of the Ith plate, where the extents
!        are expanded by TOL in each direction. We truncate
!        the expanded box at a distance of MDLTOL beyond the
!        extents of the vertex set, if necessary.
!
         XP(1) = VRTCES( 1, PLATES(1,I) )
         XP(2) = VRTCES( 1, PLATES(2,I) )
         XP(3) = VRTCES( 1, PLATES(3,I) )

         YP(1) = VRTCES( 2, PLATES(1,I) )
         YP(2) = VRTCES( 2, PLATES(2,I) )
         YP(3) = VRTCES( 2, PLATES(3,I) )

         ZP(1) = VRTCES( 3, PLATES(1,I) )
         ZP(2) = VRTCES( 3, PLATES(2,I) )
         ZP(3) = VRTCES( 3, PLATES(3,I) )



         BXMIN = BRCKTD( MIN( XP(1), XP(2), XP(3) ) - MDLTOL, &
                         XEXTNT(1),  XEXTNT(2)                )

         BXMAX = BRCKTD( MAX( XP(1), XP(2), XP(3) ) + MDLTOL, &
                         XEXTNT(1),  XEXTNT(2)                )

         BYMIN = BRCKTD( MIN( YP(1), YP(2), YP(3) ) - MDLTOL, &
                         XEXTNT(3),  XEXTNT(4)                )

         BYMAX = BRCKTD( MAX( YP(1), YP(2), YP(3) ) + MDLTOL, &
                         XEXTNT(3),  XEXTNT(4)                )

         BZMIN = BRCKTD( MIN( ZP(1), ZP(2), ZP(3) ) - MDLTOL, &
                         XEXTNT(5),  XEXTNT(6)                )

         BZMAX = BRCKTD( MAX( ZP(1), ZP(2), ZP(3) ) + MDLTOL, &
                         XEXTNT(5),  XEXTNT(6)                )

!
!        Find the range of voxel coordinates that contain the bounding
!        box of the plate. All we need look at are the coordinates
!        of the two corners having minimum and maximum coordinates.
!
!        Start with the corner having minimum coordinates:
!
         CALL VPACK ( BXMIN, BYMIN, BZMIN, VMOD )

         CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, VMOD, INBOX, VCOORD )

         IF ( .NOT. INBOX ) THEN
!
!           A corner of the bounding box lies outside the voxel grid.
!           This should never occur.
!
            CALL SETMSG ( 'BUG: bounding box of plate is outside of ' &
            //            'voxel grid. Input coordinates were ' &
            //            '(#, #, #). Plate ID = #.'                  )
            CALL ERRDP  ( '#', VMOD(1)                                )
            CALL ERRDP  ( '#', VMOD(2)                                )
            CALL ERRDP  ( '#', VMOD(3)                                )
            CALL ERRINT ( '#', I                                      )
            CALL SIGERR ( 'SPICE(BUG)'                                )
            CALL CHKOUT ( 'ZZMKSPIN'                                  )
            RETURN

         END IF

!
!        Unpack minimum voxel coordinates from VCOORD.
!
         GXMIN = VCOORD(1)
         GYMIN = VCOORD(2)
         GZMIN = VCOORD(3)

!
!        Now handle the corner having maximum coordinates:
!
         CALL VPACK ( BXMAX, BYMAX, BZMAX, VMOD )

         CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, VMOD, INBOX, VCOORD )

         IF ( .NOT. INBOX ) THEN
!
!           A corner of the bounding box lies outside the voxel grid.
!           This should never occur.
!
            CALL SETMSG ( 'BUG: bounding box of plate is outside of ' &
            //            'voxel grid. Input coordinates were ' &
            //            '(#, #, #). Plate ID = #.'                  )
            CALL ERRDP  ( '#', VMOD(1)                                )
            CALL ERRDP  ( '#', VMOD(2)                                )
            CALL ERRDP  ( '#', VMOD(3)                                )
            CALL ERRINT ( '#', I                                      )
            CALL SIGERR ( 'SPICE(BUG)'                                )
            CALL CHKOUT ( 'ZZMKSPIN'                                  )
            RETURN

         END IF

!
!        Unpack maximum voxel coordinates from VCOORD.
!
         GXMAX = VCOORD(1)
         GYMAX = VCOORD(2)
         GZMAX = VCOORD(3)

!
!        Determine voxels that the bounding box of the plate
!        intersects.
!
         DO IZ = GZMIN, GZMAX

             DO IY = GYMIN, GYMAX

                 DO IX = GXMIN, GXMAX

                    VIXYZ(1) = IX
                    VIXYZ(2) = IY
                    VIXYZ(3) = IZ
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
                    CALL ZZVOXCVO ( VIXYZ, NVOX,  CGSCAL, &
                                    CGXYZ, CGOFF, CGOF1D  )

                    IF ( FAILED() ) THEN
                       CALL CHKOUT ( 'ZZMKSPIN' )
                       RETURN
                    END IF

                    CVID = ZZVOX2ID ( CGXYZ, CGRDIM )

                    IF ( CGRPTR(CVID) .EQ. 0 ) THEN
!
!                      The coarse voxel at index CVID is empty so far.
!                      Allocate CGSCAL pointers for it in the VXPTR
!                      array; make the coarse voxel point to the first
!                      element of this sub-array.
!
                       CGRPTR(CVID) = TO
                       TO           = TO + NPCG

                    END IF
!
!                   Let IXPTR be the index in the VXPTR array of the
!                   pointer for the current voxel.
!
                    IXPTR = ( CGRPTR(CVID) - 1 )  +  CGOF1D

                    CALL ZZADDLNK ( IXPTR,  I,     MAXPTR, &
                                    MXCELL, VXPTR, NCELL, CELLS )

                    IF ( FAILED() ) THEN
                       CALL CHKOUT ( 'ZZMKSPIN' )
                       RETURN
                    END IF


                 END DO

              END DO

          END DO

      END DO

      NVXPTR = TO - 1

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
      CALL ZZUNTNGL ( NVXPTR, MXCELL, CELLS, &
                      MAXVXL, VXPTR,  NVXLST, VXLIST )


      CALL CHKOUT ( 'ZZMKSPIN' )
      RETURN
      END
