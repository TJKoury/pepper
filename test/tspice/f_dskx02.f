C$Procedure F_DSKX02 ( DSKX02 tests )

      SUBROUTINE F_DSKX02 ( OK )

C$ Abstract
C
C     Exercise the DSK type 2 ray-surface intercept routine DSKX02.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsk02.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the DSKLIB type 2 ray-surface intercept
C     routine DSKX02.
C
C        Currently tests are limited to a subset of the
C        routine's logic dealing with competing intercept
C        solutions.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 14-NOV-2020 (NJB)
C
C        Added test case for out-of-segment solution bug fix.
C
C-    TSPICE Version 1.0.0, 09-SEP-2014 (NJB)
C
C        Updated to work with SPICELIB version of DSKW02.
C
C     09-SEP-2014 (NJB)
C
C        Corrected pointer entries in spatial index
C        for voxels 3 and 4. Corrected null pointer values
C        in coarse voxel grid.
C
C-    20-JUN-2014 (NJB)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      PI

      LOGICAL               EXISTS

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               MXNVOX
      PARAMETER           ( MXNVOX = 2100 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 3000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2000 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               MAXIXI
      PARAMETER           ( MAXIXI = MAXCGR + 10 * MAXP )

C
C     Number of supported coordinate systems.
C
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 3 )

C
C     Workspace and index sizes
C
      INTEGER               WORKSZ
      PARAMETER           ( WORKSZ = MAXP * 100 )

C
C     Local Variables
C
      CHARACTER*(FILSIZ)    DSK
      CHARACTER*(FILSIZ)    DSK1
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      FINSCL
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      MNCOR1
      DOUBLE PRECISION      MNCOR2
      DOUBLE PRECISION      MNCOR3
      DOUBLE PRECISION      MXCOR1
      DOUBLE PRECISION      MXCOR2
      DOUBLE PRECISION      MXCOR3
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      SPAIXD ( IXDFIX )
      DOUBLE PRECISION      VERTS  ( 3, MAXV )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VOXSIZ
      DOUBLE PRECISION      VTXBDS ( 2, 3 )
      DOUBLE PRECISION      XOFF
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )
      DOUBLE PRECISION      ZOFF

      INTEGER               CENTER
      INTEGER               CGRPTR ( MAXCGR )
      INTEGER               CGRSCL
      INTEGER               CORSCL
      INTEGER               CORSYS
      INTEGER               SYSTMS ( NSYS )
      INTEGER               DCLASS
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HAN1
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NCVOX
      INTEGER               NP
      INTEGER               NV
      INTEGER               NVOX
      INTEGER               PLATE  ( 3 )
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               PLID
      INTEGER               PRVDSC ( DLADSZ )
      INTEGER               SEGNO
      INTEGER               SPAIXI ( MAXIXI )
      INTEGER               SURFCE
      INTEGER               TO
      INTEGER               VGREXT ( 3 )
      INTEGER               VI
      INTEGER               VOXLSZ
      INTEGER               VOXNPL
      INTEGER               VOXNPT
      INTEGER               VOXPLT ( MXNVOX )
      INTEGER               VOXPTR ( MXNVOX )
      INTEGER               VOXPSZ
      INTEGER               WORK   ( 2, WORKSZ )
      INTEGER               XPLATE ( 3 )
      INTEGER               XPLID

      LOGICAL               FOUND

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C
      DATA SYSTMS           / LATSYS, RECSYS, PDTSYS /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_DSKX02' )



C**********************************************************************
C
C     Normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create DSK containing type 2 segment' )

C
C     We're going to create a plate model and associated spatial
C     index data structures entirely by hand.
C
C     The voxel grid has voxel extents
C
C        4 x 4 x 4
C
C     The coarse voxel scale is 2.
C
C     The edge length of each voxel is 10 km.
C
      DO I = 1, 3
         VGREXT(I) = 4
      END DO

      CGRSCL    = 2
      VOXSIZ    = 10.D0

      NCVOX     = 8

C
C     The voxel grid is centered at the reference frame center.
C
      DO I = 1, 3
         ORIGIN(I) = -VGREXT(I) * VOXSIZ / 2
      END DO

C
C     Set up plates and vertices:
C
C        Voxel (1,1,1) contains a plate facing in the -X direction:
C
      VERTS(1,1) = -19.D0
      VERTS(2,1) = -18.D0
      VERTS(3,1) = -19.D0

      VERTS(1,2) = -19.D0
      VERTS(2,2) = -19.D0
      VERTS(3,2) = -19.D0

      VERTS(1,3) = -19.D0
      VERTS(2,3) = -18.5D0
      VERTS(3,3) = -18.D0

      PLATES(1,1) = 1
      PLATES(2,1) = 2
      PLATES(3,1) = 3

C
C     Add a plate that should be occulted by plate 1, as seen from
C     the -X direction.
C
C     Plate 2 is a copy of plate 1, shifted by 1 in the +X direction.
C
C     Vertices 4-6 define this plate.
C
      DO I = 1, 3

         VERTS(1,I+3) = VERTS(1,I) + 1.D0

         DO J = 2, 3
            VERTS(J,I+3) = VERTS(J,I)
         END DO

      END DO

      PLATES(1,2) = 4
      PLATES(2,2) = 5
      PLATES(3,2) = 6

C
C     Add a plate that normally should occult plate 1, as seen from
C     the -X direction. However, this plate will be "owned" by voxel 2,
C     which is voxel 1's neighbor on the +X side. Thus this plate should
C     be examined as a target only if plates 1 and 2 are missed.
C
C
C     Plate 3 is a copy of plate 1, shifted by 1/2 in the -X direction.
C
C     Vertices 7-9 define this plate.
C
      DO I = 1, 3

         VERTS(1,I+6) = VERTS(1,I) - 0.5D0

         DO J = 2, 3
            VERTS(J,I+6) = VERTS(J,I)
         END DO

      END DO

      PLATES(1,3) = 7
      PLATES(2,3) = 8
      PLATES(3,3) = 9

C
C     Add a plate that normally should occult plate 1, as seen from
C     the -X direction. However, this plate has its vertex orientation
C     reversed.
C
C     Vertices 10-12 define this plate.
C
      VERTS(1,10) = VERTS(1,1 ) - 0.75D0
      VERTS(1,11) = VERTS(1,10)
      VERTS(1,12) = VERTS(1,10)

C
C     The Y component of vertex 10 is that of vertex 2;
C     the Y component of vertex 11 is that of vertex 1.
C
      VERTS(2,10) = VERTS(2,2)
      VERTS(2,11) = VERTS(2,1)
      VERTS(2,12) = VERTS(2,3)

      VERTS(3,10) = VERTS(3,1)
      VERTS(3,11) = VERTS(3,2)
      VERTS(3,12) = VERTS(3,3)

      PLATES(1,4) = 10
      PLATES(2,4) = 11
      PLATES(3,4) = 12

C
C     Add a plate on the opposite side of the grid from plate 1,
C     but also facing in the same direction as plate one. Offset
C     this plate in the +Z direction by 1 km.
C
C     This plate is owned by voxel 4, which is contained in
C     coarse voxel 2.
C
      DO I = 1, 3

         VERTS(1,I+12) = VERTS(1,1) +  3 * VOXSIZ

         VERTS(2,I+12) = VERTS(2,I)

         VERTS(3,I+12) = VERTS(3,I) + 1.D0

      END DO


      PLATES(1,5) = 13
      PLATES(2,5) = 14
      PLATES(3,5) = 15

      NV = 15
      NP = 5

C
C     Compute vertex extents.
C
      DO I = 1, 3
         VTXBDS(1,I) = DPMAX()
         VTXBDS(2,I) = DPMIN()
      END DO

      DO I = 1, NV

         DO J = 1, 3

            VTXBDS(1,J) = MIN( VTXBDS(1,J), VERTS(J,I) )
            VTXBDS(2,J) = MAX( VTXBDS(2,J), VERTS(J,I) )

         END DO

      END DO

C
C     Set the voxel-plate mapping.
C
      CALL CLEARI ( MXNVOX, VOXPLT )

C
C     Initialize all pointers to null.
C
      CALL FILLI ( -1, MXNVOX, VOXPTR )
C
C     The voxel pointer list must contain an entry for
C     each voxel belonging to a non-empty coarse voxel.
C     Coarse voxels 1 and 2 are non-empty.
C
      VOXNPT = 16

C
C     The first voxel contains plates 1 and 2. The pointer
C     associated with this voxel points to the start of the
C     voxel's plate list.
C
      VOXPTR(1) = 1

C
C     The second voxel contains plate 3. The pointer
C     associated with this voxel points to the start of the
C     voxel's plate list, which follows the list for voxel 1.
C
      VOXPTR(2) = 5

C
C     The fourth voxel contains plate 5. The pointer
C     associated with this voxel points to the start of the
C     voxel's plate list, which follows the list for voxel 2.
C     Note that the voxel pointer list contains 6 null entries
C     following the entry for voxel 2, since there are entries
C     for every voxel in the coarse voxel containing voxel 1.
C
      VOXPTR(10) = 7

C
C     The first element of a plate list is the count.
C     The plate numbers follow.
C
C     Entries for voxel 1:
C
      VOXPLT(1) = 3
      VOXPLT(2) = 1
      VOXPLT(3) = 2
      VOXPLT(4) = 4
C
C     Entries for voxel 2:
C
      VOXPLT(5) = 1
      VOXPLT(6) = 3

C
C     Entries for voxel 4:
C
      VOXPLT(7) = 1
      VOXPLT(8) = 5

C
C     VOXNPL is the number of elements in the plate list, including
C     plate counts.
C
      VOXNPL    = 8

C
C     The first coarse voxel is non-empty and points
C     to the voxel pointer for the first voxel.
C
C     The second coarse voxel is non-empty and points
C     to the voxel pointer for the fourth voxel.
C
C     The rest of the coarse grid contains null values.
C
      CALL FILLI ( 0, NCVOX, CGRPTR )

      CGRPTR(1) = 1
      CGRPTR(2) = 9

C
C     We will not use the vertex-plate list.
C

C
C     Pack the integer component of the spatial index array.
C
      CALL MOVEI ( VGREXT, 3, SPAIXI(SIVGRX) )

      SPAIXI(SICGSC) = CGRSCL
      SPAIXI(SIVXNP) = VOXNPT
      SPAIXI(SIVXNL) = VOXNPL
      SPAIXI(SIVTNL) = 0

      CALL MOVEI ( CGRPTR, MAXCGR, SPAIXI(SICGRD) )

      I = SICGRD + MAXCGR

      CALL MOVEI ( VOXPTR, VOXNPT, SPAIXI(I) )

      I = I + VOXNPT

      CALL MOVEI ( VOXPLT, VOXNPL, SPAIXI(I) )

C
C     Pack the d.p. component of the spatial index array.
C
      CALL MOVED ( VTXBDS, 6, SPAIXD(SIVTBD) )
      CALL VEQU  ( ORIGIN,    SPAIXD(SIVXOR) )

      SPAIXD(SIVXSZ) = VOXSIZ

C
C     Set up ID and coverage parameters for the segment. These
C     are required in order to create a DSK segment but have no
C     other use for the tests performed here.
C
      SURFCE = 499
      CENTER = 4
      DCLASS = 2
      FRAME  = 'IAU_MARS'
      CORSYS = RECSYS
      MNCOR1 = VTXBDS(1,1)
      MXCOR1 = VTXBDS(2,1)
      MNCOR2 = VTXBDS(1,2)
      MXCOR2 = VTXBDS(2,2)
      MNCOR3 = VTXBDS(1,3)
      MXCOR3 = VTXBDS(2,3)

      FIRST  = -1.D0
      LAST   =  1.D0

C
C     Open a new DSK file.
C
      DSK = 'dskx02.bds'

      IF ( EXISTS(DSK) ) THEN
         CALL DELFIL( DSK )
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF


      CALL DSKOPN ( DSK, DSK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Write the segment.
C
      CALL DSKW02 ( HANDLE,
     .              CENTER, SURFCE, DCLASS, FRAME,  CORSYS,
     .              CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2,
     .              MNCOR3, MXCOR3, FIRST,  LAST,   NV,
     .              VERTS,  NP,     PLATES, SPAIXD, SPAIXI )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKCLS ( HANDLE, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Find ray intercept on plate 1; ray vertex '
     .//           'is on the -X side.'                        )


      CALL DASOPR ( DSK,    HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray vertex is on the -X side of the grid, and the ray
C     points strictly in the +X direction.
C
      VERTEX(1) = -3*VOXSIZ
      VERTEX(2) =  ( VERTS(2,1) + VERTS(2,2) ) / 2
      VERTEX(3) =  ( VERTS(3,1) + VERTS(3,3) ) / 2

      RAYDIR(1) =  1.D0
      RAYDIR(2) =  0.D0
      RAYDIR(3) =  0.D0

      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'PLID', PLID, '=', 1, 0, OK )

         CALL VEQU ( VERTEX, XXPT )
         XXPT(1) = VERTS(1,1)

         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, VTIGHT, OK )

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Find ray intercept on plate 4; ray vertex '
     .//           'is on the +X side.'                        )

C
C     The ray vertex is on the +X side of the grid, and the ray
C     points strictly in the -X direction.
C
C     Plates other than plate 4 should be invisible due to their
C     vertex orientation.
C
C
      VERTEX(1) =  3*VOXSIZ
      VERTEX(2) =  ( VERTS(2,1) + VERTS(2,2) ) / 2
      VERTEX(3) =  ( VERTS(3,1) + VERTS(3,3) ) / 2

      RAYDIR(1) = -1.D0
      RAYDIR(2) =  0.D0
      RAYDIR(3) =  0.D0

      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'PLID', PLID, '=', 4, 0, OK )

         CALL VEQU ( VERTEX, XXPT )
         XXPT(1) = VERTS(1,10)

         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, VTIGHT, OK )

      END IF


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Find ray intercept on plate 5; ray vertex '
     .//           'is on the -X side.'                        )

C
C     This case requires examination of the second group of
C     voxels within the DSKX02 loop that works with one voxel
C     group at a time.
C
C     The ray vertex is on the -X side of the grid, and the ray
C     points strictly in the +X direction.
C
      VERTEX(1) = -3*VOXSIZ
      VERTEX(2) =  ( VERTS(2,13) + VERTS(2,14) ) / 2
      VERTEX(3) =  ( VERTS(3,13) + VERTS(3,15) ) / 2

      RAYDIR(1) =  1.D0
      RAYDIR(2) =  0.D0
      RAYDIR(3) =  0.D0

      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'PLID', PLID, '=', 5, 0, OK )

         CALL VEQU ( VERTEX, XXPT )
         XXPT(1) = VERTS(1,13)

         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, VTIGHT, OK )

      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Shoot a ray through empty voxels in coarse '
     .//           'voxel 1.'                                   )

C
C     Voxels above the bottom layer are empty. In coarse voxel 1,
C     the voxel IDs of the upper layer are
C
C        17, 18, 21, 22
C
C
C
      VERTEX(1) = -2.5D0 * VOXSIZ
      VERTEX(2) = -0.5D0 * VOXSIZ
      VERTEX(3) = -0.5D0 * VOXSIZ

      RAYDIR(1) =  1.D0
      RAYDIR(2) = -1.D0
      RAYDIR(3) =  0.D0


      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Shoot a ray through empty coarse voxels on the '
     .//           'top layer of the grid.'                          )

      VERTEX(1) = -3     * VOXSIZ
      VERTEX(2) =  0.5D0 * VOXSIZ
      VERTEX(3) =  1.5D0 * VOXSIZ

      RAYDIR(1) =  1.D0
      RAYDIR(2) =  0.D0
      RAYDIR(3) =  0.D0


      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )




C
C --- Case: ------------------------------------------------------
C

C
C     Add a segment for testing handling of duplicate plates.
C
      CALL TCASE ( 'Add second segment to DSK.' )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPW ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The voxel grid has voxel extents
C
C        100 x 1 x 1
C
C     The coarse voxel scale is 1.
C
C     The edge length of each voxel is 10 km.
C
      VGREXT(1) = 100
      VGREXT(2) =   1
      VGREXT(3) =   1

      CGRSCL    = 1
      VOXSIZ    = 10.D0

      NCVOX     = 100

C
C     The voxel grid origin coincides with the reference frame center.
C
      CALL CLEARD ( 3, ORIGIN )

C
C     Voxel 1 contains 10 plates.
C
C     Each other voxel "owns" the plates in the voxel behind it in the
C     -X direction, plus its own plates. The plates physically
C     contained in voxel `n' are shifted in the +Z direction by (n-1)
C     km relative to the plate set in voxel 1.
C

C
C     Create the vertices and plates for the first voxel.
C
C     The vertices are orders to as to make the normal vectors point
C     in the -X direction.
C
      DO I = 1, 10

         J = 3*(I-1)

         VERTS(1,J+1) =       5.D0
         VERTS(2,J+1) =     DBLE(I)
         VERTS(3,J+1) =       0.D0

         VERTS(1,J+2) =       5.D0
         VERTS(2,J+2) =  DBLE(I-1)
         VERTS(3,J+2) =       0.D0

         VERTS(1,J+3) =       5.D0
         VERTS(2,J+3) =   I - 0.5D0
         VERTS(3,J+3) =       1.D0

         DO K = 1, 3
            PLATES(K,I) = J+K
         END DO

      END DO

      NV = 30
      NP = 10

C
C     Set up the voxel-plate mapping.
C
      VOXNPT = 1

      VOXPTR(1) = 1

      VOXPLT(1) = 10

      DO I = 1, 10
         VOXPLT(1+I) = I
      END DO

      VOXNPL = 11

      TO     = VOXNPL + 1

C
C     Set the coarse grid pointers.
C
      CALL FILLI ( -1, NCVOX, CGRPTR )

      CGRPTR(1) = 1

C
C     Create vertices, plates, and spatial index entries for
C     the other voxels.
C
      NVOX = VGREXT(1)

      DO VI = 2, NVOX
C
C        Create vertices and plates for the current voxel.
C
         DO I = 1, 10

            J = 3*(NP+I-1)

            XOFF = (VI - 1) * VOXSIZ
            ZOFF = (VI - 1) * ( (VOXSIZ-1) / NVOX )

            DO K = 1, 3

               VERTS(1,J+K) = VERTS(1,1) + XOFF
               VERTS(2,J+K) = VERTS( 2, K + 3*(I-1) )
               VERTS(3,J+K) = VERTS(3,K) + ZOFF

            END DO

            DO K = 1, 3
               PLATES( K, NP+I ) = J+K
            END DO

         END DO


C
C        Set the coarse grid pointer for the current coarse voxel.
C
         CGRPTR(VI) = VI

C
C        Set the voxel pointer for the current voxel.
C
         VOXPTR(VI) = TO

C
C        Fill in the plate list for the current voxel.
C
         VOXPLT(TO) = 20

C
C        Add the new plates to the plate list.
C
         DO I = 1, 10
            VOXPLT(TO+I) = NP + I
         END DO

C
C        Add the plates from the preceding voxel to the plate list.
C
         DO I = 1, 10
            VOXPLT( TO + 10 + I ) = ( NP - 10 ) + I
         END DO


         NP     = NP     + 10
         NV     = NV     + 30

         VOXNPL = VOXNPL + 21
         TO     = VOXNPL + 1

      END DO


      VOXNPT = NVOX

C
C     We will not use the vertex-plate list.
C

C
C     Compute vertex extents.
C
      DO I = 1, 3
         VTXBDS(1,I) = DPMAX()
         VTXBDS(2,I) = DPMIN()
      END DO

      DO I = 1, NV

         DO J = 1, 3

            VTXBDS(1,J) = MIN( VTXBDS(1,J), VERTS(J,I) )
            VTXBDS(2,J) = MAX( VTXBDS(2,J), VERTS(J,I) )

         END DO

      END DO


C
C     Pack the integer component of the spatial index array.
C
      CALL MOVEI ( VGREXT, 3, SPAIXI(SIVGRX) )

      SPAIXI(SICGSC) = CGRSCL
      SPAIXI(SIVXNP) = VOXNPT
      SPAIXI(SIVXNL) = VOXNPL
      SPAIXI(SIVTNL) = 0

      CALL MOVEI ( CGRPTR, MAXCGR, SPAIXI(SICGRD) )

      I = SICGRD + MAXCGR

      CALL MOVEI ( VOXPTR, VOXNPT, SPAIXI(I) )

      I = I + VOXNPT

      CALL MOVEI ( VOXPLT, VOXNPL, SPAIXI(I) )

C
C     Pack the d.p. component of the spatial index array.
C
      CALL MOVED ( VTXBDS, 6, SPAIXD(SIVTBD) )
      CALL VEQU  ( ORIGIN,    SPAIXD(SIVXOR) )

      SPAIXD(SIVXSZ) = VOXSIZ

C


C
C     Set up ID and coverage parameters for the segment. These
C     are required in order to create a DSK segment but have no
C     other use for the tests performed here.
C
      SURFCE = 599
      CENTER = 5
      DCLASS = 2
      FRAME  = 'IAU_JUPITER'
      CORSYS =  RECSYS
      MNCOR1 =       0.D0
      MXCOR1 =  VGREXT(1) * VOXSIZ
      MNCOR2 =       0.D0
      MXCOR2 =  VGREXT(2) * VOXSIZ
      MNCOR3 =       0.D0
      MXCOR3 =  VGREXT(3) * VOXSIZ

      FIRST  = -1.D0
      LAST   =  1.D0

C
C     Write the segment.
C
      CALL DSKW02 ( HANDLE,
     .              CENTER, SURFCE, DCLASS, FRAME,  CORSYS,
     .              CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2,
     .              MNCOR3, MXCOR3, FIRST,  LAST,   NV,
     .              VERTS,  NP,     PLATES, SPAIXD, SPAIXI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKCLS ( HANDLE, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Find ray-plate intercepts in the second segment.' )

      CALL DASOPR ( DSK,    HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MOVEI  ( DLADSC, DLADSZ, PRVDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLAFNS ( HANDLE, PRVDSC, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


      DO VI = 1, NVOX

         DO I = 1, 10

            TITLE = 'Intercept on plate # of voxel #.'
            CALL REPMI ( TITLE, '#', I,  TITLE )
            CALL REPMI ( TITLE, '#', VI, TITLE )

C
C --- Case: ------------------------------------------------------
C
            CALL TCASE (TITLE)

            VERTEX(1) =      - 1.0D0
            VERTEX(2) =    I - 0.5D0
            VERTEX(3) =        0.95D0 + (VI-1)*0.09D0

            RAYDIR(1) =   1.D0
            RAYDIR(2) =   0.D0
            RAYDIR(2) =   0.D0


            CALL DSKX02 ( HANDLE, DLADSC, VERTEX,
     .                    RAYDIR, PLID,   XPT,   FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( .NOT. OK )   STOP

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

            IF ( FOUND ) THEN

               XPLID = (VI-1)*10 + I

               CALL CHCKSI ( 'PLID', PLID, '=', XPLID, 0, OK )

               CALL VEQU ( VERTEX, XXPT )
               XXPT(1) = VERTS(1,1) + (VI-1)*VOXSIZ

               CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, VTIGHT, OK )

            END IF


         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C

C
C     This case tests the fix for the bug in which an out-of-segment
C     intercept can be returned, even though the returned plate ID is
C     for a valid, in-segment solution. The original bug fix was
C     written in April 2017. The bug itself was introduced that same
C     month, slightly before the N0066 delivery.
C
      CALL TCASE ( 'Test out-of-segment intercept bug fix' )

C
C     We'll create a DSK that contains six segments. For each
C     coordinate system, there will be two segments, each containing
C     two plates.
C
C     For each coordinate system, in one of the segments associated
C     with that coordinate system, the first plate will lie within the
C     segment bounds, and the second plate will be outside the segment
C     bounds. In the second segment, the order of the plates will be
C     reversed. These variations are needed to ensure that at least one
C     segment will satisfy the conditions required to exhibit the bug,
C     regardless of the order in which the DSK type 2 writer adds data
C     to the segment.

C     The plate set used for this test is contrived and completely
C     unrealistic, but it suffices to demonstrate the error in the
C     previous version of DSKX02 and the correction in the N0067
C     version.
C
      DSK1 = 'test_dskx02_fix.bds'

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
         CALL CHCKXC ( .FALSE., ' ', OK  )
      END IF

C
C     Open the DSK for writing.
C
      CALL DSKOPN( DSK1, DSK1, 0, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK  )

C
C     Create two plates. The plates are parallel to the Y-Z plane.
C     The plate centers are on the positive X axis. Vertices of the
C     first plate are
C
C         ( 10, -1, -1 )
C         ( 10,  1, -1 )
C         ( 10,  0,  1 )
C
C     Vertices of the second plate are
C
C         ( 20, -1, -1 )
C         ( 20,  1, -1 )
C         ( 20,  0,  1 )
C
      VERTS( 1, 1 )  =   1.D1
      VERTS( 2, 1 )  =  -1.D0
      VERTS( 3, 1 )  =  -1.D0

      VERTS( 1, 2 )  =   1.D1
      VERTS( 2, 2 )  =   2.D0
      VERTS( 3, 2 )  =  -1.D0

      VERTS( 1, 3 )  =   1.D1
      VERTS( 2, 3 )  =   0.D0
      VERTS( 3, 3 )  =   1.D0

      PLATES( 1, 1 ) = 1
      PLATES( 2, 1 ) = 2
      PLATES( 3, 1 ) = 3


      VERTS( 1, 4 )  =   2.D1
      VERTS( 2, 4 )  =  -1.D0
      VERTS( 3, 4 )  =  -1.D0

      VERTS( 1, 5 )  =   2.D1
      VERTS( 2, 5 )  =   2.D0
      VERTS( 3, 5 )  =  -1.D0

      VERTS( 1, 6 )  =   2.D1
      VERTS( 2, 6 )  =   0.D0
      VERTS( 3, 6 )  =   1.D0

      PLATES( 1, 2 ) = 4
      PLATES( 2, 2 ) = 5
      PLATES( 3, 2 ) = 6

      NV = 6
      NP = 2


      DO I = 1, NSYS
C
C        For each coordinate system, create segment bounds that contain
C        one segment and exclude the other.
C
         IF ( SYSTMS(I) .EQ. LATSYS ) THEN
C
C           Latitudinal bounds are for longitude, latitude, and radius,
C           in that order.
C
            MNCOR1 = -PI()
            MXCOR1 =  PI()
            MNCOR2 = -PI()/2
            MXCOR2 =  PI()/2
            MNCOR3 =   0.D0
            MXCOR3 =  15.D0
C
C           There are no coordinate system parameters.
C
            CALL CLEARD ( NSYPAR, CORPAR )
            CALL CHCKXC ( .FALSE., ' ', OK  )

         ELSE IF ( SYSTMS(I) .EQ. RECSYS ) THEN

            MNCOR1 =  0.D0
            MXCOR1 = 15.D0
            MNCOR2 = -2.D0
            MXCOR2 =  2.D0
            MNCOR3 = -2.D0
            MXCOR3 =  2.D0
C
C           There are no coordinate system parameters.
C
            CALL CLEARD ( NSYPAR, CORPAR )
            CALL CHCKXC ( .FALSE., ' ', OK  )

         ELSE IF ( SYSTMS(I) .EQ. PDTSYS ) THEN
C
C           The coordinate system parameters for planetodetic
C           coordinates are RE and F, in that order. Choose values
C           that correspond to an equatorial radius of 10 and a polar
C           radius of 8.
C
            CORPAR(1) = 10.D0
            CORPAR(2) =  0.2D0

            CALL CLEARD ( NSYPAR-2, CORPAR(3) )
            CALL CHCKXC ( .FALSE., ' ', OK  )

C
C           Planetodetic bounds are for longitude, latitude, and
C           altitude, in that order.
C
            MNCOR1 = -PI()
            MXCOR1 =  PI()
            MNCOR2 = -PI()/2
            MXCOR2 =  PI()/2
C
C           The altitude range is from -3 to 5.
C
            MNCOR3 =  -3.D0
            MXCOR3 =   5.D0

         ELSE
            CALL SETMSG ( 'Unexpected coordinate system code: #.' )
            CALL ERRINT ( '#', SYSTMS(I)   )
            CALL SIGERR ( 'SPICE(TESTBUG)' )
            CALL CHCKXC ( .FALSE., ' ', OK  )
         END IF

         DO J = 1, 2

            IF ( J .EQ. 2 ) THEN
C
C              Swap the plate order.
C
               DO K = 1, 3
                  CALL SWAPI ( PLATES(K,1), PLATES(K,2) )
               END DO

            END IF

C
C           Set the voxel scales. We need very large voxels, since the
C           case of interest is one in which both plates are in the
C           same voxel, even though one plate is inside the segment
C           bounds and the other is not.
C
            FINSCL = 100.D0
            CORSCL = 1

C
C           Create spatial index for the segment. Choose generous
C           values for the data structure sizes. We're not calculating
C           the exact required sizes.
C
            VOXPSZ = MXNVOX
            VOXLSZ = MXNVOX

            CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, FINSCL,
     .                    CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .TRUE.,
     .                    SPAISZ, WORK,   SPAIXD, SPAIXI          )
            CALL CHCKXC ( .FALSE., ' ', OK  )

C
C           Set up ID parameters for the segment. These are required in
C           order to create a DSK segment but have no other use for the
C           tests performed here.
C
            SURFCE = 499
            CENTER = 4
            DCLASS = 2
            FRAME  = 'IAU_MARS'
            CORSYS = SYSTMS(I)
C
C           Write the segment.
C
            CALL DSKW02 ( HAN1,
     .                    CENTER, SURFCE, DCLASS, FRAME,  CORSYS,
     .                    CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2,
     .                    MNCOR3, MXCOR3, FIRST,  LAST,   NV,
     .                    VERTS,  NP,     PLATES, SPAIXD, SPAIXI )
            CALL CHCKXC ( .FALSE., ' ', OK  )


            IF ( J .EQ. 2 ) THEN
C
C              Undo the plate order swap.
C
               DO K = 1, 3
                  CALL SWAPI ( PLATES(K,1), PLATES(K,2) )
               END DO

            END IF

         END DO

      END DO
C
C     Close the DSK.
C
      CALL DSKCLS ( HAN1, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK  )

C
C     Now open the file for read access and traverse its segments.
C     For each segment, try a ray-surface intercept test.
C
      CALL DASOPR ( DSK1, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      CALL DLABFS ( HAN1, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      SEGNO = 0

      DO WHILE ( FOUND )

         SEGNO = SEGNO + 1
C
C        Create the vertex and direction vector of a ray to be used
C        in a call to DSKX02.
C
         CALL VPACK ( 100.D0, 0.D0, 0.D0, VERTEX )
         CALL VPACK ( -1.D0,  0.D0, 0.D0, RAYDIR )

C
C        Compute the ray-surface intercept. We expect the ray to hit
C        the plate in the X = 10 plane.
C
         CALL DSKX02 ( HAN1, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK  )

C
C        Make sure the intercept was found.
C
         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', SEGNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )
         CALL VPACK ( 10.D0, 0.D0, 0.D0, XXPT )

C
C        Check the intercept.
C
         LABEL = 'XPT #'
         CALL REPMI ( LABEL, '#', SEGNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, VTIGHT, OK  )

C
C        To check the plate ID, we must see whether that ID belongs
C        to the plate lying in the X = 10 plane. Look up the plate.
C
         CALL DSKP02 ( HAN1, DLADSC, PLID, 1, N, PLATE )
         CALL CHCKXC ( .FALSE., ' ', OK  )

C
C        The correct plate always has vertices 1, 2, 3.
C
         DO I = 1, 3
            XPLATE(I) = I
         END DO

         LABEL = 'PLATE #'
         CALL REPMI ( LABEL, '#', SEGNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK  )

C
C        Look up the next DLA segment.
C
         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL DLAFNS ( HAN1, PRVDSC, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK  )

      END DO

      CALL DASCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK  )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up kernels.' )


      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
