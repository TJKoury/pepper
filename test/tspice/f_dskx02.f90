!$Procedure F_DSKX02 ( DSKX02 tests )

      SUBROUTINE F_DSKX02 ( OK )

! $ Abstract
!
!     Exercise the DSK type 2 ray-surface intercept routine DSKX02.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsk02.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the DSKLIB type 2 ray-surface intercept
!     routine DSKX02.
!
!        Currently tests are limited to a subset of the
!        routine's logic dealing with competing intercept
!        solutions.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 14-NOV-2020 (NJB)
!
!        Added test case for out-of-segment solution bug fix.
!
!-    TSPICE Version 1.0.0, 09-SEP-2014 (NJB)
!
!        Updated to work with SPICELIB version of DSKW02.
!
!     09-SEP-2014 (NJB)
!
!        Corrected pointer entries in spatial index
!        for voxels 3 and 4. Corrected null pointer values
!        in coarse voxel grid.
!
!-    20-JUN-2014 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      PI

      LOGICAL               EXISTS

!
!     Local Parameters
!
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

!
!     Number of supported coordinate systems.
!
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 3 )

!
!     Workspace and index sizes
!
      INTEGER               WORKSZ
      PARAMETER           ( WORKSZ = MAXP * 100 )

!
!     Local Variables
!
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

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!
      DATA SYSTMS           / LATSYS, RECSYS, PDTSYS /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_DSKX02' )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create DSK containing type 2 segment' )

!
!     We're going to create a plate model and associated spatial
!     index data structures entirely by hand.
!
!     The voxel grid has voxel extents
!
!        4 x 4 x 4
!
!     The coarse voxel scale is 2.
!
!     The edge length of each voxel is 10 km.
!
      DO I = 1, 3
         VGREXT(I) = 4
      END DO

      CGRSCL    = 2
      VOXSIZ    = 10.D0

      NCVOX     = 8

!
!     The voxel grid is centered at the reference frame center.
!
      DO I = 1, 3
         ORIGIN(I) = -VGREXT(I) * VOXSIZ / 2
      END DO

!
!     Set up plates and vertices:
!
!        Voxel (1,1,1) contains a plate facing in the -X direction:
!
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

!
!     Add a plate that should be occulted by plate 1, as seen from
!     the -X direction.
!
!     Plate 2 is a copy of plate 1, shifted by 1 in the +X direction.
!
!     Vertices 4-6 define this plate.
!
      DO I = 1, 3

         VERTS(1,I+3) = VERTS(1,I) + 1.D0

         DO J = 2, 3
            VERTS(J,I+3) = VERTS(J,I)
         END DO

      END DO

      PLATES(1,2) = 4
      PLATES(2,2) = 5
      PLATES(3,2) = 6

!
!     Add a plate that normally should occult plate 1, as seen from
!     the -X direction. However, this plate will be "owned" by voxel 2,
!     which is voxel 1's neighbor on the +X side. Thus this plate should
!     be examined as a target only if plates 1 and 2 are missed.
!
!
!     Plate 3 is a copy of plate 1, shifted by 1/2 in the -X direction.
!
!     Vertices 7-9 define this plate.
!
      DO I = 1, 3

         VERTS(1,I+6) = VERTS(1,I) - 0.5D0

         DO J = 2, 3
            VERTS(J,I+6) = VERTS(J,I)
         END DO

      END DO

      PLATES(1,3) = 7
      PLATES(2,3) = 8
      PLATES(3,3) = 9

!
!     Add a plate that normally should occult plate 1, as seen from
!     the -X direction. However, this plate has its vertex orientation
!     reversed.
!
!     Vertices 10-12 define this plate.
!
      VERTS(1,10) = VERTS(1,1 ) - 0.75D0
      VERTS(1,11) = VERTS(1,10)
      VERTS(1,12) = VERTS(1,10)

!
!     The Y component of vertex 10 is that of vertex 2;
!     the Y component of vertex 11 is that of vertex 1.
!
      VERTS(2,10) = VERTS(2,2)
      VERTS(2,11) = VERTS(2,1)
      VERTS(2,12) = VERTS(2,3)

      VERTS(3,10) = VERTS(3,1)
      VERTS(3,11) = VERTS(3,2)
      VERTS(3,12) = VERTS(3,3)

      PLATES(1,4) = 10
      PLATES(2,4) = 11
      PLATES(3,4) = 12

!
!     Add a plate on the opposite side of the grid from plate 1,
!     but also facing in the same direction as plate one. Offset
!     this plate in the +Z direction by 1 km.
!
!     This plate is owned by voxel 4, which is contained in
!     coarse voxel 2.
!
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

!
!     Compute vertex extents.
!
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

!
!     Set the voxel-plate mapping.
!
      CALL CLEARI ( MXNVOX, VOXPLT )

!
!     Initialize all pointers to null.
!
      CALL FILLI ( -1, MXNVOX, VOXPTR )
!
!     The voxel pointer list must contain an entry for
!     each voxel belonging to a non-empty coarse voxel.
!     Coarse voxels 1 and 2 are non-empty.
!
      VOXNPT = 16

!
!     The first voxel contains plates 1 and 2. The pointer
!     associated with this voxel points to the start of the
!     voxel's plate list.
!
      VOXPTR(1) = 1

!
!     The second voxel contains plate 3. The pointer
!     associated with this voxel points to the start of the
!     voxel's plate list, which follows the list for voxel 1.
!
      VOXPTR(2) = 5

!
!     The fourth voxel contains plate 5. The pointer
!     associated with this voxel points to the start of the
!     voxel's plate list, which follows the list for voxel 2.
!     Note that the voxel pointer list contains 6 null entries
!     following the entry for voxel 2, since there are entries
!     for every voxel in the coarse voxel containing voxel 1.
!
      VOXPTR(10) = 7

!
!     The first element of a plate list is the count.
!     The plate numbers follow.
!
!     Entries for voxel 1:
!
      VOXPLT(1) = 3
      VOXPLT(2) = 1
      VOXPLT(3) = 2
      VOXPLT(4) = 4
!
!     Entries for voxel 2:
!
      VOXPLT(5) = 1
      VOXPLT(6) = 3

!
!     Entries for voxel 4:
!
      VOXPLT(7) = 1
      VOXPLT(8) = 5

!
!     VOXNPL is the number of elements in the plate list, including
!     plate counts.
!
      VOXNPL    = 8

!
!     The first coarse voxel is non-empty and points
!     to the voxel pointer for the first voxel.
!
!     The second coarse voxel is non-empty and points
!     to the voxel pointer for the fourth voxel.
!
!     The rest of the coarse grid contains null values.
!
      CALL FILLI ( 0, NCVOX, CGRPTR )

      CGRPTR(1) = 1
      CGRPTR(2) = 9

!
!     We will not use the vertex-plate list.
!

!
!     Pack the integer component of the spatial index array.
!
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

!
!     Pack the d.p. component of the spatial index array.
!
      CALL MOVED ( VTXBDS, 6, SPAIXD(SIVTBD) )
      CALL VEQU  ( ORIGIN,    SPAIXD(SIVXOR) )

      SPAIXD(SIVXSZ) = VOXSIZ

!
!     Set up ID and coverage parameters for the segment. These
!     are required in order to create a DSK segment but have no
!     other use for the tests performed here.
!
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

!
!     Open a new DSK file.
!
      DSK = 'dskx02.bds'

      IF ( EXISTS(DSK) ) THEN
         CALL DELFIL( DSK )
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF


      CALL DSKOPN ( DSK, DSK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Write the segment.
!
      CALL DSKW02 ( HANDLE, &
                    CENTER, SURFCE, DCLASS, FRAME,  CORSYS, &
                    CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2, &
                    MNCOR3, MXCOR3, FIRST,  LAST,   NV, &
                    VERTS,  NP,     PLATES, SPAIXD, SPAIXI )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKCLS ( HANDLE, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find ray intercept on plate 1; ray vertex ' &
      //           'is on the -X side.'                        )


      CALL DASOPR ( DSK,    HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray vertex is on the -X side of the grid, and the ray
!     points strictly in the +X direction.
!
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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find ray intercept on plate 4; ray vertex ' &
      //           'is on the +X side.'                        )

!
!     The ray vertex is on the +X side of the grid, and the ray
!     points strictly in the -X direction.
!
!     Plates other than plate 4 should be invisible due to their
!     vertex orientation.
!
!
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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find ray intercept on plate 5; ray vertex ' &
      //           'is on the -X side.'                        )

!
!     This case requires examination of the second group of
!     voxels within the DSKX02 loop that works with one voxel
!     group at a time.
!
!     The ray vertex is on the -X side of the grid, and the ray
!     points strictly in the +X direction.
!
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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Shoot a ray through empty voxels in coarse ' &
      //           'voxel 1.'                                   )

!
!     Voxels above the bottom layer are empty. In coarse voxel 1,
!     the voxel IDs of the upper layer are
!
!        17, 18, 21, 22
!
!
!
      VERTEX(1) = -2.5D0 * VOXSIZ
      VERTEX(2) = -0.5D0 * VOXSIZ
      VERTEX(3) = -0.5D0 * VOXSIZ

      RAYDIR(1) =  1.D0
      RAYDIR(2) = -1.D0
      RAYDIR(3) =  0.D0


      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Shoot a ray through empty coarse voxels on the ' &
      //           'top layer of the grid.'                          )

      VERTEX(1) = -3     * VOXSIZ
      VERTEX(2) =  0.5D0 * VOXSIZ
      VERTEX(3) =  1.5D0 * VOXSIZ

      RAYDIR(1) =  1.D0
      RAYDIR(2) =  0.D0
      RAYDIR(3) =  0.D0


      CALL DSKX02 ( HANDLE, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!

!
!     Add a segment for testing handling of duplicate plates.
!
      CALL TCASE ( 'Add second segment to DSK.' )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASOPW ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The voxel grid has voxel extents
!
!        100 x 1 x 1
!
!     The coarse voxel scale is 1.
!
!     The edge length of each voxel is 10 km.
!
      VGREXT(1) = 100
      VGREXT(2) =   1
      VGREXT(3) =   1

      CGRSCL    = 1
      VOXSIZ    = 10.D0

      NCVOX     = 100

!
!     The voxel grid origin coincides with the reference frame center.
!
      CALL CLEARD ( 3, ORIGIN )

!
!     Voxel 1 contains 10 plates.
!
!     Each other voxel "owns" the plates in the voxel behind it in the
!     -X direction, plus its own plates. The plates physically
!     contained in voxel `n' are shifted in the +Z direction by (n-1)
!     km relative to the plate set in voxel 1.
!

!
!     Create the vertices and plates for the first voxel.
!
!     The vertices are orders to as to make the normal vectors point
!     in the -X direction.
!
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

!
!     Set up the voxel-plate mapping.
!
      VOXNPT = 1

      VOXPTR(1) = 1

      VOXPLT(1) = 10

      DO I = 1, 10
         VOXPLT(1+I) = I
      END DO

      VOXNPL = 11

      TO     = VOXNPL + 1

!
!     Set the coarse grid pointers.
!
      CALL FILLI ( -1, NCVOX, CGRPTR )

      CGRPTR(1) = 1

!
!     Create vertices, plates, and spatial index entries for
!     the other voxels.
!
      NVOX = VGREXT(1)

      DO VI = 2, NVOX
!
!        Create vertices and plates for the current voxel.
!
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


!
!        Set the coarse grid pointer for the current coarse voxel.
!
         CGRPTR(VI) = VI

!
!        Set the voxel pointer for the current voxel.
!
         VOXPTR(VI) = TO

!
!        Fill in the plate list for the current voxel.
!
         VOXPLT(TO) = 20

!
!        Add the new plates to the plate list.
!
         DO I = 1, 10
            VOXPLT(TO+I) = NP + I
         END DO

!
!        Add the plates from the preceding voxel to the plate list.
!
         DO I = 1, 10
            VOXPLT( TO + 10 + I ) = ( NP - 10 ) + I
         END DO


         NP     = NP     + 10
         NV     = NV     + 30

         VOXNPL = VOXNPL + 21
         TO     = VOXNPL + 1

      END DO


      VOXNPT = NVOX

!
!     We will not use the vertex-plate list.
!

!
!     Compute vertex extents.
!
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


!
!     Pack the integer component of the spatial index array.
!
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

!
!     Pack the d.p. component of the spatial index array.
!
      CALL MOVED ( VTXBDS, 6, SPAIXD(SIVTBD) )
      CALL VEQU  ( ORIGIN,    SPAIXD(SIVXOR) )

      SPAIXD(SIVXSZ) = VOXSIZ

!


!
!     Set up ID and coverage parameters for the segment. These
!     are required in order to create a DSK segment but have no
!     other use for the tests performed here.
!
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

!
!     Write the segment.
!
      CALL DSKW02 ( HANDLE, &
                    CENTER, SURFCE, DCLASS, FRAME,  CORSYS, &
                    CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2, &
                    MNCOR3, MXCOR3, FIRST,  LAST,   NV, &
                    VERTS,  NP,     PLATES, SPAIXD, SPAIXI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKCLS ( HANDLE, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
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

!
! --- Case: ------------------------------------------------------
!
            CALL TCASE (TITLE)

            VERTEX(1) =      - 1.0D0
            VERTEX(2) =    I - 0.5D0
            VERTEX(3) =        0.95D0 + (VI-1)*0.09D0

            RAYDIR(1) =   1.D0
            RAYDIR(2) =   0.D0
            RAYDIR(2) =   0.D0


            CALL DSKX02 ( HANDLE, DLADSC, VERTEX, &
                          RAYDIR, PLID,   XPT,   FOUND )
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

!
! --- Case: ------------------------------------------------------
!

!
!     This case tests the fix for the bug in which an out-of-segment
!     intercept can be returned, even though the returned plate ID is
!     for a valid, in-segment solution. The original bug fix was
!     written in April 2017. The bug itself was introduced that same
!     month, slightly before the N0066 delivery.
!
      CALL TCASE ( 'Test out-of-segment intercept bug fix' )

!
!     We'll create a DSK that contains six segments. For each
!     coordinate system, there will be two segments, each containing
!     two plates.
!
!     For each coordinate system, in one of the segments associated
!     with that coordinate system, the first plate will lie within the
!     segment bounds, and the second plate will be outside the segment
!     bounds. In the second segment, the order of the plates will be
!     reversed. These variations are needed to ensure that at least one
!     segment will satisfy the conditions required to exhibit the bug,
!     regardless of the order in which the DSK type 2 writer adds data
!     to the segment.

!     The plate set used for this test is contrived and completely
!     unrealistic, but it suffices to demonstrate the error in the
!     previous version of DSKX02 and the correction in the N0067
!     version.
!
      DSK1 = 'test_dskx02_fix.bds'

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
         CALL CHCKXC ( .FALSE., ' ', OK  )
      END IF

!
!     Open the DSK for writing.
!
      CALL DSKOPN( DSK1, DSK1, 0, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK  )

!
!     Create two plates. The plates are parallel to the Y-Z plane.
!     The plate centers are on the positive X axis. Vertices of the
!     first plate are
!
!         ( 10, -1, -1 )
!         ( 10,  1, -1 )
!         ( 10,  0,  1 )
!
!     Vertices of the second plate are
!
!         ( 20, -1, -1 )
!         ( 20,  1, -1 )
!         ( 20,  0,  1 )
!
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
!
!        For each coordinate system, create segment bounds that contain
!        one segment and exclude the other.
!
         IF ( SYSTMS(I) .EQ. LATSYS ) THEN
!
!           Latitudinal bounds are for longitude, latitude, and radius,
!           in that order.
!
            MNCOR1 = -PI()
            MXCOR1 =  PI()
            MNCOR2 = -PI()/2
            MXCOR2 =  PI()/2
            MNCOR3 =   0.D0
            MXCOR3 =  15.D0
!
!           There are no coordinate system parameters.
!
            CALL CLEARD ( NSYPAR, CORPAR )
            CALL CHCKXC ( .FALSE., ' ', OK  )

         ELSE IF ( SYSTMS(I) .EQ. RECSYS ) THEN

            MNCOR1 =  0.D0
            MXCOR1 = 15.D0
            MNCOR2 = -2.D0
            MXCOR2 =  2.D0
            MNCOR3 = -2.D0
            MXCOR3 =  2.D0
!
!           There are no coordinate system parameters.
!
            CALL CLEARD ( NSYPAR, CORPAR )
            CALL CHCKXC ( .FALSE., ' ', OK  )

         ELSE IF ( SYSTMS(I) .EQ. PDTSYS ) THEN
!
!           The coordinate system parameters for planetodetic
!           coordinates are RE and F, in that order. Choose values
!           that correspond to an equatorial radius of 10 and a polar
!           radius of 8.
!
            CORPAR(1) = 10.D0
            CORPAR(2) =  0.2D0

            CALL CLEARD ( NSYPAR-2, CORPAR(3) )
            CALL CHCKXC ( .FALSE., ' ', OK  )

!
!           Planetodetic bounds are for longitude, latitude, and
!           altitude, in that order.
!
            MNCOR1 = -PI()
            MXCOR1 =  PI()
            MNCOR2 = -PI()/2
            MXCOR2 =  PI()/2
!
!           The altitude range is from -3 to 5.
!
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
!
!              Swap the plate order.
!
               DO K = 1, 3
                  CALL SWAPI ( PLATES(K,1), PLATES(K,2) )
               END DO

            END IF

!
!           Set the voxel scales. We need very large voxels, since the
!           case of interest is one in which both plates are in the
!           same voxel, even though one plate is inside the segment
!           bounds and the other is not.
!
            FINSCL = 100.D0
            CORSCL = 1

!
!           Create spatial index for the segment. Choose generous
!           values for the data structure sizes. We're not calculating
!           the exact required sizes.
!
            VOXPSZ = MXNVOX
            VOXLSZ = MXNVOX

            CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, FINSCL, &
                          CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .TRUE., &
                          SPAISZ, WORK,   SPAIXD, SPAIXI          )
            CALL CHCKXC ( .FALSE., ' ', OK  )

!
!           Set up ID parameters for the segment. These are required in
!           order to create a DSK segment but have no other use for the
!           tests performed here.
!
            SURFCE = 499
            CENTER = 4
            DCLASS = 2
            FRAME  = 'IAU_MARS'
            CORSYS = SYSTMS(I)
!
!           Write the segment.
!
            CALL DSKW02 ( HAN1, &
                          CENTER, SURFCE, DCLASS, FRAME,  CORSYS, &
                          CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2, &
                          MNCOR3, MXCOR3, FIRST,  LAST,   NV, &
                          VERTS,  NP,     PLATES, SPAIXD, SPAIXI )
            CALL CHCKXC ( .FALSE., ' ', OK  )


            IF ( J .EQ. 2 ) THEN
!
!              Undo the plate order swap.
!
               DO K = 1, 3
                  CALL SWAPI ( PLATES(K,1), PLATES(K,2) )
               END DO

            END IF

         END DO

      END DO
!
!     Close the DSK.
!
      CALL DSKCLS ( HAN1, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK  )

!
!     Now open the file for read access and traverse its segments.
!     For each segment, try a ray-surface intercept test.
!
      CALL DASOPR ( DSK1, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      CALL DLABFS ( HAN1, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK  )

      SEGNO = 0

      DO WHILE ( FOUND )

         SEGNO = SEGNO + 1
!
!        Create the vertex and direction vector of a ray to be used
!        in a call to DSKX02.
!
         CALL VPACK ( 100.D0, 0.D0, 0.D0, VERTEX )
         CALL VPACK ( -1.D0,  0.D0, 0.D0, RAYDIR )

!
!        Compute the ray-surface intercept. We expect the ray to hit
!        the plate in the X = 10 plane.
!
         CALL DSKX02 ( HAN1, DLADSC, VERTEX, RAYDIR, PLID, XPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK  )

!
!        Make sure the intercept was found.
!
         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', SEGNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )
         CALL VPACK ( 10.D0, 0.D0, 0.D0, XXPT )

!
!        Check the intercept.
!
         LABEL = 'XPT #'
         CALL REPMI ( LABEL, '#', SEGNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, VTIGHT, OK  )

!
!        To check the plate ID, we must see whether that ID belongs
!        to the plate lying in the X = 10 plane. Look up the plate.
!
         CALL DSKP02 ( HAN1, DLADSC, PLID, 1, N, PLATE )
         CALL CHCKXC ( .FALSE., ' ', OK  )

!
!        The correct plate always has vertices 1, 2, 3.
!
         DO I = 1, 3
            XPLATE(I) = I
         END DO

         LABEL = 'PLATE #'
         CALL REPMI ( LABEL, '#', SEGNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK  )

!
!        Look up the next DLA segment.
!
         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC )
         CALL CHCKXC ( .FALSE., ' ', OK  )

         CALL DLAFNS ( HAN1, PRVDSC, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK  )

      END DO

      CALL DASCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK  )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up kernels.' )


      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
