!
!     Include file dsk02.inc
!
!     This include file declares parameters for DSK data type 2
!     (plate model).
!
!-       SPICELIB Version 1.0.0 08-FEB-2017 (NJB)
!
!          Updated version info.
!
!           22-JAN-2016 (NJB)
!
!              Now includes spatial index parameters.
!
!           26-MAR-2015 (NJB)
!
!              Updated to increase MAXVRT to 16000002. MAXNPV
!              has been changed to (3/2)*MAXPLT. Set MAXVOX
!              to 100000000.
!
!           13-MAY-2010 (NJB)
!
!              Updated to reflect new no-record design.
!
!           04-MAY-2010 (NJB)
!
!              Updated for new type 2 segment design. Now uses
!              a local parameter to represent DSK descriptor
!              size (NB).
!
!           13-SEP-2008 (NJB)
!
!              Updated to remove albedo information.
!              Updated to use parameter for DSK descriptor size.
!
!           27-DEC-2006 (NJB)
!
!              Updated to remove minimum and maximum radius information
!              from segment layout.  These bounds are now included
!              in the segment descriptor.
!
!           26-OCT-2006 (NJB)
!
!              Updated to remove normal, center, longest side, albedo,
!              and area keyword parameters.
!
!           04-AUG-2006 (NJB)
!
!              Updated to support coarse voxel grid.  Area data
!              have now been removed.
!
!           10-JUL-2006 (NJB)
!
!
!     Each type 2 DSK segment has integer, d.p., and character
!     components.  The segment layout in DAS address space is as
!     follows:
!
!
!        Integer layout:
!
!           +-----------------+
!           | NV              |  (# of vertices)
!           +-----------------+
!           | NP              |  (# of plates )
!           +-----------------+
!           | NVXTOT          |  (total number of voxels)
!           +-----------------+
!           | VGREXT          |  (voxel grid extents, 3 integers)
!           +-----------------+
!           | CGRSCL          |  (coarse voxel grid scale, 1 integer)
!           +-----------------+
!           | VOXNPT          |  (size of voxel-plate pointer list)
!           +-----------------+
!           | VOXNPL          |  (size of voxel-plate list)
!           +-----------------+
!           | VTXNPL          |  (size of vertex-plate list)
!           +-----------------+
!           | PLATES          |  (NP 3-tuples of vertex IDs)
!           +-----------------+
!           | VOXPTR          |  (voxel-plate pointer array)
!           +-----------------+
!           | VOXPLT          |  (voxel-plate list)
!           +-----------------+
!           | VTXPTR          |  (vertex-plate pointer array)
!           +-----------------+
!           | VTXPLT          |  (vertex-plate list)
!           +-----------------+
!           | CGRPTR          |  (coarse grid occupancy pointers)
!           +-----------------+
!
!
      INTEGER               IXNV
      PARAMETER           ( IXNV   =          1 )

      INTEGER               IXNP
      PARAMETER           ( IXNP   = IXNV   + 1 )

      INTEGER               IXNVXT
      PARAMETER           ( IXNVXT = IXNP   + 1 )

      INTEGER               IXVGRX
      PARAMETER           ( IXVGRX = IXNVXT + 1 )

      INTEGER               IXCGSC
      PARAMETER           ( IXCGSC = IXVGRX + 3 )

      INTEGER               IXVXPS
      PARAMETER           ( IXVXPS = IXCGSC + 1 )

      INTEGER               IXVXLS
      PARAMETER           ( IXVXLS = IXVXPS + 1 )

      INTEGER               IXVTLS
      PARAMETER           ( IXVTLS = IXVXLS + 1 )

      INTEGER               IXPLAT
      PARAMETER           ( IXPLAT = IXVTLS + 1 )

!
!        D.p. layout:
!
!           +-----------------+
!           | DSK descriptor  |  DSKDSZ elements
!           +-----------------+
!           | Vertex bounds   |  6 values (min/max for each component)
!           +-----------------+
!           | Voxel origin    |  3 elements
!           +-----------------+
!           | Voxel size      |  1 element
!           +-----------------+
!           | Vertices        |  3*NV elements
!           +-----------------+
!
      INTEGER               IXDSCR
      PARAMETER           ( IXDSCR = 1 )

!
!     This local parameter MUST be kept consistent with
!     the parameter DSKDSZ which is declared in dskdsc.inc.
!
      INTEGER               DSCSZ2
      PARAMETER           ( DSCSZ2 = 24 )

      INTEGER               IXVTBD
      PARAMETER           ( IXVTBD = IXDSCR + DSCSZ2 )

      INTEGER               IXVXOR
      PARAMETER           ( IXVXOR = IXVTBD + 6 )

      INTEGER               IXVXSZ
      PARAMETER           ( IXVXSZ = IXVXOR + 3 )

      INTEGER               IXVERT
      PARAMETER           ( IXVERT = IXVXSZ + 1 )



!
!     Integer item keyword parameters used by fetch routines:
!
      INTEGER               KWNV
      PARAMETER           ( KWNV   = 1 )

      INTEGER               KWNP
      PARAMETER           ( KWNP   = KWNV   + 1 )

      INTEGER               KWNVXT
      PARAMETER           ( KWNVXT = KWNP   + 1 )

      INTEGER               KWVGRX
      PARAMETER           ( KWVGRX = KWNVXT + 1 )

      INTEGER               KWCGSC
      PARAMETER           ( KWCGSC = KWVGRX + 1 )

      INTEGER               KWVXPS
      PARAMETER           ( KWVXPS = KWCGSC + 1 )

      INTEGER               KWVXLS
      PARAMETER           ( KWVXLS = KWVXPS + 1 )

      INTEGER               KWVTLS
      PARAMETER           ( KWVTLS = KWVXLS + 1 )

      INTEGER               KWPLAT
      PARAMETER           ( KWPLAT = KWVTLS + 1 )

      INTEGER               KWVXPT
      PARAMETER           ( KWVXPT = KWPLAT + 1 )

      INTEGER               KWVXPL
      PARAMETER           ( KWVXPL = KWVXPT + 1 )

      INTEGER               KWVTPT
      PARAMETER           ( KWVTPT = KWVXPL + 1 )

      INTEGER               KWVTPL
      PARAMETER           ( KWVTPL = KWVTPT + 1 )

      INTEGER               KWCGPT
      PARAMETER           ( KWCGPT = KWVTPL + 1 )

!
!     Double precision item keyword parameters used by fetch routines:
!
      INTEGER               KWDSC
      PARAMETER           ( KWDSC  = KWCGPT + 1 )

      INTEGER               KWVTBD
      PARAMETER           ( KWVTBD = KWDSC  + 1 )

      INTEGER               KWVXOR
      PARAMETER           ( KWVXOR = KWVTBD + 1 )

      INTEGER               KWVXSZ
      PARAMETER           ( KWVXSZ = KWVXOR + 1 )

      INTEGER               KWVERT
      PARAMETER           ( KWVERT = KWVXSZ + 1 )

!
!     The parameters below formerly were declared in pltmax.inc.
!
!     Limits on plate model capacity:
!
!     The maximum number of bodies, vertices and
!     plates in a plate model or collective thereof are
!     provided here.
!
!     These values can be used to dimension arrays, or to
!     use as limit checks.
!
!     The value of MAXPLT is determined from MAXVRT via
!     Euler's Formula for simple polyhedra having triangular
!     faces.
!
!     MAXVRT is the maximum number of vertices the triangular
!            plate model software will support.
!
      INTEGER               MAXVRT
      PARAMETER           ( MAXVRT = 16000002  )

!
!     MAXPLT is the maximum number of plates that the triangular
!            plate model software will support.
!
      INTEGER               MAXPLT
      PARAMETER           ( MAXPLT = 2*(MAXVRT - 2) )

!
!     MAXNPV is the maximum allowed number of vertices, not taking into
!     account shared vertices.
!
!     Note that this value is not sufficient to create a vertex-plate
!     mapping for a model of maximum plate count.
!
      INTEGER               MAXNPV
      PARAMETER           ( MAXNPV = 3*MAXPLT/2 + 1 )

!
!     MAXVOX is the maximum number of voxels.
!
      INTEGER               MAXVOX
      PARAMETER           ( MAXVOX = 100000000 )

!
!     MAXCGR is the maximum size of the coarse voxel grid.
!
      INTEGER               MAXCGR
      PARAMETER           ( MAXCGR = 100000 )

!
!     MAXEDG is the maximum allowed number of vertex or plate
!     neighbors a vertex may have.
!
      INTEGER               MAXEDG
      PARAMETER           ( MAXEDG = 120 )



!     DSK type 2 spatial index parameters
!     ===================================
!
!        DSK type 2 spatial index integer component
!        ------------------------------------------
!
!           +-----------------+
!           | VGREXT          |  (voxel grid extents, 3 integers)
!           +-----------------+
!           | CGRSCL          |  (coarse voxel grid scale, 1 integer)
!           +-----------------+
!           | VOXNPT          |  (size of voxel-plate pointer list)
!           +-----------------+
!           | VOXNPL          |  (size of voxel-plate list)
!           +-----------------+
!           | VTXNPL          |  (size of vertex-plate list)
!           +-----------------+
!           | CGRPTR          |  (coarse grid occupancy pointers)
!           +-----------------+
!           | VOXPTR          |  (voxel-plate pointer array)
!           +-----------------+
!           | VOXPLT          |  (voxel-plate list)
!           +-----------------+
!           | VTXPTR          |  (vertex-plate pointer array)
!           +-----------------+
!           | VTXPLT          |  (vertex-plate list)
!           +-----------------+
!
!
!        Index parameters
!
!
!     Grid extent:
!
      INTEGER               SIVGRX
      PARAMETER           ( SIVGRX = 1 )
!
!     Coarse grid scale:
!
      INTEGER               SICGSC
      PARAMETER           ( SICGSC = SIVGRX + 3 )
!
!     Voxel pointer count:
!
      INTEGER               SIVXNP
      PARAMETER           ( SIVXNP = SICGSC + 1 )
!
!     Voxel-plate list count:
!
      INTEGER               SIVXNL
      PARAMETER           ( SIVXNL = SIVXNP + 1 )
!
!     Vertex-plate list count:
!
      INTEGER               SIVTNL
      PARAMETER           ( SIVTNL = SIVXNL + 1 )
!
!     Coarse grid pointers:
!
      INTEGER               SICGRD
      PARAMETER           ( SICGRD = SIVTNL + 1 )

!
!     Size of fixed-size portion of integer component:
!
      INTEGER               IXIFIX
      PARAMETER           ( IXIFIX = MAXCGR + 7 )


!
!        DSK type 2 spatial index double precision component
!        ---------------------------------------------------
!
!           +-----------------+
!           | Vertex bounds   |  6 values (min/max for each component)
!           +-----------------+
!           | Voxel origin    |  3 elements
!           +-----------------+
!           | Voxel size      |  1 element
!           +-----------------+
!
!
!
!        Index parameters
!
!     Vertex bounds:
!
      INTEGER               SIVTBD
      PARAMETER           ( SIVTBD = 1 )
!
!     Voxel grid origin:
!
      INTEGER               SIVXOR
      PARAMETER           ( SIVXOR = SIVTBD + 6 )
!
!     Voxel size:
!
      INTEGER               SIVXSZ
      PARAMETER           ( SIVXSZ = SIVXOR + 3 )

!
!     Size of fixed-size portion of double precision component:
!
      INTEGER               IXDFIX
      PARAMETER           ( IXDFIX = 10 )


!
!     The limits below are used to define a suggested maximum
!     size for the integer component of the spatial index.
!

!
!     Maximum number of entries in voxel-plate pointer array:
!
      INTEGER               MAXVXP
      PARAMETER           ( MAXVXP = MAXPLT / 2 )

!
!     Maximum cell size:
!
      INTEGER               MAXCEL
      PARAMETER           ( MAXCEL = 60000000 )

!
!     Maximum number of entries in voxel-plate list:
!
      INTEGER               MXNVLS
      PARAMETER           ( MXNVLS = MAXCEL + MAXVXP/2 )

!
!     Spatial index integer component size:
!
      INTEGER               SPAISZ
      PARAMETER           ( SPAISZ =   IXIFIX + MAXVXP &
                                     + MXNVLS + MAXVRT &
                                     + MAXNPV          )


!
!     End of include file dsk02.inc
!

