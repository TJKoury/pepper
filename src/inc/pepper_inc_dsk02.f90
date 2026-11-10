!> The parameters of SPICELIB's include file dsk02.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_dsk02

   implicit none

   public

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
   integer               ixnv
   parameter           ( ixnv   =          1 )

   integer               ixnp
   parameter           ( ixnp   = ixnv   + 1 )

   integer               ixnvxt
   parameter           ( ixnvxt = ixnp   + 1 )

   integer               ixvgrx
   parameter           ( ixvgrx = ixnvxt + 1 )

   integer               ixcgsc
   parameter           ( ixcgsc = ixvgrx + 3 )

   integer               ixvxps
   parameter           ( ixvxps = ixcgsc + 1 )

   integer               ixvxls
   parameter           ( ixvxls = ixvxps + 1 )

   integer               ixvtls
   parameter           ( ixvtls = ixvxls + 1 )

   integer               ixplat
   parameter           ( ixplat = ixvtls + 1 )

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
   integer               ixdscr
   parameter           ( ixdscr = 1 )

!
!     This local parameter MUST be kept consistent with
!     the parameter DSKDSZ which is declared in dskdsc.inc.
!
   integer               dscsz2
   parameter           ( dscsz2 = 24 )

   integer               ixvtbd
   parameter           ( ixvtbd = ixdscr + dscsz2 )

   integer               ixvxor
   parameter           ( ixvxor = ixvtbd + 6 )

   integer               ixvxsz
   parameter           ( ixvxsz = ixvxor + 3 )

   integer               ixvert
   parameter           ( ixvert = ixvxsz + 1 )

!
!     Integer item keyword parameters used by fetch routines:
!
   integer               kwnv
   parameter           ( kwnv   = 1 )

   integer               kwnp
   parameter           ( kwnp   = kwnv   + 1 )

   integer               kwnvxt
   parameter           ( kwnvxt = kwnp   + 1 )

   integer               kwvgrx
   parameter           ( kwvgrx = kwnvxt + 1 )

   integer               kwcgsc
   parameter           ( kwcgsc = kwvgrx + 1 )

   integer               kwvxps
   parameter           ( kwvxps = kwcgsc + 1 )

   integer               kwvxls
   parameter           ( kwvxls = kwvxps + 1 )

   integer               kwvtls
   parameter           ( kwvtls = kwvxls + 1 )

   integer               kwplat
   parameter           ( kwplat = kwvtls + 1 )

   integer               kwvxpt
   parameter           ( kwvxpt = kwplat + 1 )

   integer               kwvxpl
   parameter           ( kwvxpl = kwvxpt + 1 )

   integer               kwvtpt
   parameter           ( kwvtpt = kwvxpl + 1 )

   integer               kwvtpl
   parameter           ( kwvtpl = kwvtpt + 1 )

   integer               kwcgpt
   parameter           ( kwcgpt = kwvtpl + 1 )

!
!     Double precision item keyword parameters used by fetch routines:
!
   integer               kwdsc
   parameter           ( kwdsc  = kwcgpt + 1 )

   integer               kwvtbd
   parameter           ( kwvtbd = kwdsc  + 1 )

   integer               kwvxor
   parameter           ( kwvxor = kwvtbd + 1 )

   integer               kwvxsz
   parameter           ( kwvxsz = kwvxor + 1 )

   integer               kwvert
   parameter           ( kwvert = kwvxsz + 1 )

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
   integer               maxvrt
   parameter           ( maxvrt = 16000002  )

!
!     MAXPLT is the maximum number of plates that the triangular
!            plate model software will support.
!
   integer               maxplt
   parameter           ( maxplt = 2*(maxvrt - 2) )

!
!     MAXNPV is the maximum allowed number of vertices, not taking into
!     account shared vertices.
!
!     Note that this value is not sufficient to create a vertex-plate
!     mapping for a model of maximum plate count.
!
   integer               maxnpv
   parameter           ( maxnpv = 3*maxplt/2 + 1 )

!
!     MAXVOX is the maximum number of voxels.
!
   integer               maxvox
   parameter           ( maxvox = 100000000 )

!
!     MAXCGR is the maximum size of the coarse voxel grid.
!
   integer               maxcgr
   parameter           ( maxcgr = 100000 )

!
!     MAXEDG is the maximum allowed number of vertex or plate
!     neighbors a vertex may have.
!
   integer               maxedg
   parameter           ( maxedg = 120 )

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
   integer               sivgrx
   parameter           ( sivgrx = 1 )
!
!     Coarse grid scale:
!
   integer               sicgsc
   parameter           ( sicgsc = sivgrx + 3 )
!
!     Voxel pointer count:
!
   integer               sivxnp
   parameter           ( sivxnp = sicgsc + 1 )
!
!     Voxel-plate list count:
!
   integer               sivxnl
   parameter           ( sivxnl = sivxnp + 1 )
!
!     Vertex-plate list count:
!
   integer               sivtnl
   parameter           ( sivtnl = sivxnl + 1 )
!
!     Coarse grid pointers:
!
   integer               sicgrd
   parameter           ( sicgrd = sivtnl + 1 )

!
!     Size of fixed-size portion of integer component:
!
   integer               ixifix
   parameter           ( ixifix = maxcgr + 7 )

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
   integer               sivtbd
   parameter           ( sivtbd = 1 )
!
!     Voxel grid origin:
!
   integer               sivxor
   parameter           ( sivxor = sivtbd + 6 )
!
!     Voxel size:
!
   integer               sivxsz
   parameter           ( sivxsz = sivxor + 3 )

!
!     Size of fixed-size portion of double precision component:
!
   integer               ixdfix
   parameter           ( ixdfix = 10 )

!
!     The limits below are used to define a suggested maximum
!     size for the integer component of the spatial index.
!

!
!     Maximum number of entries in voxel-plate pointer array:
!
   integer               maxvxp
   parameter           ( maxvxp = maxplt / 2 )

!
!     Maximum cell size:
!
   integer               maxcel
   parameter           ( maxcel = 60000000 )

!
!     Maximum number of entries in voxel-plate list:
!
   integer               mxnvls
   parameter           ( mxnvls = maxcel + maxvxp/2 )

!
!     Spatial index integer component size:
!
   integer               spaisz
   parameter           ( spaisz =   ixifix + maxvxp &
   + mxnvls + maxvrt &
   + maxnpv          )

!
!     End of include file dsk02.inc
!

end module pepper_inc_dsk02
