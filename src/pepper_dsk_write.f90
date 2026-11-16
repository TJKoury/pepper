!> Writing type 2 DSK segments ([[dskw02]]).
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]). Moved here from src/spicelib by
!> tools/modularize_ctx.py; the external procedures with the original
!> interface are in tspice/src/legacy/pepper_legacy_dsk_write.f90.
submodule (pepper_instance_module) pepper_dsk_write

   implicit none

contains

      !>## Detailed Input
      !>
      !>     HANDLE   is the DAS file handle associated with a DSK file.
      !>              The file must be open for write access.
      !>
      !>     CENTER   is the ID code of the body whose surface is described
      !>              by the input plate model. CENTER refers to an
      !>              ephemeris object.
      !>
      !>     SURFID   is the ID code of the surface described by the input
      !>              plate model. Multiple surfaces (for example, surfaces
      !>              having different resolutions) may be associated with
      !>              a given body.
      !>
      !>     DCLASS   is the data class of the input data set. See the
      !>              INCLUDE file dskdsc.inc for values and meanings.
      !>
      !>     FRAME    is the name of the reference frame with respect to
      !>              which the input data are expressed.
      !>
      !>     CORSYS   is the coordinate system in which the spatial coverage
      !>              of the input data is expressed. CORSYS is an integer
      !>              code. The supported values of CORPAR are
      !>
      !>                 Parameter name      Coordinate system
      !>                 --------------      -----------------
      !>                 LATSYS              Planetocentric latitudinal
      !>                 RECSYS              Rectangular (Cartesian)
      !>                 PDTSYS              Planetodetic
      !>
      !>              See the INCLUDE file dskdsc.inc for parameter
      !>              declarations.
      !>
      !>     CORPAR   is an array of parameters associated with the input
      !>              coordinate system.
      !>
      !>              For latitudinal and rectangular coordinates, CORPAR
      !>              is ignored.
      !>
      !>              For planetodetic coordinates, the contents of CORPAR
      !>              are:
      !>
      !>                 Element         Contents
      !>                 ---------       -----------------------------------
      !>                 CORPAR(1)       Equatorial radius of reference
      !>                                 spheroid.
      !>
      !>                 CORPAR(2)       Flattening coefficient. The polar
      !>                                 radius of the reference spheroid
      !>                                 is given by
      !>
      !>                                    CORPAR(1) * ( 1 - CORPAR(2) )
      !>
      !>                 CORPAR(3)...
      !>                 CORPAR(NSYPAR)  Unused.
      !>
      !>     MNCOR1,
      !>     MXCOR1,
      !>     MNCOR2,
      !>     MXCOR2,
      !>     MNCOR3,
      !>     MXCOR3   are, respectively, lower and upper bounds of
      !>              each of the coordinates of the input data, where the
      !>              coordinate system is defined by CORSYS and CORPAR.
      !>              These bounds define the region for which the segment
      !>              provides data.
      !>
      !>              Distance units are km. Angular units are radians.
      !>
      !>              The interpretation of these bounds depends on the data
      !>              class; see DCLASS above.
      !>
      !>                 Single-valued surfaces
      !>                 ----------------------
      !>
      !>                 If the segment has data class SVFCLS (see
      !>                 dskdsc.inc), the segment defines a surface as a
      !>                 single-valued function of its domain coordinates:
      !>                 for example, it may define the radius of the
      !>                 surface as a function of planetocentric longitude
      !>                 and latitude, or Z as a function of X and Y.
      !>
      !>                 In this case, the input data must cover a
      !>                 rectangle in dimensions 1 and 2 of the input
      !>                 coordinate system: the set of points
      !>
      !>                    R = { (x,y): MNCOR1 <= x <= MXCOR1;
      !>                                 MNCOR2 <= y <= MXCOR2  }
      !>
      !>                 must be completely covered by the input data. In
      !>                 other words, for each point (x,y) of R, there must
      !>                 be some plate containing a point whose first two
      !>                 coordinates are (x,y).
      !>
      !>                 The plate set may extend beyond the coordinate
      !>                 range defined by the bounds on the domain.
      !>
      !>                 Normally for single-valued surfaces, MNCOR3 and
      !>                 MXCOR3 are the minimum and maximum values of the
      !>                 function attained on the domain.
      !>
      !>                 General surfaces
      !>                 ----------------
      !>
      !>                 If the segment has data class GENCLS (see
      !>                 dskdsc.inc), the segment simply contains a
      !>                 collection of plates: no guarantees are made about
      !>                 the topology of the surface. The coordinate bounds
      !>                 simply indicate the spatial region for which the
      !>                 segment provides data.
      !>
      !>                 Note that shapes of small bodies such as asteroids
      !>                 and comet nuclei may fall into the "general
      !>                 surface" category. Surface features such as cliffs,
      !>                 caves, and arches can prevent a surface from being
      !>                 represented as a single-valued function of latitude
      !>                 and longitude, for example.
      !>
      !>              Longitude interpretation and restrictions
      !>              -----------------------------------------
      !>
      !>              The following rules apply to longitudes provided in
      !>              the arguments
      !>
      !>                 MNCOR1
      !>                 MXCOR1
      !>
      !>              All angles have units of radians. The tolerance
      !>              ANGMRG is used for the comparisons shown below.
      !>
      !>                 1) Longitudes must be in the range
      !>
      !>                       -2*pi  :  2*pi
      !>
      !>                    Values that are out of range by no more than
      !>                    ANGMRG are bracketed to be in range.
      !>
      !>                 2) It is acceptable for the longitude bounds to be
      !>                    out of order. If
      !>
      !>                       MXCOR1 < MNCOR1
      !>
      !>                    then either MXCOR1 is treated by the DSK
      !>                    subsystem as though it were MXCOR1 + 2*pi, or
      !>                    MNCOR1 is treated as MNCOR1 - 2*pi: whichever
      !>                    shift puts the bounds in the allowed range is
      !>                    made.
      !>
      !>                    The input longitude bounds must not be equal.
      !>                    If the lower bound is greater than the upper
      !>                    bound, the difference between the bounds must
      !>                    not be an integer multiple of 2*pi.
      !>
      !>                 3) MXCOR1 must not exceed MNCOR1 by more than 2*pi.
      !>                    Values that are out of range by no more than
      !>                    ANGMRG are bracketed to be in range.
      !>
      !>     FIRST,
      !>     LAST     are the endpoints of the time interval over which
      !>              this data set is applicable. These times are
      !>              expressed as seconds past J2000 TDB.
      !>
      !>     NV       is the number of vertices belonging to the plate
      !>              model.
      !>
      !>     VRTCES   is an array of coordinates of the vertices.
      !>              The Ith vertex occupies elements (1:3,I) of
      !>              this array.
      !>
      !>     NP       is the number of plates in the plate model.
      !>
      !>     PLATES   is an array representing the plates of the model.
      !>              The elements of PLATES are vertex indices. The vertex
      !>              indices of the Ith plate occupy elements (1:3,I) of
      !>              this array.
      !>
      !>     SPAIXD,
      !>     SPAIXI   are, respectively, the double precision and integer
      !>              components of the spatial index of the segment.
      !>
      !>              It is strongly recommended that the helper routine
      !>              DSKMI2 be used to create these arrays. See the
      !>              $Examples section below.
      !>
      !>## Detailed Output
      !>
      !>     None. This routine operates by side effects.
      !>
      !>## Parameters
      !>
      !>     See the SPICELIB include files
      !>
      !>        dsk02.inc
      !>        dskdsc.inc
      !>        dsktol.inc
      !>
      !>     for declarations and detailed descriptions of the parameters
      !>     referenced in this header.
      !>
      !>## Exceptions
      !>
      !>     1)  If the reference frame name FRAME could not be mapped to
      !>         an ID code, the error SPICE(FRAMEIDNOTFOUND) is signaled.
      !>
      !>     2)  If the segment stop time precedes the start time, the
      !>         error SPICE(TIMESOUTOFORDER) is signaled.
      !>
      !>     3)  If an input longitude value is outside the range
      !>
      !>            [ -2*pi - ANGMRG,   2*pi + ANGMRG ]
      !>
      !>         the error SPICE(VALUEOUTOFRANGE) is signaled. Longitudes
      !>         outside of the range by a smaller amount than ANGMRG will be
      !>         truncated to lie in the interval [-2*pi, 2*pi].
      !>
      !>     4)  If the absolute value of the difference between the input
      !>         maximum longitude and the minimum longitude is more than 2*pi
      !>         + ANGMRG, the error SPICE(INVALIDLONEXTENT) is signaled.
      !>         If either longitude bound exceeds the other by an amount
      !>         between 2*pi and 2*pi+ANGMRG, the larger value will be
      !>         truncated to the smaller value plus 2*pi.
      !>
      !>     5)  If an input latitude value is outside the range
      !>
      !>            [ -pi/2 - ANGMRG,   pi/2 + ANGMRG ]
      !>
      !>         the error SPICE(VALUEOUTOFRANGE) is signaled. Latitudes
      !>         outside of the range by a smaller amount than ANGMRG will be
      !>         truncated to lie in the interval [-pi/2, pi/2].
      !>
      !>     6)  If the coordinate system is latitudinal and the lower radius
      !>         bound is negative, or if the upper radius bound is
      !>         non-positive, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     7)  If the coordinate system is latitudinal or planetodetic and
      !>         the bounds of the latitude, radius or altitude coordinate are
      !>         out of order, the error SPICE(BOUNDSOUTOFORDER) is signaled.
      !>
      !>     8)  If the coordinate system is latitudinal or planetodetic and
      !>         the lower and upper bounds of the longitude, latitude, radius
      !>         or altitude coordinate, respectively, are equal, the error
      !>         SPICE(ZEROBOUNDSEXTENT) is signaled. If the lower
      !>         longitude bound is greater than the upper bound, and if the
      !>         difference between the bounds is an integer multiple of 2*pi,
      !>         the same error is signaled.
      !>
      !>     9)  If the coordinate system is planetodetic and the input
      !>         equatorial radius is non-positive, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     10) If the coordinate system is planetodetic and the input
      !>         flattening coefficient is greater than or equal to 1, the
      !>         error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     11) If the coordinate system is planetodetic, and if the minimum
      !>         altitude is less than the maximum of
      !>
      !>                    2           2
      !>              {  -(B / A),   -(A / B)  }
      !>
      !>         where A and B are the semi-major and semi-minor axis lengths
      !>         of the reference ellipsoid, the error
      !>         SPICE(DEGENERATESURFACE) is signaled.
      !>
      !>     12) If the coordinate system is rectangular and any coordinate
      !>         lower bound is greater than or equal to the corresponding
      !>         upper bound, the error SPICE(BOUNDSOUTOFORDER) is signaled.
      !>
      !>     13) If the coordinate system code is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     14) If any vertex index belonging to an input plate is outside of
      !>         the range 1:NV, the error SPICE(BADVERTEXINDEX) is signaled.
      !>
      !>     15) If NV is less than 1 or greater than MAXVRT, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     16) If NP is less than 1 or greater than MAXPLT, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     17) If any voxel grid extent is less than 1 or greater than
      !>         MAXVOX, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     18) If the voxel count is greater than MAXVOX, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     19) If the coarse voxel count is less than 1 or greater than
      !>         MAXCGR, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     20) If the coarse voxel scale is less than 1 or more than
      !>         the cube root of the fine voxel count, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     21) If the cube of the coarse voxel scale does not divide the
      !>         fine voxel count evenly, the error SPICE(INCOMPATIBLESCALE)
      !>         is signaled.
      !>
      !>     22) If the input data class is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     23) If an error occurs while writing the segment to the output
      !>         DSK file, the error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine writes a type 2 segment to a DSK file that
      !>     has been opened for write access.
      !>
      !>     Users planning to create DSK files should consider whether the
      !>     SPICE DSK creation utility MKDSK may be suitable for their needs.
      !>
      !>     This routine is supported by the routines DSKMI2 and DSKRB2
      !>     DSKMI2 simplifies use of this routine by creating the "spatial
      !>     index" arrays required as inputs by this routine. DSKRB2 computes
      !>     bounds on the third coordinate of the input plate set.
      !>
      !>     Spatial Indexes
      !>     ===============
      !>
      !>     A spatial index is a group of data structures that facilitates
      !>     rapid high-level computations involving sets of plates. The data
      !>     structures created by this routine are aggregated into arrays
      !>     of type INTEGER and type DOUBLE PRECISION.
      !>
      !>     Voxel grids
      !>     ===========
      !>
      !>     A key geometric computation---probably the most important, as it
      !>     serves as a foundation for other high-level computations---is
      !>     finding the intersection of a ray with the plate set. DSK type 2
      !>     segments use data structures called "voxel grids" as part of
      !>     their indexing mechanism. There is a "coarse grid": a box that
      !>     completely encloses a DSK type 2 segment's plate set, and which
      !>     is composed of identically-sized cubes called "coarse voxels."
      !>     Each coarse voxel in composed of smaller cubes called "fine
      !>     voxels." When the term "voxel" is used without qualification, it
      !>     refers to fine voxels.
      !>
      !>     Type 2 DSK segments contain data structures that associate plates
      !>     with the fine voxels intersected by those plates. These
      !>     structures enable the type 2 DSK software to rapidly find plates
      !>     in a given region of space.
      !>
      !>     Voxel scales
      !>     ============
      !>
      !>     There are two voxel scales:
      !>
      !>     -  The coarse voxel scale is the integer ratio of the
      !>        edge length of a coarse voxel to the edge length of
      !>        a fine voxel
      !>
      !>     -  The fine voxel scale is the double precision ratio
      !>        of the edge length of a fine voxel to the average
      !>        extent of the plates in the input plate set. "Extents"
      !>        of a plate are the absolute values of the differences
      !>        between the respective maximum and minimum X, Y, and Z
      !>        coordinates of the plate's vertices.
      !>
      !>     Voxel scales determine the resolution of the voxel grid.
      !>     Voxel scales must be chosen to satisfy size constraints and
      !>     provide reasonable plate lookup performance.
      !>
      !>     The following considerations apply to spatial indexes of
      !>     type 2 DSK segments:
      !>
      !>        1)  The maximum number of coarse voxels is fixed at MAXCGR
      !>            (declared in dsk02.inc).
      !>
      !>        2)  If there are too few fine voxels, the average number of
      !>            plates per fine voxel will be very large. This largely
      !>            negates the performance improvement afforded by having an
      !>            index. Also, the number of plates per voxel may exceed
      !>            limits imposed by DSK subroutines that use static arrays.
      !>
      !>        3)  If there are too many fine voxels, the average number of
      !>            voxels intersected by a given plate may be too large for
      !>            all the plate-voxel associations to be stored. In
      !>            addition, the time needed to examine the plate lists for
      !>            each voxel (including the empty ones) may become quite
      !>            large, again negating the value of the index.
      !>
      !>     In many cases, voxel scales yielding optimum performance must be
      !>     determined by experiment. However, the following heuristics can
      !>     provide reasonable starting values:
      !>
      !>        Let NP be the number of plates. Let FS be the fine voxel
      !>        scale. Then a reasonable value of FS may be
      !>
      !>                   (0.25D0)
      !>           FS =  NP       / 8.D0
      !>
      !>        In general, FS should not smaller than 1.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a three-segment DSK file using plate model data for
      !>        Phobos. Use latitudinal, rectangular, and planetodetic
      !>        coordinates in the respective segments. This is not a
      !>        realistic example, but it serves to demonstrate use of
      !>        the supported coordinate systems.
      !>
      !>        Use the DSK kernel below to provide, for simplicity, the
      !>        input plate and vertex data. The selected input file has one
      !>        segment.
      !>
      !>           phobos_3_3.bds
      !>
      !>        Example code begins here.
      !>
      !>        C
      !>        C     Example program for DSKW02, DSKMI2, and DSKRB2
      !>        C
      !>        C        Create a three-segment DSK file using plate model
      !>        C        data for Phobos. Use latitudinal, rectangular, and
      !>        C        planetodetic coordinates in the respective segments.
      !>        C
      !>        C        For simplicity, use an existing DSK file to provide
      !>        C        the input plate and vertex data. The selected input
      !>        C        file has one segment.
      !>        C
      !>        C           Version 1.0.0 22-JAN-2016 (NJB)
      !>        C
      !>              PROGRAM DSKW02_EX1
      !>              IMPLICIT NONE
      !>
      !>              INCLUDE 'dla.inc'
      !>              INCLUDE 'dskdsc.inc'
      !>              INCLUDE 'dsk02.inc'
      !>
      !>        C
      !>        C     SPICELIB functions
      !>        C
      !>              DOUBLE PRECISION      JYEAR
      !>              DOUBLE PRECISION      PI
      !>        C
      !>        C     Local parameters
      !>        C
      !>              INTEGER               FRNMLN
      !>              PARAMETER           ( FRNMLN = 32 )
      !>
      !>              INTEGER               NSEG
      !>              PARAMETER           ( NSEG   = 3 )
      !>
      !>              INTEGER               NAMLEN
      !>              PARAMETER           ( NAMLEN = 20 )
      !>
      !>              INTEGER               FILSIZ
      !>              PARAMETER           ( FILSIZ = 255 )
      !>
      !>              INTEGER               LNSIZE
      !>              PARAMETER           ( LNSIZE = 80 )
      !>
      !>              INTEGER               NCOR
      !>              PARAMETER           ( NCOR   = 4 )
      !>
      !>        C
      !>        C     Local variables
      !>        C
      !>              CHARACTER*(NAMLEN)    CORNAM ( NCOR )
      !>              CHARACTER*(FILSIZ)    DSK
      !>              CHARACTER*(FRNMLN)    FRAME
      !>              CHARACTER*(FILSIZ)    INDSK
      !>              CHARACTER*(LNSIZE)    LINE
      !>        C
      !>        C     Note: the values of MAXVRT and MAXPLT declared
      !>        C     in dsk02.inc, and the integer spatial index
      !>        C     dimension SPAISZ are very large. Smaller buffers
      !>        C     can be used for most applications.
      !>        C
      !>              DOUBLE PRECISION      CORPAR ( NSYPAR )
      !>              DOUBLE PRECISION      F
      !>              DOUBLE PRECISION      FINSCL
      !>              DOUBLE PRECISION      FIRST
      !>              DOUBLE PRECISION      LAST
      !>              DOUBLE PRECISION      MNCOR1
      !>              DOUBLE PRECISION      MNCOR2
      !>              DOUBLE PRECISION      MNCOR3
      !>              DOUBLE PRECISION      MXCOR1
      !>              DOUBLE PRECISION      MXCOR2
      !>              DOUBLE PRECISION      MXCOR3
      !>              DOUBLE PRECISION      RE
      !>              DOUBLE PRECISION      RP
      !>              DOUBLE PRECISION      SPAIXD ( IXDFIX )
      !>              DOUBLE PRECISION      VRTCES ( 3, MAXVRT )
      !>
      !>              INTEGER               CENTER
      !>              INTEGER               CORSCL
      !>              INTEGER               CORSYS
      !>              INTEGER               DCLASS
      !>              INTEGER               DLADSC ( DLADSZ )
      !>              INTEGER               HANDLE
      !>              INTEGER               INHAN
      !>              INTEGER               NP
      !>              INTEGER               NV
      !>              INTEGER               PLATES ( 3, MAXPLT )
      !>              INTEGER               SEGNO
      !>              INTEGER               SPAIXI ( SPAISZ )
      !>              INTEGER               SURFID
      !>              INTEGER               VOXPSZ
      !>              INTEGER               VOXLSZ
      !>              INTEGER               WORK   ( 2, MAXCEL )
      !>              INTEGER               WORKSZ
      !>
      !>              LOGICAL               FOUND
      !>        C
      !>        C     Saved variables
      !>        C
      !>        C     Save all large arrays to avoid stack problems.
      !>        C
      !>              SAVE
      !>        C
      !>        C     Initial values
      !>        C
      !>              DATA                  CORNAM / 'radius',
      !>             .                               'Z-coordinate',
      !>             .                               'Z-coordinate',
      !>             .                               'altitude'     /
      !>
      !>        C
      !>        C     Assign names of input and output DSK files.
      !>        C
      !>              INDSK = 'phobos_3_3.bds'
      !>              DSK   = 'phobos_3_3_3seg.bds'
      !>        C
      !>        C     Open input DSK for read access; find first segment.
      !>        C
      !>              CALL DASOPR ( INDSK, INHAN )
      !>              CALL DLABFS ( INHAN, DLADSC, FOUND )
      !>        C
      !>        C     Fetch vertices and plates from input DSK file.
      !>        C
      !>              WRITE (*,*) 'Reading input data...'
      !>
      !>              CALL DSKV02 ( INHAN, DLADSC, 1, MAXVRT, NV, VRTCES )
      !>              CALL DSKP02 ( INHAN, DLADSC, 1, MAXPLT, NP, PLATES )
      !>
      !>              WRITE (*,*) 'Done.'
      !>        C
      !>        C     Set input array sizes required by DSKMI2.
      !>        C
      !>              VOXPSZ = MAXVXP
      !>              VOXLSZ = MXNVLS
      !>              WORKSZ = MAXCEL
      !>        C
      !>        C     Set fine and coarse voxel scales. (These usually
      !>        C     need to determined by experimentation.)
      !>        C
      !>              FINSCL = 5.D0
      !>              CORSCL = 4
      !>        C
      !>        C     Open a new DSK file.
      !>        C
      !>              CALL DSKOPN ( DSK, DSK, 0, HANDLE )
      !>        C
      !>        C     Create three segments and add them to the file.
      !>        C
      !>              DO SEGNO = 1, NSEG
      !>        C
      !>        C        Create spatial index.
      !>        C
      !>                 WRITE (*,*) 'Creating segment ', SEGNO
      !>                 WRITE (*,*) 'Creating spatial index...'
      !>
      !>                 CALL DSKMI2 ( NV,     VRTCES, NP,     PLATES, FINSCL,
      !>             .                 CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .TRUE.,
      !>             .                 SPAISZ, WORK,   SPAIXD, SPAIXI        )
      !>
      !>                 WRITE (*,*) 'Done.'
      !>        C
      !>        C        Set up inputs describing segment attributes:
      !>        C
      !>        C        - Central body: Phobos
      !>        C        - Surface ID code: user's choice.
      !>        C          We use the segment number here.
      !>        C        - Data class: general (arbitrary) shape
      !>        C        - Body-fixed reference frame
      !>        C        - Time coverage bounds (TBD)
      !>        C
      !>                 CENTER = 401
      !>                 SURFID = SEGNO
      !>                 DCLASS = GENCLS
      !>                 FRAME  = 'IAU_PHOBOS'
      !>
      !>                 FIRST = -50 * JYEAR()
      !>                 LAST  =  50 * JYEAR()
      !>        C
      !>        C        Set the coordinate system and coordinate system
      !>        C        bounds based on the segment index.
      !>        C
      !>        C        Zero out the coordinate parameters to start.
      !>        C
      !>                 CALL CLEARD ( NSYPAR, CORPAR )
      !>
      !>                 IF ( SEGNO .EQ. 1 ) THEN
      !>        C
      !>        C           Use planetocentric latitudinal coordinates. Set
      !>        C           the longitude and latitude bounds.
      !>        C
      !>                    CORSYS = LATSYS
      !>
      !>                    MNCOR1 = -PI()
      !>                    MXCOR1 =  PI()
      !>                    MNCOR2 = -PI()/2
      !>                    MXCOR2 =  PI()/2
      !>
      !>                 ELSE IF ( SEGNO .EQ. 2 ) THEN
      !>        C
      !>        C           Use rectangular coordinates. Set the
      !>        C           X and Y bounds.
      !>        C
      !>        C           The bounds shown here were derived from
      !>        C           the plate data. They lie slightly outside
      !>        C           of the range spanned by the plates.
      !>        C
      !>                    CORSYS = RECSYS
      !>
      !>                    MNCOR1 = -1.3D0
      !>                    MXCOR1 =  1.31D0
      !>                    MNCOR2 = -1.21D0
      !>                    MXCOR2 =  1.2D0
      !>
      !>                 ELSE
      !>        C
      !>        C           Set the coordinate system to planetodetic.
      !>        C
      !>                    CORSYS    = PDTSYS
      !>
      !>                    MNCOR1    = -PI()
      !>                    MXCOR1    =  PI()
      !>                    MNCOR2    = -PI()/2
      !>                    MXCOR2    =  PI()/2
      !>        C
      !>        C           We'll use equatorial and polar radii from
      !>        C           pck00010.tpc. These normally would be fetched
      !>        C           at run time, but for simplicity, we'll use
      !>        C           hard-coded values.
      !>
      !>                    RE        = 13.0D0
      !>                    RP        =  9.1D0
      !>                    F         = ( RE - RP ) / RE
      !>
      !>                    CORPAR(1) = RE
      !>                    CORPAR(2) = F
      !>
      !>                 END IF
      !>        C
      !>        C        Compute plate model radius bounds.
      !>        C
      !>                 LINE = 'Computing # bounds of plate set...'
      !>
      !>                 CALL REPMC ( LINE, '#', CORNAM(CORSYS), LINE )
      !>                 WRITE (*,*) LINE
      !>
      !>                 CALL DSKRB2 ( NV,     VRTCES, NP,     PLATES,
      !>             .                 CORSYS, CORPAR, MNCOR3, MXCOR3 )
      !>
      !>                 WRITE (*,*) 'Done.'
      !>        C
      !>        C        Write the segment to the file.
      !>        C
      !>                 WRITE (*,*) 'Writing segment...'
      !>
      !>                 CALL DSKW02 ( HANDLE,
      !>             .                 CENTER, SURFID, DCLASS, FRAME,  CORSYS,
      !>             .                 CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2,
      !>             .                 MNCOR3, MXCOR3, FIRST,  LAST,   NV,
      !>             .                 VRTCES, NP,     PLATES, SPAIXD, SPAIXI )
      !>
      !>                 WRITE (*,*) 'Done.'
      !>
      !>              END DO
      !>        C
      !>        C     Segregate the data records in the DSK file and
      !>        C     close the file.
      !>        C
      !>              WRITE (*,*) 'Segregating and closing DSK file...'
      !>
      !>              CALL DSKCLS ( HANDLE, .TRUE. )
      !>
      !>              WRITE (*,*) 'Done.'
      !>              END
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Reading input data...
      !>         Done.
      !>         Creating segment            1
      !>         Creating spatial index...
      !>         Done.
      !>         Computing radius bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Creating segment            2
      !>         Creating spatial index...
      !>         Done.
      !>         Computing Z-coordinate bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Creating segment            3
      !>         Creating spatial index...
      !>         Done.
      !>         Computing altitude bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Segregating and closing DSK file...
      !>         Done.
      !>
      !>        Note that after run completion, a new DSK exists in the output
      !>        directory.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 28-AUG-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added solution to code example.
      !>
      !>        Corrected description of coverage requirements for class 1
      !>        segments.
      !>
      !>        Deleted paragraph saying that, except for changes made to
      !>        move longitude values into range, the values are stored in
      !>        segment "as is."
      !>
      !>-    SPICELIB Version 1.0.0, 04-MAR-2017 (NJB)
      !>
      !>        Fixed some comment typos.
      !>
      !>     10-OCT-2016 (NJB)
      !>
      !>        New error checks on inputs were added.
      !>
      !>     07-MAR-2016 (NJB)
      !>
      !>        New error checks on inputs were added.
      !>
      !>        Argument list change: spatial index is now passed in
      !>        as two arrays: SPAIXD and SPAIXI.
      !>
      !>        Argument CORPAR was added.
      !>
      !>        Double precision data are now written to the output
      !>        segment before integer data.
      !>
      !>        22-AUG-2012 (NJB)
      !>
      !>           Bug fix: corrected upper bound in test for
      !>           vertex count.
      !>
      !>        13-MAY-2010 (NJB)
      !>
      !>           Updated to reflect new type 2 segment design: normal
      !>           vectors, plate centers, and lengths of longest plate sides
      !>           are no longer stored in these segments.
      !>
      !>        03-APR-2010 (NJB)
      !>
      !>           New interface; general coordinates are supported. Time
      !>           bounds, surface ID, data class, and bounds of third
      !>           coordinate have been added. Albedo inputs have been
      !>           deleted.
      !>
      !>        09-OCT-2009 (NJB)
      !>
      !>           Header was added.
      !>
      !>        31-OCT-2006 (NJB)
      !>
      !>           Input arguments CGSCAL and VTXBDS were removed.
      !>
      !>        27-JUN-2006 (NJB)
      !>
      !>           Initial version.
      module subroutine dskw02 ( ctx, handle, center, surfid, dclass, frame, corsys, corpar, mncor1, mxcor1, mncor2, mxcor2, mncor3, mxcor3, first, last, nv, vrtces, np, plates, spaixd, spaixi )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpr, halfpi, twopi
      use pepper_inc_dskdsc
      use pepper_inc_dsktol
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle assigned to the opened DSK file.
      integer(ip), intent(in) :: center  !! Central body ID code.
      integer(ip), intent(in) :: surfid  !! Surface ID code.
      integer(ip), intent(in) :: dclass  !! Data class.
      character(*), intent(in) :: frame  !! Reference frame.
      integer(ip), intent(in) :: corsys  !! Coordinate system code.
      real(wp), intent(in) :: corpar ( * )  !! Coordinate system parameters.
      real(wp), intent(in) :: mncor1  !! Minimum value of first coordinate.
      real(wp), intent(in) :: mxcor1  !! Maximum value of first coordinate.
      real(wp), intent(in) :: mncor2  !! Minimum value of second coordinate.
      real(wp), intent(in) :: mxcor2  !! Maximum value of second coordinate.
      real(wp), intent(in) :: mncor3  !! Minimum value of third coordinate.
      real(wp), intent(in) :: mxcor3  !! Maximum value of third coordinate.
      real(wp), intent(in) :: first  !! Coverage start time.
      real(wp), intent(in) :: last  !! Coverage stop time.
      integer(ip), intent(in) :: nv  !! Number of vertices.
      real(wp), intent(in) :: vrtces ( 3, nv )  !! Vertices.
      integer(ip), intent(in) :: np  !! Number of plates.
      integer(ip), intent(in) :: plates ( 3, np )  !! Plates.
      real(wp), intent(in) :: spaixd ( * )  !! Double precision component of spatial index.
      integer(ip), intent(in) :: spaixi ( * )  !! Integer component of spatial index.

      real(wp) :: a
      real(wp) :: altlim
      real(wp) :: b
      real(wp) :: descr  ( dskdsz )
      real(wp) :: r
      real(wp) :: segbds ( 2, 2 )
      real(wp) :: voxori ( 3 )
      real(wp) :: voxsiz
      real(wp) :: vtxbds ( 2, 3 )

      integer(ip) :: cgrscl
      integer(ip) :: frmcde
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: ncgr
      integer(ip) :: nvxtot
      integer(ip) :: pvoxpl
      integer(ip) :: pvoxpt
      integer(ip) :: pvtxpl
      integer(ip) :: pvtxpt
      integer(ip) :: q
      integer(ip) :: vgrext ( 3 )
      integer(ip) :: voxnpt
      integer(ip) :: voxnpl
      integer(ip) :: vtxnpl

      if ( ctx%failed() ) return

!
!     Map the input reference frame name to an ID code.
!
      call namfrm( ctx, frame, frmcde )

      if ( frmcde == 0  ) then

         call ctx%setmsg ( 'Input reference frame # could not ' &
         //            'be mapped to an ID code. The frame ' &
         //            'name might be misspelled, or possibly ' &
         //            'a required frame kernel was not loaded. ' )
         call ctx%errch  ( '#',  frame                                )
         call ctx%sigerr ( 'SPICE(FRAMEIDNOTFOUND)'                   )
         return

      end if

!
!     Make sure the time bounds are in order.
!
      if ( last <= first ) then

         call ctx%setmsg ( 'Segment time bounds must be increasing; ' &
         //            'bounds were #:#.'                         )
         call ctx%errdp  ( '#',  first                                )
         call ctx%errdp  ( '#',  last                                 )
         call ctx%sigerr ( 'SPICE(TIMESOUTOFORDER)'                   )
         return

      end if

!
!     If applicable, check segment boundaries. Check the
!     coordinate system as well.
!
      if ( ( corsys == latsys ) .or. ( corsys == pdtsys )  ) then
!
!        Reject invalid latitudes and longitudes. Move
!        values that are slightly out of range into range.
!
!        Longitude bounds must be distinct.
!
         if ( mncor1 == mxcor1 ) then

            call ctx%setmsg ( 'Minimum longitude # radians (# degrees) ' &
            //            'was equal to maximum longitude. Longitude ' &
            //            'bounds must be distinct.'                 )
            call ctx%errdp  ( '#', mncor1                                )
            call ctx%errdp  ( '#', mncor1 * dpr                        )
            call ctx%sigerr ( 'SPICE(ZEROBOUNDSEXTENT)'                  )
            return

         end if

!
!        Check minimum longitude.
!
         if (      ( mncor1 < -twopi-angmrg ) &
              .or. ( mncor1 >  twopi-angmrg ) ) then

            call ctx%setmsg ( 'Minimum longitude # radians (# degrees) ' &
            //            'was outside of valid range ' &
            //            '[-2*pi, 2*pi - ANGMRG]'                   )
            call ctx%errdp  ( '#', mncor1                                )
            call ctx%errdp  ( '#', mncor1 * dpr                        )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                   )
            return

         end if

!
!        The minimum longitude is too small by ANGMRG, at worst.
!        Make it greater than or equal to -2*pi.
!
         segbds(1,1) = max( -twopi, mncor1 )

!
!        Check maximum longitude.
!
         if (      ( mxcor1 < -twopi+angmrg ) &
              .or. ( mxcor1 >  twopi+angmrg ) ) then

            call ctx%setmsg ( 'Maximum longitude # radians (# degrees) ' &
            //            'was outside of valid range ' &
            //            '[-2*pi+ANGMRG, 2*pi]'                     )
            call ctx%errdp  ( '#', mxcor1                                )
            call ctx%errdp  ( '#', mxcor1 * dpr                        )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                   )
            return

         end if

!
!        The maximum longitude is too large by ANGMRG, at worst.
!        Make it less than or equal to 2*pi.
!
         segbds(2,1) = min( twopi, mxcor1 )

!
!        The longitude extent cannot exceed 2*pi.
!
         if ( mxcor1 > (mncor1 + twopi + angmrg) ) then

            call ctx%setmsg ( 'Longitude bounds #:# radians ' &
            //            '(#:# degrees) are too far apart.' )
            call ctx%errdp  ( '#', mxcor1                        )
            call ctx%errdp  ( '#', mxcor2                        )
            call ctx%errdp  ( '#', mxcor1 * dpr                )
            call ctx%errdp  ( '#', mxcor2 * dpr                )
            call ctx%sigerr ( 'SPICE(INVALIDLONEXTENT)'          )
            return

         end if

         if ( mxcor1 < (mncor1 - twopi - angmrg) ) then

            call ctx%setmsg ( 'Longitude bounds #:# radians ' &
            //            '(#:# degrees) are too far apart.' )
            call ctx%errdp  ( '#', mxcor1                        )
            call ctx%errdp  ( '#', mxcor2                        )
            call ctx%errdp  ( '#', mxcor1 * dpr                )
            call ctx%errdp  ( '#', mxcor2 * dpr                )
            call ctx%sigerr ( 'SPICE(INVALIDLONEXTENT)'          )
            return

         end if

         if ( segbds(2,1) > segbds(1,1) ) then
!
!           The upper bound exceeds the lower by at most 2*pi + ANGMRG.
!           Trim the upper bound to make the difference no more than
!           2*pi.
!
            segbds(2,1) = min( segbds(2,1), segbds(1,1)+twopi )

         else if ( segbds(2,1) < segbds(1,1) ) then
!
!           The lower bound exceeds the upper by at most 2*pi + ANGMRG.
!           Advance the upper bound, if necessary, to make the
!           difference no more than 2*pi.
!
            segbds(2,1) = max( segbds(2,1), segbds(1,1)-twopi )

         end if

!
!        Make sure the adjusted longitude bounds don't describe an
!        interval that could be interpreted as having length zero,
!        if the bounds were placed in order. If the lower bound is
!        greater than the upper bound, then the difference between
!        the bounds must not be an integer multiple of 2*pi.
!
         if (      ( segbds(2,1) == segbds(1,1)           ) &
              .or. ( segbds(2,1) == segbds(1,1) - twopi )  ) then

            call ctx%setmsg ( 'After adjustment, minimum longitude ' &
            //            '# radians (# degrees) was equal to ' &
            //            'maximum longitude. Longitude bounds must ' &
            //            'be distinct.'                             )
            call ctx%errdp  ( '#', segbds(1,1)                           )
            call ctx%errdp  ( '#', mncor1 * dpr                        )
            call ctx%sigerr ( 'SPICE(ZEROBOUNDSEXTENT)'                  )
            return

         end if

!
!        Check minimum latitude.
!
         if (      ( mncor2 < -halfpi-angmrg ) &
              .or. ( mncor2 >  halfpi-angmrg ) ) then

            call ctx%setmsg ( 'Minimum latitude # radians (# degrees) ' &
            //            'was outside of valid range ' &
            //            '[-pi/2, pi/2 - ANGMRG]'                   )
            call ctx%errdp  ( '#', mncor2                                )
            call ctx%errdp  ( '#', mncor2 * dpr                        )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                   )
            return

         end if

!
!        Trim the lower latitude bound to make it at least -pi/2.
!
         segbds(1,2) = max( -halfpi, mncor2 )

!
!        Check maximum latitude.
!
         if (      ( mxcor2 < -halfpi+angmrg ) &
              .or. ( mxcor2 >  halfpi+angmrg ) ) then

            call ctx%setmsg ( 'Maximum latitude # radians (# degrees) ' &
            //            'was outside of valid range ' &
            //            '[-pi/2+ANGMRG, pi/2]'                     )
            call ctx%errdp  ( '#', mxcor2                                )
            call ctx%errdp  ( '#', mxcor2 * dpr                        )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                   )
            return

         end if

!
!        Trim the upper latitude bound to make it no more than -pi/2.
!
         segbds(1,2) = max( -halfpi, mncor2 )
         segbds(2,2) = min(  halfpi, mxcor2 )

!
!        The latitude bounds must be in order.
!
         if ( mxcor2 < mncor2 ) then

            call ctx%setmsg ( 'Latitude bounds # and # are out of order.' )
            call ctx%errdp  ( '#',  mncor2                                )
            call ctx%errdp  ( '#',  mxcor2                                )
            call ctx%sigerr ( 'SPICE(BOUNDSOUTOFORDER)'                   )
            return

         end if

         if ( corsys == latsys ) then
!
!           The coordinate system is latitudinal. Check radius
!           bounds.
!
            if ( mncor3 < 0.0_wp ) then

               call ctx%setmsg ( 'Radius lower bound must be ' &
               //            'non-negative but was #.'     )
               call ctx%errdp  ( '#',  mncor3                  )
               call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'      )
               return

            end if

            if ( mxcor3 <= 0.0_wp ) then

               call ctx%setmsg ( 'Radius upper bound must be ' &
               //            'strictly positive but was #.'  )
               call ctx%errdp  ( '#',  mxcor3                    )
               call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'        )
               return

            end if

         end if

         if ( corsys == pdtsys ) then
!
!           The coordinate system is planetodetic. Check the coordinate
!           parameters as well.
!
            if ( corpar(1) <= 0.0_wp ) then

               call ctx%setmsg ( 'Equatorial radius was #; this radius ' &
               //            'must be strictly positive.'            )
               call ctx%errdp  ( '#', corpar(1)                          )
               call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                )
               return

            end if

            if ( corpar(2) >= 1.0_wp ) then

               call ctx%setmsg ( 'Flattening coefficient was #; this ' &
               //            'value must be strictly less than 1.' )
               call ctx%errdp  ( '#', corpar(2)                        )
               call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'              )
               return

            end if

!
!           Make sure the surface of minimum altitude is smooth and
!           non-self-intersecting.
!
            a      = corpar(1)
            b      = a * ( 1 - corpar(2) )

            altlim = max( -(a**2)/b, -(b**2)/a )

            if (  mncor3  <=  altlim  ) then

               call ctx%setmsg ( 'Reference ellipsoid has semi-axis ' &
               //            'lengths # and #. The minimum ' &
               //            'altitude was #. The minimum altitude ' &
               //            'is required to be greater than the ' &
               //            'maximum of {-(A**2)/B, -(B**2)/A}, ' &
               //            'which is #.'                         )
               call ctx%errdp  ( '#', a                                )
               call ctx%errdp  ( '#', b                                )
               call ctx%errdp  ( '#', mncor3                           )
               call ctx%errdp  ( '#', altlim                           )
               call ctx%sigerr ( 'SPICE(DEGENERATESURFACE)'            )
               return

            end if

         end if

!
!        The bounds of the third coordinate, whether radius or altitude,
!        must be in order and must have positive extent.
!
         if ( mxcor3 < mncor3 ) then

            if ( corsys == latsys ) then

               call ctx%setmsg ( 'Radius bounds # and # are out of order' )
            else
               call ctx%setmsg ( 'Altitude bounds # and # are out of ' &
               //            'order.'                                 )
            end if

            call ctx%errdp  ( '#',  mncor3              )
            call ctx%errdp  ( '#',  mxcor3              )
            call ctx%sigerr ( 'SPICE(BOUNDSOUTOFORDER)' )
            return

         end if

         if ( mxcor3 == mncor3 ) then

            if ( corsys == latsys ) then

               call ctx%setmsg ( 'Radius bounds # and # must have ' &
               //            'positive extent but are equal.'  )
            else
               call ctx%setmsg ( 'Altitude bounds # and # must have ' &
               //            'positive extent but are equal.'  )
            end if

            call ctx%errdp  ( '#',  mncor3              )
            call ctx%errdp  ( '#',  mxcor3              )
            call ctx%sigerr ( 'SPICE(ZEROBOUNDSEXTENT)' )
            return

         end if

      else if ( corsys == recsys ) then
!
!        All coordinate bounds must be in strictly increasing order.
!
         if (      ( mxcor1 <= mncor1 ) &
              .or. ( mxcor2 <= mncor2 ) &
              .or. ( mxcor3 <= mncor3 )  ) then

            call ctx%setmsg ( 'Rectangular coordinate bounds must be ' &
            //            'strictly increasing in each dimension. ' &
            //            'The bounds were:  X = #:#; Y = #:#; ' &
            //            'Z = #:#.'                               )
            call ctx%errdp  ( '#', mncor1                              )
            call ctx%errdp  ( '#', mxcor1                              )
            call ctx%errdp  ( '#', mncor2                              )
            call ctx%errdp  ( '#', mxcor2                              )
            call ctx%errdp  ( '#', mncor3                              )
            call ctx%errdp  ( '#', mxcor3                              )
            call ctx%sigerr ( 'SPICE(BOUNDSOUTOFORDER)'                )
            return

         end if

         segbds(1,1) = mncor1
         segbds(2,1) = mxcor1
         segbds(1,2) = mncor2
         segbds(2,2) = mxcor2

      else

         call ctx%setmsg ( 'Coordinate system code # is not recognized.' )
         call ctx%errint ( '#', corsys                                   )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                         )
         return

      end if

!
!     Check the data class.
!
      if (  ( dclass < 1 ) .or. ( dclass > 2 )  ) then

         call ctx%setmsg ( 'Data class # is not recognized.' )
         call ctx%errint ( '#', dclass                       )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
         return

      end if

!
!     Check NV and NP.
!
!     Note that we don't apply Euler's law, since the data
!     set need not represent a complete surface.
!
      if (  ( nv < 1 ) .or. ( nv > maxvrt )  ) then

         call ctx%setmsg ( 'Vertex count NV = #; count must ' &
         //            'be in the range 1:#.'            )
         call ctx%errint ( '#',  nv                          )
         call ctx%errint ( '#',  maxvrt                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'          )
         return

      end if

      if (  ( np < 1 ) .or. ( np > maxplt )  ) then

         call ctx%setmsg ( 'Plate count NP = #; count must ' &
         //            'be in the range 1:#.'            )
         call ctx%errint ( '#',  np                          )
         call ctx%errint ( '#',  maxplt                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'          )
         return

      end if

!
!     Check the vertex indices in the plates.
!
      do i = 1, np

         do j = 1, 3

            k = plates(j,i)

            if ( ( k < 1 ) .or. ( k > nv ) ) then

               call ctx%setmsg ( 'Vertex index # of plate # was #; ' &
               //            'vertex indices must be in the ' &
               //            'range 1:NV. The input NV = #.'   )
               call ctx%errint ( '#', j                            )
               call ctx%errint ( '#', i                            )
               call ctx%errint ( '#', k                            )
               call ctx%errint ( '#', nv                           )
               call ctx%sigerr ( 'SPICE(BADVERTEXINDEX)'           )
               return

            end if

         end do

      end do

!
!     Locate the spatial index elements. Some of the elements are at
!     fixed addresses; for others the addresses must be calculated.
!
!     The two components of the spatial index together contain the
!     following items:
!
!        VGREXT      is an array containing the extents of the voxel
!                    grid in the X, Y, and Z directions of the
!                    body-fixed frame. The extents are measured as
!                    voxel counts.
!
!        ORIGIN      is the position, in the body-fixed, body-centered
!                    reference frame associated with BODY, of the
!                    origin of the both the fine and coarse voxel grids
!                    associated with this model.
!
!        VOXSIZ      is the voxel edge length in km.
!
!        CGRSCL      is the coarse voxel grid scale factor: the edge
!                    length of each coarse voxel is scaled up from the
!                    length of a fine voxel edge by this factor.
!
!        CGRPTR      is an array of pointers associated with this
!                    model's coarse voxel grid; these pointers map
!                    one-dimensional coarse voxel indices to start
!                    indices in the fine voxel pointer list.
!
!        VOXNPT      is the cardinality of the fine voxel pointer list.
!
!        VOXPTR      is the fine voxel pointer list. For each fine
!                    voxel belonging to a non-empty coarse voxel, there
!                    is a pointer in this list that identifies the
!                    start index in VOXPLT of the list of plate indices
!                    associated with this fine voxel.
!
!                    The start index in VOXPTR of the set of pointers
!                    associated with a coarse voxel is given by the
!                    element of CGRPTR associated with that coarse
!                    voxel.
!
!                    Within a given coarse voxel, each fine voxel has
!                    an associated one-dimensional offset from the
!                    corner of the coarse voxel closest to the origin
!                    of the voxel grids. This offset gives the location
!                    in VOXPTR of the plate list pointer for the fine
!                    voxel.
!
!        VOXNPL      is the cardinality of the plate list of the fine
!                    voxel-plate mapping.
!
!        VOXPLT      is the plate list of the fine voxel-plate mapping.
!
!        VTXPTR      is the vertex pointer list.
!
!        VTXNPL      is the cardinality of the plate list of the
!                    vertex-plate mapping.
!
!     Extract double precision elements of the spatial index.
!
      vtxbds = reshape(spaixd(sivtbd:sivtbd+6-1), shape(vtxbds))
      voxori = spaixd(sivxor:sivxor+3-1)

      voxsiz = spaixd(sivxsz)

!
!     Extract scalars and small fixed-size arrays from the integer
!     component of the spatial index.
!
!     Fetch grid extents (in units of whole voxels):
!
      vgrext = spaixi(sivgrx:sivgrx+3-1)

!
!     Fetch coarse grid scale, voxel pointer count, and voxel-plate
!     list count.
!
      cgrscl = spaixi(sicgsc)
      voxnpt = spaixi(sivxnp)
      voxnpl = spaixi(sivxnl)

!
!     Create a pointer to the voxel-plate pointer array.
!
      pvoxpt = sicgrd + maxcgr

!
!     Create a pointer to the voxel-plate list array.
!
      pvoxpl = pvoxpt + voxnpt

!
!     Create a pointer to the vertex pointer array.
!
      pvtxpt = pvoxpl + voxnpl

!
!     Create a pointer to the vertex-plate list array.
!
      pvtxpl = pvtxpt + nv

!
!     Fetch vertex-plate list size.
!
      vtxnpl = spaixi(sivtnl)

!
!     Check the input parameters.
!
!     Make sure the voxel grid extents are within range.
!
      do i = 1, 3

         if (      ( vgrext(i) < 1      ) &
              .or. ( vgrext(i) > maxvox )  ) then

            call ctx%setmsg ( 'Voxel grid extents are = (#, #, #); ' &
         //               'all be in the range 1:#.'             )
            call ctx%errint ( '#',  vgrext(1)                        )
            call ctx%errint ( '#',  vgrext(2)                        )
            call ctx%errint ( '#',  vgrext(3)                        )
            call ctx%errint ( '#',  maxvox                           )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'               )
            return

         end if

      end do

!
!     Make sure the number of voxels NVXTOT is within range.
!
      nvxtot = vgrext(1) * vgrext(2) * vgrext(3)

      if ( nvxtot > maxvox ) then

         call ctx%setmsg ( 'Fine voxel count NVXTOT = #; count must ' &
         //            'be in the range 1:#.'                    )
         call ctx%errint ( '#',  nvxtot                              )
         call ctx%errint ( '#',  maxvox                              )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                  )
         return

      end if

!
!     Check the coarse voxel scale. It must be at least 1, and its
!     cube must not exceed the fine voxel count.
!
      if (      ( cgrscl < 1                ) &
           .or. ( cgrscl > nvxtot**(1.0_wp/3) )  ) then

         call ctx%setmsg ( 'Coarse voxel scale = #; scale must ' &
         //            'be in the range 1:NVXTOT**3, where ' &
         //            'NVXTOT is the total fine voxel count. ' &
         //            'In this case, NVXTOT = #.'               )
         call ctx%errint ( '#',  cgrscl                              )
         call ctx%errint ( '#',  nvxtot                              )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                  )
         return

      end if

!
!     The cube of the coarse scale must divide the total voxel count
!     evenly.
!
      q = nvxtot /   (cgrscl**3)
      r = nvxtot - q*(cgrscl**3)

      if ( r /= 0 ) then

         call ctx%setmsg ( 'Coarse voxel scale = #; the cube of ' &
         //            'the scale must divide NVXTOT evenly, ' &
         //            'where NVXTOT is the total  fine voxel ' &
         //            'count. In this case, NVXTOT = #.'        )
         call ctx%errint ( '#',  cgrscl                              )
         call ctx%errint ( '#',  nvxtot                              )
         call ctx%sigerr ( 'SPICE(INCOMPATIBLESCALE)'                )
         return

      end if

!
!     NCGR        is the number of voxels in the coarse voxel grid
!                 associated with this model. Since each coarse voxel
!                 is a cube containing an integer number of fine
!                 voxels, this number is determined by NVXTOT and
!                 CGRSCL.
!
      ncgr = nvxtot / ( cgrscl**3 )

!
!     Make sure NCGR is within range.
!
      if (  ( ncgr < 1 ) .or. ( ncgr > maxcgr )  ) then

         call ctx%setmsg ( 'Coarse voxel count = #; count must ' &
         //            'be in the range 1:#.'                )
         call ctx%errint ( '#',  ncgr                            )
         call ctx%errint ( '#',  maxcgr                          )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'              )
         return

      end if

!
!     Start a new DLA segment.
!
      call dlabns ( ctx, handle )

      if ( ctx%failed() ) return

!
!     Write the d.p. data to the segment first. In the segregated
!     segment, d.p. data will precede integer data, so less
!     rearrangement will occur this way.
!
!     First, fill in the DSK segment descriptor.
!
      descr = 0.0_wp

      descr ( srfidx ) = surfid
      descr ( ctridx ) = center
      descr ( clsidx ) = dclass
      descr ( typidx ) = 2
      descr ( frmidx ) = frmcde
      descr ( sysidx ) = corsys

      descr(paridx:paridx+nsypar-1) = corpar(1:nsypar)

      descr ( mn1idx ) = segbds(1,1)
      descr ( mx1idx ) = segbds(2,1)
      descr ( mn2idx ) = segbds(1,2)
      descr ( mx2idx ) = segbds(2,2)
      descr ( mn3idx ) = mncor3
      descr ( mx3idx ) = mxcor3
      descr ( btmidx ) = first
      descr ( etmidx ) = last

!
!     Now write the descriptor into the segment.
!
      call dasadd ( ctx, handle, dskdsz, descr )

!
!     Add the voxel grid origin and voxel size.
!     Finish with the vertex data.
!
      call dasadd ( ctx, handle, 6,      vtxbds )
      call dasadd ( ctx, handle, 3,      voxori )
      call dasadd ( ctx, handle, 1, [ voxsiz ] )
      call dasadd ( ctx, handle, 3*nv,   vrtces )

!
!     Next add the integer data to the segment.
!
!     NV is the number of vertices.
!     NP is the number of plates.
!     NVXTOT is the number of voxels in the spatial index.
!
      call dasadi ( ctx, handle, 1, [ nv ] )
      call dasadi ( ctx, handle, 1, [ np ] )
      call dasadi ( ctx, handle, 1, [ nvxtot ] )
      call dasadi ( ctx, handle, 3,      vgrext )
      call dasadi ( ctx, handle, 1, [ cgrscl ] )
      call dasadi ( ctx, handle, 1, [ voxnpt ] )
      call dasadi ( ctx, handle, 1, [ voxnpl ] )
      call dasadi ( ctx, handle, 1, [ vtxnpl ] )
      call dasadi ( ctx, handle, 3*np,   plates )
      call dasadi ( ctx, handle, voxnpt, spaixi(pvoxpt) )
      call dasadi ( ctx, handle, voxnpl, spaixi(pvoxpl) )
      call dasadi ( ctx, handle, nv,     spaixi(pvtxpt) )
      call dasadi ( ctx, handle, vtxnpl, spaixi(pvtxpl) )
      call dasadi ( ctx, handle, ncgr,   spaixi(sicgrd) )

!
!     End the segment.
!
      call dlaens ( ctx, handle )

      return
      end subroutine dskw02

end submodule pepper_dsk_write
