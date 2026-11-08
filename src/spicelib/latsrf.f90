!$Procedure LATSRF ( Latitudinal grid to surface points )

      subroutine latsrf ( method, target, et, &
                          fixref, npts,   lonlat, srfpts )

! $ Abstract
!
!     Map array of planetocentric longitude/latitude coordinate pairs
!     to surface points on a specified target body.
!
!     The surface of the target body may be represented by a triaxial
!     ellipsoid or by topographic data provided by DSK files.
!
! $ Required_Reading
!
!     DSK
!     FRAMES
!     PCK
!     SPK
!     TIME
!
! $ Keywords
!
!     COORDINATES
!     DSK
!     GEOMETRY
!     SURFACE
!
! $ Declarations

      implicit none

      include 'dsk.inc'
      include 'gf.inc'
      include 'zzctr.inc'
      include 'zzdsk.inc'

      character*(*)         method
      character*(*)         target
      double precision      et
      character*(*)         fixref
      integer               npts
      double precision      lonlat ( 2, * )
      double precision      srfpts ( 3, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TARGET     I   Name of target body.
!     ET         I   Epoch in TDB seconds past J2000 TDB.
!     FIXREF     I   Body-fixed, body-centered target body frame.
!     NPTS       I   Number of coordinate pairs in input array.
!     LONLAT     I   Array of longitude/latitude coordinate pairs.
!     SRFPTS     O   Array of surface points.
!
! $ Detailed_Input
!
!     METHOD   is a short string providing parameters defining
!              the computation method to be used. In the syntax
!              descriptions below, items delimited by brackets
!              are optional.
!
!              METHOD may be assigned the following values:
!
!                 'ELLIPSOID'
!
!                    The surface point computation uses a triaxial
!                    ellipsoid to model the surface of the target
!                    body. The ellipsoid's radii must be available
!                    in the kernel pool.
!
!                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]'
!
!                    The surface point computation uses topographic
!                    data to model the surface of the target body.
!                    These data must be provided by loaded DSK
!                    files.
!
!                    The surface list specification is optional. The
!                    syntax of the list is
!
!                       <surface 1> [, <surface 2>...]
!
!                    If present, it indicates that data only for the
!                    listed surfaces are to be used; however, data
!                    need not be available for all surfaces in the
!                    list. If absent, loaded DSK data for any surface
!                    associated with the target body are used.
!
!                    The surface list may contain surface names or
!                    surface ID codes. Names containing blanks must
!                    be delimited by double quotes, for example
!
!                       SURFACES = "Mars MEGDR 128 PIXEL/DEG"
!
!                    If multiple surfaces are specified, their names
!                    or IDs must be separated by commas.
!
!                    See the $Particulars section below for details
!                    concerning use of DSK data.
!
!
!              Neither case nor white space are significant in
!              METHOD, except within double-quoted strings. For
!              example, the string ' eLLipsoid ' is valid.
!
!              Within double-quoted strings, blank characters are
!              significant, but multiple consecutive blanks are
!              considered equivalent to a single blank. Case is
!              not significant. So
!
!                 "Mars MEGDR 128 PIXEL/DEG"
!
!              is equivalent to
!
!                 " mars megdr  128  pixel/deg "
!
!              but not to
!
!                 "MARS MEGDR128PIXEL/DEG"
!
!
!     TARGET   is the name of the target body. TARGET is
!              case-insensitive, and leading and trailing blanks in
!              TARGET are not significant. Optionally, you may
!              supply a string containing the integer ID code for
!              the object. For example both 'MOON' and '301' are
!              legitimate strings that indicate the Moon is the
!              target body.
!
!              When the target body's surface is represented by a
!              tri-axial ellipsoid, this routine assumes that a
!              kernel variable representing the ellipsoid's radii is
!              present in the kernel pool. Normally the kernel
!              variable would be defined by loading a PCK file.
!
!
!     ET       is the epoch for which target surface data will be
!              selected, if the surface is modeled using DSK data.
!              In this case, only segments having time coverage that
!              includes the epoch ET will be used.
!
!              ET is ignored if the target is modeled as an
!              ellipsoid.
!
!              ET is expressed as TDB seconds past J2000 TDB.
!
!
!     FIXREF   is the name of a body-fixed reference frame centered
!              on the target body.
!
!              If the target shape is given by DSK data, FIXREF may
!              designate any such frame supported by the SPICE
!              system, including built-in frames (documented in the
!              Frames Required Reading) and frames defined by a
!              loaded frame kernel (FK).
!
!              When the target surface is modeled as an ellipsoid,
!              the reference frame designated by FIXREF (described
!              below) must have its coordinate axes aligned with the
!              respective principal axes of the reference ellipsoid.
!
!              The string FIXREF is case-insensitive, and leading
!              and trailing blanks in FIXREF are not significant.
!
!              The output surface points in the array SRFPTS will be
!              expressed relative to this reference frame.
!
!
!     NPTS     is the number of coordinate pairs in the array LONLAT.
!
!
!     LONLAT   is an array of pairs of planetocentric longitudes and
!              latitudes of surface points. Elements
!
!                 LONLAT(1,I)
!                 LONLAT(2,I)
!
!              are, respectively, the planetocentric longitude and
!              latitude of the Ith surface point.
!
!              The units of longitude and latitude are radians.
!
! $ Detailed_Output
!
!     SRFPTS   is an array of target body surface points
!              corresponding to the pairs of coordinates in the
!              input LONLAT array. Elements
!
!                 SRFPTS(1,I)
!                 SRFPTS(2,I)
!                 SRFPTS(3,I)
!
!              are the Cartesian coordinates, expressed in the
!              reference frame designated by FIXREF, of the surface
!              point corresponding to the Ith pair of input
!              coordinates.
!
!              If there are multiple solutions for a given input
!              coordinate pair, this routine will return the point
!              at those coordinates having the greatest distance
!              from the origin of the coordinate system.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the target body name input string cannot be converted to an
!         integer ID code, the error SPICE(IDCODENOTFOUND) is signaled.
!
!     2)  If the input target body-fixed frame FIXREF is not
!         recognized, the error SPICE(NOFRAME) is signaled. A frame
!         name may fail to be recognized because a required frame
!         specification kernel has not been loaded; another cause is a
!         misspelling of the frame name.
!
!     3)  If the input frame FIXREF is not centered at the target body,
!         the error SPICE(INVALIDFRAME) is signaled.
!
!     4)  If data are not available to convert between the frame
!         FIXREF and the frame of a DSK segment of interest, an error
!         is signaled by a routine in the call tree of this
!         routine.
!
!     5)  If the input argument METHOD cannot be parsed, an error
!         is signaled by either this routine or a routine in
!         the call tree of this routine.
!
!     6)  If the computation method specifies an ellipsoidal target
!         model, and if triaxial radii of the target body have not been
!         loaded into the kernel pool prior to calling LATSRF, an error
!         is signaled by a routine in the call tree of this routine.
!
!     7)  If the computation method specifies an ellipsoidal target
!         model, and if any of the radii of the target body are
!         non-positive, an error is signaled by a routine in the call
!         tree of this routine. The target must be an extended body.
!
!     8)  If METHOD specifies that the target surface is represented by
!         DSK data, and no DSK files are loaded for the specified
!         target, an error is signaled by a routine in the call tree
!         of this routine.
!
!     9)  If METHOD specifies that the target surface is represented
!         by DSK data, and data representing the portion of the surface
!         corresponding to the coordinates provided in LONLAT are not
!         available, an error is signaled by a routine in the call
!         tree of this routine.
!
!     10) If a surface point cannot be computed because the ray
!         corresponding to a longitude/latitude pair fails to intersect
!         the target surface as defined by the plate model, the error
!         SPICE(NOINTERCEPT) is signaled.
!
!     11) If the surface point corresponding to a longitude/latitude
!         pair in LONLAT does not have matching longitude and latitude
!         (because it is on the opposite side of the origin), the error
!         SPICE(SHAPENOTSUPPORTED) is signaled.
!
!     12) If the radii are not available in the kernel pool, an error is
!         signaled by a routine in the call tree of this routine.
!
!     13) If the target shape is "ellipsoid" and not all radii of the
!         ellipsoid are strictly positive, the error
!         SPICE(BADAXISLENGTH) is signaled.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!     -  Shape data for the target body:
!
!           PCK data:
!
!              If the target shape is modeled as an ellipsoid,
!              triaxial radii for the target body must be loaded into
!              the kernel pool. Typically this is done by loading a
!              text PCK file via FURNSH.
!
!           DSK data:
!
!              If the target shape is modeled by DSK data, DSK files
!              containing topographic data for the target body must be
!              loaded. If a surface list is specified, data for at
!              least one of the listed surfaces must be loaded.
!
!     -  Target body orientation data: these may be provided in a
!        text or binary PCK file. In some cases, target body
!        orientation may be provided by one more more CK files. In
!        either case, data are made available by loading the files
!        via FURNSH.
!
!     The following data may be required:
!
!     -  Frame data: if a frame definition is required to convert
!        between the body-fixed frame of the target and the frame of
!        a DSK segment providing topographic data, that definition
!        must be available in the kernel pool. Typically the
!        definition is supplied by loading a frame kernel via FURNSH.
!
!     -  Surface name-ID associations: if surface names are specified
!        in METHOD, the association of these names with their
!        corresponding surface ID codes must be established by
!        assignments of the kernel variables
!
!           NAIF_SURFACE_NAME
!           NAIF_SURFACE_CODE
!           NAIF_SURFACE_BODY
!
!        Normally these associations are made by loading a text
!        kernel containing the necessary assignments. An example of
!        such a set of assignments is
!
!           NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG'
!           NAIF_SURFACE_CODE += 1
!           NAIF_SURFACE_BODY += 499
!
!     -  SCLK data: if the target body's orientation is provided by
!        CK files, an associated SCLK kernel must be loaded.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This routine is intended to be used for target body surfaces that
!     have a unique radius for each pair of planetocentric longitude
!     and latitude coordinates.
!
!     If the target surface is represented by topographic data, it is
!     possible for there to be multiple surface points at a given
!     planetocentric longitude and latitude. For example, this can
!     occur if the surface has features such as cliffs, caves, or
!     arches.
!
!     For more complex surfaces, the routine
!
!        DSKSXV {DSK, ray-surface intercept, vectorized}
!
!     may be more suitable. That routine works with rays having vertices
!     anywhere outside of the target body.
!
!
!     Planetocentric coordinates
!     ==========================
!
!     Planetocentric longitude and latitude are defined as follows:
!
!        Longitude of a point P is the angle between the prime meridian
!        and the meridian containing P. The direction of increasing
!        longitude is from the +X axis towards the +Y axis.
!
!        Latitude of a point P is the angle from the XY plane of the
!        ray from the origin through the point.
!
!
!     Using DSK data
!     ==============
!
!        DSK loading and unloading
!        -------------------------
!
!        DSK files providing data used by this routine are loaded by
!        calling FURNSH and can be unloaded by calling UNLOAD or
!        KCLEAR. See the documentation of FURNSH for limits on numbers
!        of loaded DSK files.
!
!        For run-time efficiency, it's desirable to avoid frequent
!        loading and unloading of DSK files. When there is a reason to
!        use multiple versions of data for a given target body---for
!        example, if topographic data at varying resolutions are to be
!        used---the surface list can be used to select DSK data to be
!        used for a given computation. It is not necessary to unload
!        the data that are not to be used. This recommendation presumes
!        that DSKs containing different versions of surface data for a
!        given body have different surface ID codes.
!
!
!        DSK data priority
!        -----------------
!
!        A DSK coverage overlap occurs when two segments in loaded DSK
!        files cover part or all of the same domain---for example, a
!        given longitude-latitude rectangle---and when the time
!        intervals of the segments overlap as well.
!
!        When DSK data selection is prioritized, in case of a coverage
!        overlap, if the two competing segments are in different DSK
!        files, the segment in the DSK file loaded last takes
!        precedence. If the two segments are in the same file, the
!        segment located closer to the end of the file takes
!        precedence.
!
!        When DSK data selection is unprioritized, data from competing
!        segments are combined. For example, if two competing segments
!        both represent a surface as sets of triangular plates, the
!        union of those sets of plates is considered to represent the
!        surface.
!
!        Currently only unprioritized data selection is supported.
!        Because prioritized data selection may be the default behavior
!        in a later version of the routine, the UNPRIORITIZED keyword is
!        required in the METHOD argument.
!
!
!        Syntax of the METHOD input argument
!        -----------------------------------
!
!        The keywords and surface list in the METHOD argument
!        are called "clauses." The clauses may appear in any
!        order, for example
!
!           DSK/<surface list>/UNPRIORITIZED
!           DSK/UNPRIORITIZED/<surface list>
!           UNPRIORITIZED/<surface list>/DSK
!
!        The simplest form of the METHOD argument specifying use of
!        DSK data is one that lacks a surface list, for example:
!
!           'DSK/UNPRIORITIZED'
!
!        For applications in which all loaded DSK data for the target
!        body are for a single surface, and there are no competing
!        segments, the above string suffices. This is expected to be
!        the usual case.
!
!        When, for the specified target body, there are loaded DSK
!        files providing data for multiple surfaces for that body, the
!        surfaces to be used by this routine for a given call must be
!        specified in a surface list, unless data from all of the
!        surfaces are to be used together.
!
!        The surface list consists of the string
!
!           SURFACES =
!
!        followed by a comma-separated list of one or more surface
!        identifiers. The identifiers may be names or integer codes in
!        string format. For example, suppose we have the surface
!        names and corresponding ID codes shown below:
!
!           Surface Name                              ID code
!           ------------                              -------
!           'Mars MEGDR 128 PIXEL/DEG'                1
!           'Mars MEGDR 64 PIXEL/DEG'                 2
!           'Mars_MRO_HIRISE'                         3
!
!        If data for all of the above surfaces are loaded, then
!        data for surface 1 can be specified by either
!
!           'SURFACES = 1'
!
!        or
!
!           'SURFACES = "Mars MEGDR 128 PIXEL/DEG"'
!
!        Double quotes are used to delimit the surface name because
!        it contains blank characters.
!
!        To use data for surfaces 2 and 3 together, any
!        of the following surface lists could be used:
!
!           'SURFACES = 2, 3'
!
!           'SURFACES = "Mars MEGDR  64 PIXEL/DEG", 3'
!
!           'SURFACES = 2, Mars_MRO_HIRISE'
!
!           'SURFACES = "Mars MEGDR 64 PIXEL/DEG", Mars_MRO_HIRISE'
!
!        An example of a METHOD argument that could be constructed
!        using one of the surface lists above is
!
!           'DSK/UNPRIORITIZED/SURFACES = "Mars MEGDR 64 PIXEL/DEG", 3'
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input,
!     the compiler and supporting libraries, and the machine specific
!     arithmetic implementation.
!
!
!     1) In the following example program, a DSK file containing a
!        type 2 segment is used to provide a plate model representation
!        of the surface of Phobos.
!
!        Find the surface points on a target body corresponding to a
!        given planetocentric longitude/latitude grid.
!
!
!        Example code begins here.
!
!
!              PROGRAM LATSRF_EX1
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      RPD
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1   = '(1X,A,F11.6)' )
!
!              INTEGER               BDNMLN
!              PARAMETER           ( BDNMLN = 36 )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               FRNMLN
!              PARAMETER           ( FRNMLN = 32 )
!
!              INTEGER               LNSIZE
!              PARAMETER           ( LNSIZE = 79 )
!
!              INTEGER               MAXN
!              PARAMETER           ( MAXN   = 100 )
!
!              INTEGER               MTHLEN
!              PARAMETER           ( MTHLEN = 80 )
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    DSK
!              CHARACTER*(FRNMLN)    FIXREF
!              CHARACTER*(MTHLEN)    METHOD
!              CHARACTER*(LNSIZE)    OUTLIN
!              CHARACTER*(BDNMLN)    TARGET
!
!              DOUBLE PRECISION      DLAT
!              DOUBLE PRECISION      DLON
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      GRID   ( 2, MAXN )
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LAT0
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      LON0
!              DOUBLE PRECISION      SRFPTS ( 3, MAXN )
!              DOUBLE PRECISION      XLAT
!              DOUBLE PRECISION      XLON
!              DOUBLE PRECISION      XR
!
!              INTEGER               I
!              INTEGER               J
!              INTEGER               N
!              INTEGER               NLAT
!              INTEGER               NLON
!
!        C
!        C     Set target, reference frame, and epoch.
!        C
!              TARGET = 'PHOBOS'
!              FIXREF = 'IAU_PHOBOS'
!              ET     = 0.D0
!        C
!        C     Use DSK data to represent the surface.
!        C
!              METHOD = 'DSK/UNPRIORITIZED'
!        C
!        C     Set the grid dimensions.
!        C
!              NLON   = 3
!              NLAT   = 2
!        C
!        C     Derive evenly spaced grid separations and starting
!        C     values in the longitude and latitude dimensions.
!        C     Units are degrees.
!        C
!              LAT0 = 90.D0
!              LON0 =  0.D0
!
!              DLAT = 180.D0 / (NLAT + 1)
!              DLON = 360.D0 /  NLON
!        C
!        C     Prompt for the name of the DSK to read.
!        C
!              CALL PROMPT ( 'Enter DSK name    > ', DSK )
!        C
!        C     Load the DSK file.
!        C
!              CALL FURNSH ( DSK )
!        C
!        C     Now generate the grid points.  We generate
!        C     points along latitude bands, working from
!        C     north to south.  The latitude range is selected
!        C     to range from +30 to -30 degrees.  Longitude
!        C     ranges from 0 to 240 degrees.  The increment
!        C     is 90 degrees for latitude and 120 degrees for
!        C     longitude.
!        C
!              N = 0
!
!              DO I = 1, NLAT
!
!                 LAT = RPD() * ( LAT0 - I*DLAT )
!
!                 DO J = 1, NLON
!
!                    N   = N + 1
!                    LON = RPD() * ( LON0 + (J-1)*DLON )
!
!                    GRID(1,N) = LON
!                    GRID(2,N) = LAT
!
!                 END DO
!
!              END DO
!        C
!        C     Find the surface points corresponding to the grid points.
!        C
!              CALL LATSRF ( METHOD, TARGET, ET,
!             .              FIXREF, N,      GRID, SRFPTS )
!        C
!        C     Print out the surface points in latitudinal
!        C     coordinates and compare the derived lon/lat values
!        C     to those of the input grid.
!        C
!              DO I = 1, N
!        C
!        C        Use RECRAD rather than RECLAT to produce
!        C        non-negative longitudes.
!        C
!                 CALL RECRAD ( SRFPTS(1,I), XR, XLON, XLAT )
!
!                 WRITE (*,*) ' '
!
!                 OUTLIN = 'Intercept for grid point #:'
!                 CALL REPMI ( OUTLIN, '#', I, OUTLIN )
!
!                 WRITE(*,*)  OUTLIN
!                 OUTLIN = '  Cartesian coordinates: (#, #, #)'
!
!                 DO J = 1, 3
!                    CALL REPMF( OUTLIN, '#', SRFPTS(J,I),
!             .                  8,      'F', OUTLIN      )
!                 END DO
!
!                 WRITE (*,*) OUTLIN
!
!                 WRITE (*,*)    '  Latitudinal Coordinates:'
!                 WRITE (*,FMT1) '   Longitude (deg): ', XLON*DPR()
!                 WRITE (*,FMT1) '   Latitude  (deg): ', XLAT*DPR()
!                 WRITE (*,FMT1) '   Radius     (km): ', XR
!                 WRITE (*,*)    ' '
!                 WRITE (*,*)    '  Original Grid Coordinates:'
!                 WRITE (*,FMT1) '   Longitude (deg): ', GRID(1,I)*DPR()
!                 WRITE (*,FMT1) '   Latitude  (deg): ', GRID(2,I)*DPR()
!
!              END DO
!              WRITE (*,*) ' '
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos512.bds, the output
!        was:
!
!
!        Enter DSK name    > phobos512.bds
!
!         Intercept for grid point 1:
!           Cartesian coordinates: (9.5706817, 0.00000000, 5.5256356)
!           Latitudinal Coordinates:
!            Longitude (deg):    0.000000
!            Latitude  (deg):   30.000000
!            Radius     (km):   11.051271
!
!           Original Grid Coordinates:
!            Longitude (deg):    0.000000
!            Latitude  (deg):   30.000000
!
!         Intercept for grid point 2:
!           Cartesian coordinates: (-4.7586430, 8.2422114, 5.4948076)
!           Latitudinal Coordinates:
!            Longitude (deg):  120.000000
!            Latitude  (deg):   30.000000
!            Radius     (km):   10.989615
!
!           Original Grid Coordinates:
!            Longitude (deg):  120.000000
!            Latitude  (deg):   30.000000
!
!         Intercept for grid point 3:
!           Cartesian coordinates: (-4.5704268, -7.9162115, 5.2774743)
!           Latitudinal Coordinates:
!            Longitude (deg):  240.000000
!            Latitude  (deg):   30.000000
!            Radius     (km):   10.554949
!
!           Original Grid Coordinates:
!            Longitude (deg):  240.000000
!            Latitude  (deg):   30.000000
!
!         Intercept for grid point 4:
!           Cartesian coordinates: (10.959385, 0.00000000, -6.3274040)
!           Latitudinal Coordinates:
!            Longitude (deg):    0.000000
!            Latitude  (deg):  -30.000000
!            Radius     (km):   12.654808
!
!           Original Grid Coordinates:
!            Longitude (deg):    0.000000
!            Latitude  (deg):  -30.000000
!
!         Intercept for grid point 5:
!           Cartesian coordinates: (-4.8830077, 8.4576174, -5.6384116)
!           Latitudinal Coordinates:
!            Longitude (deg):  120.000000
!            Latitude  (deg):  -30.000000
!            Radius     (km):   11.276823
!
!           Original Grid Coordinates:
!            Longitude (deg):  120.000000
!            Latitude  (deg):  -30.000000
!
!         Intercept for grid point 6:
!           Cartesian coordinates: (-4.5322568, -7.8500991, -5.2333994)
!           Latitudinal Coordinates:
!            Longitude (deg):  240.000000
!            Latitude  (deg):  -30.000000
!            Radius     (km):   10.466799
!
!           Original Grid Coordinates:
!            Longitude (deg):  240.000000
!            Latitude  (deg):  -30.000000
!
!
! $ Restrictions
!
!     1)  This routine assumes that the origin of the body-fixed
!         reference frame associated with the target body is located in
!         the interior of that body.
!
!     2)  The results returned by this routine may not be meaningful
!         if the target surface has multiple surface points associated
!         with some (longitude, latitude) coordinates.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (NJB) (JDR)
!
!        Bug fix: removed spurious blank from long error
!        message.
!
!        Edited the header to comply with NAIF standard. Modified
!        the grid dimensions and output format in the code example to
!        reduce the number of lines and their length in the solution.
!
!-    SPICELIB Version 1.0.0, 21-FEB-2017 (NJB)
!
!        Original version 01-JUL-2016 (NJB)
!
!-&


! $ Index_Entries
!
!     map latitudinal coordinates to Cartesian surface points
!     map latitudinal coordinates to DSK surface points
!
!-&


!
!     SPICELIB functions
!
      double precision      dpr
      double precision      vdot

      logical               eqstr
      logical               failed
      logical               return

!
!     Local parameters
!
      character*(*)         rname
      parameter           ( rname = 'LATSRF' )

      integer               bdnmln
      parameter           ( bdnmln = 36 )

      integer               frnmln
      parameter           ( frnmln = 32 )

!
!     Local variables
!
      character*(cvtlen)    lmbtyp
      character*(mthlen)    prvmth
      character*(shplen)    shpstr
      character*(sublen)    subtyp
      character*(tmtlen)    trmtyp

      double precision      r
      double precision      raydir ( 3 )
      double precision      x      ( 3 )

      integer               fixfid
      integer               fxcent
      integer               fxclss
      integer               fxclid
      integer               i
      integer               n
      integer               nsurf
      integer               shape
      integer               srfctr ( ctrsiz )
      integer               srflst ( maxsrf )
      integer               trgcde

      logical               first
      logical               fnd
      logical               pri
      logical               surfup

!
!     Saved name/ID item declarations.
!
      integer               svctr1 ( ctrsiz )
      character*(bdnmln)    svtarg
      integer               svtcde
      logical               svfnd1

!
!     Saved frame name/ID item declarations.
!
      integer               svctr2 ( ctrsiz )
      character*(frnmln)    svfref
      integer               svfxfc

!
!     Saved target radius values.
!
      integer               svctr3 ( ctrsiz )
      integer               svprvt
      double precision      svradi ( 3 )

!
!     Saved name/ID items.
!
      save                  svctr1
      save                  svtarg
      save                  svtcde
      save                  svfnd1
!
!     Saved frame name/ID items.
!
      save                  svctr2
      save                  svfref
      save                  svfxfc

!
!     Saved target radius values.
!
      save                  svctr3
      save                  svprvt
      save                  svradi

!
!     Saved values
!
      save                  nsurf
      save                  first
      save                  pri
      save                  prvmth
      save                  shape
      save                  srfctr
      save                  srflst

!
!     Initial values
!
      data                  first  / .true. /
      data                  prvmth / ' '    /
      data                  svprvt / 0      /



      if ( return() ) then
         return
      end if

      call chkin ( rname )

      if ( first ) then
!
!        Initialize local surface counter.
!
         call zzctruin ( srfctr )
!
!        Initialize target, frame, and radius counters.
!
         call zzctruin ( svctr1 )
         call zzctruin ( svctr2 )
         call zzctruin ( svctr3 )

      end if

!
!     Obtain integer code for the target.
!
      call zzbods2c ( svctr1, svtarg, svtcde, svfnd1, &
                      target, trgcde, fnd    )

      if ( .not. fnd ) then

         call setmsg ( 'The target, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit, or that you ' &
         //            'failed to load a kernel containing a ' &
         //            'name-ID mapping for this body.'           )
         call errch  ( '#', target                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( rname                                      )
         return

      end if

!
!     Determine the attributes of the frame designated by FIXREF.
!
      call zznamfrm ( svctr2, svfref, svfxfc, fixref, fixfid )

      call frinfo ( fixfid, fxcent, fxclss, fxclid, fnd )

      if ( failed() ) then
         call chkout ( rname )
         return
      end if

      if ( .not. fnd ) then

         call setmsg ( 'Reference frame # is not recognized by ' // &
                       'the SPICE frame subsystem. Possibly '    // &
                       'a required frame definition kernel has ' // &
                       'not been loaded.'                        )
         call errch  ( '#',  fixref                              )
         call sigerr ( 'SPICE(NOFRAME)'                          )
         call chkout ( rname                                     )
         return

      end if

!
!     Make sure that FIXREF is centered at the target body's center.
!
      if ( fxcent .ne. trgcde ) then

         call setmsg ( 'Reference frame # is not centered at the ' &
         //            'target body #. The ID code of the frame ' &
         //            'center is #.'                             )
         call errch  ( '#',  fixref                               )
         call errch  ( '#',  target                               )
         call errint ( '#',  fxcent                               )
         call sigerr ( 'SPICE(INVALIDFRAME)'                      )
         call chkout ( rname                                      )
         return

      end if

!
!     Check whether the surface name/ID mapping has been updated.
!
      call zzsrftrk ( srfctr, surfup )

!
!     Initialize the SINCPT utility package for the next computation.
!     The choice of initialization routine depends on the target
!     surface type.
!
      if ( first  .or.  surfup  .or.  ( method .ne. prvmth ) ) then
!
!        Set the previous method string to an invalid value, so it
!        cannot match any future, valid input. This will force this
!        routine to parse the input method on the next call if any
!        failure occurs in this branch. Once success is assured, we can
!        record the current method in the previous method string.
!
         prvmth = ' '

!
!        Parse the method string. If the string is valid, the
!        outputs SHAPE and SUBTYP will always be be set. However,
!        SUBTYP is not used in this routine.
!
!        For DSK shapes, the surface list array and count will be set
!        if the method string contains a surface list.
!
         call zzprsmet ( trgcde, method, maxsrf, shpstr, subtyp, &
                         pri,    nsurf,  srflst, lmbtyp, trmtyp )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

         if (  eqstr( shpstr, 'ELLIPSOID' )  ) then

            shape = ellshp

         else if (  eqstr( shpstr, 'DSK' )  ) then

            shape = dskshp

         else
!
!           This is a backstop check.
!
            call setmsg ( '[1] Returned shape value from method ' &
            //            'string was <#>.'                      )
            call errch  ( '#', shpstr                            )
            call sigerr ( 'SPICE(BUG)'                           )
            call chkout ( rname                                  )
            return

         end if

!
!        There should be no subtype specification in the method
!        string.
!
         if ( subtyp .ne. ' ' ) then

            call setmsg ( 'Spurious sub-observer point type <#> ' &
            //            'was present in the method string #. ' &
            //            'The sub-observer type is valid in ' &
            //            'the method strings for SUBPNT and ' &
            //            'SUBSLR, but is not applicable for LATSRF.' )
            call errch  ( '#', subtyp                                 )
            call errch  ( '#', method                                 )
            call sigerr ( 'SPICE(INVALIDMETHOD)'                      )
            call chkout ( rname                                       )
            return

         end if

         prvmth = method

      end if

!
!     At this point, the first pass actions were successful.
!
      first = .false.

!
!     Check the target body shape.
!
      if ( shape .eq. ellshp ) then

         if ( trgcde .ne. svprvt ) then
!
!           Reset counter to force lookup.
!
            call zzctruin ( svctr3 )

         end if
!
!        Look up target radii using counter.
!
         call zzbodvcd ( trgcde, 'RADII', 3, svctr3, n, svradi )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

         if (  min( svradi(1), svradi(2), svradi(3) ) .le. 0.d0 ) then

            call setmsg ( 'Body # radii should be positive but ' &
            //            'were # # #.'                         )
            call errch  ( '#',  target                          )
            call errdp  ( '#',  svradi(1)                       )
            call errdp  ( '#',  svradi(2)                       )
            call errdp  ( '#',  svradi(3)                       )
            call sigerr ( 'SPICE(BADAXISLENGTH)'                )
            call chkout ( rname                                 )
            return

         end if

!
!        The radii are valid. Update the previous target ID.
!
         svprvt = trgcde

!
!        Generate surface points.
!
         do i = 1, npts
!
!           Let X be a point having norm 1 and located at the Ith input
!           longitude and latitude.
!
            call latrec ( 1.d0, lonlat(1,i), lonlat(2,i), x )
!
!           Scale X to place it on the ellipsoid surface.
!
            call edpnt ( x,         svradi(1), &
                         svradi(2), svradi(3), srfpts(1,i) )

            if ( failed() ) then
               call chkout ( rname )
               return
            end if

         end do



      else if  ( shape .eq. dskshp ) then
!
!        Initialize the DSK ray-surface intercept algorithm to use a
!        DSK model for the surface of the target body.
!
         call zzsudski ( trgcde, nsurf, srflst, fixfid )

!
!        Get the radius of an outer bounding sphere for the body. Scale
!        up to avoid getting too close to the target surface.
!
         call zzmaxrad ( r )
         r = 2 * r

         if ( failed() ) then
            call chkout ( rname )
            return
         end if


!
!        Generate surface points.
!
         do i = 1, npts

            call latrec ( r, lonlat(1,i), lonlat(2,i), x )

            call vminus ( x, raydir )
!
!           Find the ray-surface intercept for the ray emanating
!           from X and pointing in the -X direction, where the
!           surface is represented by DSK data for the specified
!           body and surface list (the surface list was supplied
!           to ZZSUDSKI).
!
            call zzraysfx ( x, raydir, et, srfpts(1,i), fnd )

            if ( failed() ) then
               call chkout ( rname )
               return
            end if

            if ( .not. fnd ) then

               call setmsg ( 'No surface point was found on body # ' &
               //            'at planetocentric longitude # (# deg), ' &
               //            'latitude # (# deg). This problem may ' &
               //            'be due to insufficient DSK data having ' &
               //            'been loaded for the body. It also ' &
               //            'could be due to the body having a ' &
               //            'shape not suitable for this computation' &
               //            ', for example, a torus.'                 )
               call errch  ( '#', target                               )
               call errdp  ( '#', lonlat(1,i)                          )
               call errdp  ( '#', lonlat(1,i) * dpr()                  )
               call errdp  ( '#', lonlat(2,i)                          )
               call errdp  ( '#', lonlat(2,i) * dpr()                  )
               call sigerr ( 'SPICE(POINTNOTFOUND)'                    )
               call chkout ( rname                                     )
               return

            end if

!
!           Make sure the intercept is on the correct side of the
!           object.
!
            if (  vdot( x, srfpts(1,i) )  .lt. 0.d0  ) then

               call setmsg ( 'A surface point was found on body # ' &
               //            'for the input planetocentric ' &
               //            'longitude # (# deg), ' &
               //            'latitude # (# deg), but this point ' &
               //            'is on the opposite side of the body. ' &
               //            'This likely indicates the ' &
               //            'the body does not contain the origin ' &
               //            'of the coordinate system. LATSRF does ' &
               //            'not work with such surfaces. Consider ' &
               //            'using DSKSXV for this computation.'    )
               call errch  ( '#', target                             )
               call errdp  ( '#', lonlat(1,i)                        )
               call errdp  ( '#', lonlat(1,i) * dpr()                )
               call errdp  ( '#', lonlat(2,i)                        )
               call errdp  ( '#', lonlat(2,i) * dpr()                )
               call sigerr ( 'SPICE(SHAPENOTSUPPORTED)'              )
               call chkout ( rname                                   )
               return

            end if

         end do


      else

         call setmsg ( 'Input method <#> does not specify ' &
         //            'the target shape as either ELLIPSOID ' &
         //            'or DSK.'                               )
         call errch  ( '#', method                             )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                   )
         call chkout ( rname                                   )
         return

      end if

      call chkout ( rname )
      return
      end
