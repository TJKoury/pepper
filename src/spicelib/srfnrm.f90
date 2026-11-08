!$Procedure SRFNRM ( Map surface points to outward normal vectors )

      subroutine srfnrm ( method, target, et,    fixref, &
                          npts,   srfpts, normls        )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Map array of surface points on a specified target body to
!     the corresponding unit length outward surface normal vectors.
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
      include 'dsktol.inc'
      include 'gf.inc'
      include 'zzctr.inc'
      include 'zzdsk.inc'

      character*(*)         method
      character*(*)         target
      real(wp)              et
      character*(*)         fixref
      integer(ip)           npts
      real(wp)              srfpts ( 3, * )
      real(wp)              normls ( 3, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TARGET     I   Name of target body.
!     ET         I   Epoch in TDB seconds past J2000 TDB.
!     FIXREF     I   Body-fixed, body-centered target body frame.
!     NPTS       I   Number of surface points in input array.
!     SRFPTS     I   Array of surface points.
!     NORMLS     O   Array of outward, unit length normal vectors.
!     PTMEMM     P   Default point-surface membership margin.
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
!                    The normal vector computation uses a triaxial
!                    ellipsoid to model the surface of the target
!                    body. The ellipsoid's radii must be available
!                    in the kernel pool.
!
!
!                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]'
!
!                    The normal vector computation uses topographic
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
!              on the target body. FIXREF may be any such frame
!              supported by the SPICE system, including built-in
!              frames (documented in the Frames Required Reading)
!              and frames defined by a loaded frame kernel (FK). The
!              string FIXREF is case-insensitive, and leading and
!              trailing blanks in FIXREF are not significant.
!
!              The input surface points in the array SRFPTS are
!              expressed relative to this reference frame, as are
!              the normal vectors computed by this routine.
!
!
!     NPTS     is the number of surface points in the array SRFPTS.
!
!
!     SRFPTS   is an array of target body surface points. Elements
!
!                 SRFPTS(1,I)
!                 SRFPTS(2,I)
!                 SRFPTS(3,I)
!
!              are the Cartesian coordinates, expressed in the
!              reference frame designated by FIXREF, of the Ith
!              surface point in the array. Each surface point
!              represents an offset from the center of that frame.
!
!              All surface points must actually be "on" the surface,
!              that is, the distance of each point from the surface
!              must be less than a small margin. See the $Parameters
!              section below for a description of this margin.
!
! $ Detailed_Output
!
!     NORMLS   is an array of unit length, outward normal vectors
!              corresponding to the points in SRFPTS. Elements
!
!                 NORMLS(1,I)
!                 NORMLS(2,I)
!                 NORMLS(3,I)
!
!              are the Cartesian coordinates, expressed in the
!              reference frame designated by FIXREF, of the Ith
!              normal vector in the array.
!
! $ Parameters
!
!     PTMEMM   is the default point-surface membership margin. This
!              margin limits the distance an input point can be from
!              a surface and still be considered to lie on that
!              surface.
!
!              The details of the application of PTMEMM are
!              implementation-dependent. In the DSK case, roughly
!              speaking, a point-surface distance limit within a DSK
!              segment is set to
!
!                 PTMEMM * MAXR
!
!              where MAXR is the radius of an outer bounding sphere
!              for the segment.
!
!              For shapes modeled as ellipsoids, the expression
!              above is applied to the maximum radius of the
!              ellipsoid.
!
!              See the include file
!
!                 dsktol.inc
!
!              for the declaration of PTMEMM.
!
!              This margin can be overridden. See dsktol.inc
!              and DSKSTL for details.
!
! $ Exceptions
!
!     1)  If the target body name specified in the input string cannot
!         be converted to an integer ID code, the error
!         SPICE(IDCODENOTFOUND) is signaled.
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
!         loaded into the kernel pool prior to calling SRFNRM, an error
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
!     9)  If METHOD specifies that the target surface is represented by
!         DSK data, and data representing the portion of the surface
!         corresponding to the surface points provided in SRFPTS are
!         not available, an error is signaled by a routine in the
!         call tree of this routine.
!
!     10) If an input surface point is not within a small tolerance
!         of the specified surface, the error SPICE(POINTNOTONSURFACE)
!         is signaled. See the $Parameters section for details.
!
!     11) If the radii are not available in the kernel pool, an error is
!         signaled by a routine in the call tree of this routine.
!
!     12) If the target shape is "ellipsoid" and not all radii of the
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
!          PCK data:
!
!             If the target shape is modeled as an ellipsoid,
!             triaxial radii for the target body must be loaded into
!             the kernel pool. Typically this is done by loading a
!             text PCK file via FURNSH.
!
!          DSK data:
!
!             If the target shape is modeled by DSK data, DSK files
!             containing topographic data for the target body must be
!             loaded. If a surface list is specified, data for at
!             least one of the listed surfaces must be loaded.
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
!     1) Compute outward normal vectors at surface points on a target
!        body, where the points correspond to a given planetocentric
!        longitude/latitude grid. Use both ellipsoid and DSK shape
!        models.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: srfnrm_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                        Contents
!              ---------                        --------
!              pck00010.tpc                     Planet orientation and
!                                               radii
!              phobos512.bds                    DSK based on
!                                               Gaskell ICQ Q=512
!                                               plate model
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'pck00010.tpc',
!                                  'phobos512.bds' )
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM SRFNRM_EX1
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
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'srfnrm_ex1.tm' )
!
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
!              PARAMETER           ( MAXN   = 100000 )
!
!              INTEGER               MTHLEN
!              PARAMETER           ( MTHLEN = 80 )
!        C
!        C     Local variables
!        C
!              CHARACTER*(FRNMLN)    FIXREF
!              CHARACTER*(MTHLEN)    METHOD ( 2 )
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
!              DOUBLE PRECISION      NORMLS ( 3, MAXN, 2 )
!              DOUBLE PRECISION      NRMLAT
!              DOUBLE PRECISION      NRMLON
!              DOUBLE PRECISION      NRMRAD
!              DOUBLE PRECISION      SRFPTS ( 3, MAXN, 2 )
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
!        C     Saved variables
!        C
!              SAVE                  GRID
!              SAVE                  NORMLS
!              SAVE                  SRFPTS
!
!        C
!        C     Load kernels.
!        C
!              CALL FURNSH ( META )
!        C
!        C     Set target, reference frame, and epoch.
!        C
!              TARGET = 'PHOBOS'
!              FIXREF = 'IAU_PHOBOS'
!              ET     = 0.D0
!        C
!        C     Use both a reference ellipsoid and DSK data
!        C     to represent the surface.
!        C
!              METHOD(1) = 'ELLIPSOID'
!              METHOD(2) = 'DSK/UNPRIORITIZED'
!        C
!        C     Set the grid dimensions.
!        C
!              NLON   = 6
!              NLAT   = 3
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
!
!        C
!        C     Now generate the grid points.  We generate
!        C     points along latitude bands, working from
!        C     north to south.  The latitude range is selected
!        C     to range from +45 to -45 degrees.  Longitude
!        C     ranges from 0 to 300 degrees.  The increment
!        C     is 45 degrees for latitude and 60 degrees for
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
!
!        C
!        C     Find the surface points corresponding to the grid points.
!        C
!        C
!        C     Compute outward normal vectors at the surface points,
!        C     using both surface representations.
!        C
!              DO I = 1, 2
!
!                 CALL LATSRF ( METHOD(I),    TARGET, ET,
!             .                 FIXREF,       N,      GRID,
!             .                 SRFPTS(1,1,I)              )
!
!                 CALL SRFNRM ( METHOD(I),    TARGET, ET,
!             .                 FIXREF,       N,      SRFPTS(1,1,I),
!             .                 NORMLS(1,1,I)                      )
!              END DO
!
!
!              WRITE (*,*) 'Number of grid points: ', N
!
!        C
!        C     Print out the surface points in latitudinal
!        C     coordinates and compare the derived lon/lat values
!        C     to those of the input grid for the first 3 points.
!        C
!              DO I = 1, 3
!        C
!        C        Use RECRAD rather than RECLAT to produce
!        C        non-negative longitudes.
!        C
!                 CALL RECRAD ( SRFPTS(1,I,1), XR, XLON, XLAT )
!
!                 WRITE (*,*) ' '
!
!                 OUTLIN = ' Surface point for grid point #:'
!                 CALL REPMI  ( OUTLIN, '#', I, OUTLIN )
!                 CALL TOSTDO ( OUTLIN )
!
!                 WRITE (*,*)    '  Latitudinal Coordinates:'
!                 WRITE (*,FMT1) '    Longitude           (deg): ',
!             .                  XLON*DPR()
!                 WRITE (*,FMT1) '    Latitude            (deg): ',
!             .                  XLAT*DPR()
!                 WRITE (*,FMT1) '    Ellipsoid Radius     (km): ',
!             .                  XR
!
!                 CALL RECRAD ( SRFPTS(1,I,2), XR, XLON, XLAT )
!
!                 WRITE (*,FMT1) '    DSK Radius           (km): ',
!             .                  XR
!        C
!        C        Convert the Ith normal vector to latitudinal
!        C        coordinates.
!        C
!                 CALL RECRAD ( NORMLS(1,I,1), NRMRAD, NRMLON, NRMLAT )
!
!                 WRITE (*,*)    '  Ellipsoid normal vector direction:'
!                 WRITE (*,FMT1) '    Longitude           (deg): ',
!             .                       NRMLON*DPR()
!                 WRITE (*,FMT1) '    Latitude            (deg): ',
!             .                      NRMLAT*DPR()
!
!                 CALL RECRAD ( NORMLS(1,I,2), NRMRAD, NRMLON, NRMLAT )
!
!                 WRITE (*,*)    '  DSK normal vector direction:'
!                 WRITE (*,FMT1) '    Longitude           (deg): ',
!             .                  NRMLON*DPR()
!                 WRITE (*,FMT1) '    Latitude            (deg): ',
!             .                  NRMLAT*DPR()
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Number of grid points:           18
!
!         Surface point for grid point 1:
!           Latitudinal Coordinates:
!             Longitude           (deg):    0.000000
!             Latitude            (deg):   45.000000
!             Ellipsoid Radius     (km):   10.542977
!             DSK Radius           (km):   10.156402
!           Ellipsoid normal vector direction:
!             Longitude           (deg):    0.000000
!             Latitude            (deg):   63.895146
!           DSK normal vector direction:
!             Longitude           (deg):  341.337568
!             Latitude            (deg):   62.610726
!
!         Surface point for grid point 2:
!           Latitudinal Coordinates:
!             Longitude           (deg):   60.000000
!             Latitude            (deg):   45.000000
!             Ellipsoid Radius     (km):   10.172847
!             DSK Radius           (km):   10.131412
!           Ellipsoid normal vector direction:
!             Longitude           (deg):   66.059787
!             Latitude            (deg):   58.877649
!           DSK normal vector direction:
!             Longitude           (deg):   48.859884
!             Latitude            (deg):   56.924717
!
!         Surface point for grid point 3:
!           Latitudinal Coordinates:
!             Longitude           (deg):  120.000000
!             Latitude            (deg):   45.000000
!             Ellipsoid Radius     (km):   10.172847
!             DSK Radius           (km):   10.423766
!           Ellipsoid normal vector direction:
!             Longitude           (deg):  113.940213
!             Latitude            (deg):   58.877649
!           DSK normal vector direction:
!             Longitude           (deg):  118.553200
!             Latitude            (deg):   55.906774
!
!
!        Note that only the first 3 points of the grid are
!        presented in the output (the rest of the points are not
!        shown due to their large number).
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
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Limited the number of grid points presented in the output of
!        the code example to three.
!
!-    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB)
!
!        Added FAILED call.
!
!        01-JUL-2016 (NJB)
!
!-&


! $ Index_Entries
!
!     map Cartesian surface points to normal vectors
!     compute normal vectors on topographic surface
!     compute normal vectors on DSK surface
!
!-&


!
!     SPICELIB functions
!
      logical               eqstr
      logical               failed
      logical               return

!
!     Local parameters
!
      character*(*)         rname
      parameter           ( rname  = 'SRFNRM' )

      integer(ip)           bdnmln
      parameter           ( bdnmln = 36 )

      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

!
!     Local variables
!
      character*(cvtlen)    lmbtyp
      character*(mthlen)    prvmth
      character*(shplen)    shpstr
      character*(sublen)    subtyp
      character*(tmtlen)    trmtyp

      real(wp)              a2
      real(wp)              b2
      real(wp)              c2
      real(wp)              level
      real(wp)              limit
      real(wp)              maxr
      real(wp)              ptsrfm

      integer(ip)           fixfid
      integer(ip)           fxcent
      integer(ip)           fxclss
      integer(ip)           fxclid
      integer(ip)           i
      integer(ip)           n
      integer(ip)           nsurf
      integer(ip)           shape
      integer(ip)           srfctr ( ctrsiz )
      integer(ip)           srflst ( maxsrf )
      integer(ip)           trgcde

      logical               first
      logical               fnd
      logical               pri
      logical               surfup

!
!     Saved name/ID item declarations.
!
      integer(ip)           svctr1 ( ctrsiz )
      character*(bdnmln)    svtarg
      integer(ip)           svtcde
      logical               svfnd1

!
!     Saved frame name/ID item declarations.
!
      integer(ip)           svctr2 ( ctrsiz )
      character*(frnmln)    svfref
      integer(ip)           svfxfc

!
!     Saved target radius values.
!
      integer(ip)           svctr3 ( ctrsiz )
      integer(ip)           svprvt
      real(wp)              svradi ( 3 )

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
      save                  maxr
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

      if ( failed() ) then
         call chkout ( rname )
         return
      end if


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
            //            'SUBSLR, but is not applicable for SRFNRM.' )
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

         if (  min( svradi(1), svradi(2), svradi(3) ) .le. 0.0_wp ) then

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
!        Compute the point-surface distance limit.
!
         maxr   = max ( svradi(1), svradi(2), svradi(3) )

         call dskgtl ( keyptm, ptsrfm )

         limit  = ptsrfm * maxr

!
!        Generate normal vectors.
!
         do i = 1, npts

            a2    = svradi(1)*svradi(1)
            b2    = svradi(2)*svradi(2)
            c2    = svradi(3)*svradi(3)

            level = (   ( srfpts(1,i)*srfpts(1,i) / a2 ) &
                      + ( srfpts(2,i)*srfpts(2,i) / b2 ) &
                      + ( srfpts(3,i)*srfpts(3,i) / c2 )  ) ** 0.5_wp

!
!           The test below is a distance test if the target shape
!           is a sphere. For other ellipsoids, it's an approximation.
!
            if ( abs( level - 1.0_wp ) .ge. limit ) then

               call setmsg ( 'Input point at index # is not on ' &
               //            'the target body surface. The ' &
               //            'level surface parameter (x/a)**2 ' &
               //            '+ (y/b)**2 + (z/c)**2 for this ' &
               //            'point is #.'                      )
               call errint ( '#', i                             )
               call errdp  ( '#', level                         )
               call sigerr ( 'SPICE(POINTNOTONSURFACE)'         )
               call chkout ( rname                              )
               return

            end if

            call surfnm ( svradi(1),   svradi(2),  svradi(3), &
                          srfpts(1,i), normls(1,i)            )

            if ( failed() ) then
               call chkout ( rname )
               return
            end if


         end do


      else if  ( shape .eq. dskshp ) then
!
!        Generate normal vectors.
!
         do i = 1, npts
!
!           Use the DSK API segment buffering system to efficiently
!           select relevant segments and compute normals.
!
            call zzsbfnrm ( trgcde, nsurf,       srflst,     et, &
                            fixfid, srfpts(1,i), normls(1,i)     )

            if ( failed() ) then
               call chkout ( rname )
               return
            end if

!
!           Make sure normals have unit length.
!
            call vhatip ( normls(1,i) )

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
