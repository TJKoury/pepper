!$Procedure DSKX02 ( DSK, ray-surface intercept, type 2 )

      SUBROUTINE DSKX02 ( HANDLE, DLADSC, VERTEX, &
                          RAYDIR, PLID,   XPT,    FOUND )

! $ Abstract
!
!     Determine the plate ID and body-fixed coordinates of the
!     intersection of a specified ray with the surface defined by a
!     type 2 DSK plate model.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     GEOMETRY
!     SHAPES
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'dla.inc'
      INCLUDE              'dskdsc.inc'
      INCLUDE              'dsk02.inc'
      INCLUDE              'dsktol.inc'


      INTEGER               HANDLE
      INTEGER               DLADSC   ( * )
      DOUBLE PRECISION      VERTEX   ( 3 )
      DOUBLE PRECISION      RAYDIR   ( 3 )
      INTEGER               PLID
      DOUBLE PRECISION      XPT      ( 3 )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DSK kernel containing plate model.
!     DLADSC     I   DLA descriptor of plate model segment.
!     VERTEX     I   Ray's vertex in the  body fixed frame.
!     RAYDIR     I   Ray direction in the body fixed frame.
!     PLID       O   ID code of the plate intersected by the ray.
!     XPT        O   Intercept.
!     FOUND      O   Flag indicating whether intercept exists.
!     XFRACT     P   Plate expansion fraction.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a DSK file containing a shape
!              model for a target body. The shape model is stored
!              in a type 2 DSK segment.
!
!     DLADSC   is the DLA descriptor of a type 2 DSK segment
!              containing plate model data representing the surface of
!              the target body. The caller should declare DLADSC
!              with size DLADSZ; this size parameter is defined in
!              the INCLUDE file dla.inc. Normally this descriptor
!              will be obtained by a search through a DSK file
!              using the DLA search routines; see the $Examples
!              header section below for a working code example
!              illustrating a simple search.
!
!     VERTEX   is the vertex of a ray. VERTEX is expressed relative
!              to the body fixed reference frame associated with the
!              target body. This reference frame is the same frame
!              relative to which the vertices of the plate model
!              are expressed. Units are km.
!
!              The vertex is required to be outside the target
!              body.
!
!     RAYDIR   is the ray's direction vector. RAYDIR is expressed
!              relative to the body fixed reference frame associated
!              with the target body.
!
! $ Detailed_Output
!
!     PLID     is the ID of the plate closest to the input ray's
!              vertex at which a ray-surface intercept exists.
!              If no intercept exists, PLID is undefined.
!
!     XPT      is the ray-target intercept closest to the ray's
!              vertex, if an intercept exists. XPT is expressed
!              relative to the body-fixed reference frame associated
!              with the target body. Units are km.
!
!              If no intercept exists, XPT is undefined.
!
!     FOUND    is a logical flag that indicates whether or not the
!              ray does indeed intersect the target. If the ray
!              intersects a plate FOUND is .TRUE. Otherwise FOUND is
!              .FALSE.
!
! $ Parameters
!
!     XFRACT   is the default plate expansion fraction. This
!              parameter can be overridden.
!
!     See the include file
!
!        dsktol.inc
!
!     for the values of tolerance parameters used by default by the
!     ray-surface intercept algorithm.
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
!     4)  If an error occurs while trying to look up any component
!         of the shape model, the error is signaled by a routine in the
!         call tree of this routine.
!
!     5)  If the input ray direction is the zero vector, the error
!         SPICE(ZEROVECTOR) is signaled.
!
!     6)  If the coarse voxel grid scale of the shape model is less
!         than 1, the error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     7)  If the coarse voxel grid of the shape model contains more
!         than MAXCGR (see dsk02.inc) voxels, the error
!         SPICE(GRIDTOOLARGE) is signaled.
!
!     8)  If the plate list for any intersected voxel is too large
!         for this routine to buffer, the error SPICE(ARRAYTOOSMALL)
!         is signaled.
!
!     9)  Due to round-off errors, results from this routine may
!         differ across platforms. Results also may differ from
!         those expected---and not necessarily by a small amount.
!         For example, a ray may miss a plate it was expected to
!         hit and instead hit another plate considerably farther
!         from the ray's vertex, or miss the target entirely.
!
!     10) In the event that an intercept point lies on multiple
!         plates (that is, the point is on an edge or vertex),
!         a plate will be selected. Due to round-off error, the
!         selection may vary across platforms.
!
! $ Files
!
!     See the description of the input argument HANDLE.
!
! $ Particulars
!
!     This routine solves the ray-surface intercept problem for
!     a specified ray and a surface represented by triangular plate
!     model. The surface representation is provided by data in a
!     type 2 segment of a DSK file.
!
!     This routine does not assume that the segment from which the
!     surface model data are read represents the entire surface of
!     the target body. A program could call this routine repeatedly
!     to find the surface intercept of a ray and a shape model
!     partitioned into multiple segments.
!
!     In general, this routine should be expected to run faster
!     when used with smaller shape models.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input,
!     the compiler and supporting libraries, and the machine specific
!     arithmetic implementation.
!
!     1) Find the surface intercept points corresponding to a latitude/
!        longitude grid of a specified resolution, for a specified
!        target body.
!
!        This simple program assumes the shape model for the target
!        body is stored in a single type 2 DSK segment, and that this
!        segment is the first one in the DSK file to which it belongs.
!
!
!        Example code begins here.
!
!
!              PROGRAM DSKX02_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!              INCLUDE 'dskdsc.inc'
!              INCLUDE 'dsk02.inc'
!        C
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DSKSGR
!              DOUBLE PRECISION      RPD
!        C
!        C
!        C     Local parameters
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               NLAT
!              PARAMETER           ( NLAT   = 9 )
!
!              INTEGER               NLON
!              PARAMETER           ( NLON   = 9 )
!
!        C
!        C     Local parameters
!        C
!              DOUBLE PRECISION      TOL
!              PARAMETER           ( TOL   =  1.D-12 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    DSK
!
!              DOUBLE PRECISION      DSKDSC ( DSKDSZ )
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      MAXR
!              DOUBLE PRECISION      R
!              DOUBLE PRECISION      RAYDIR ( 3 )
!              DOUBLE PRECISION      VERTEX ( 3 )
!              DOUBLE PRECISION      XLAT
!              DOUBLE PRECISION      XLON
!              DOUBLE PRECISION      XPT    ( 3 )
!              DOUBLE PRECISION      XR
!
!              INTEGER               DLADSC ( DLADSZ )
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!              INTEGER               PLID
!
!              LOGICAL               FOUND
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
!        C     We're going to generate the intercept points
!        C     using a set of rays which point toward the
!        C     origin and whose vertices are on a
!        C     specified lat/lon grid.  To start out we
!        C     must pick a reasonable range from the origin
!        C     for the vertices:  the range must be large
!        C     enough so that the vertices are guaranteed
!        C     to be outside the target body but small
!        C     enough that we don't lose too much precision
!        C     in the surface intercept computation.
!        C
!        C     We'll look up the upper bound for the target
!        C     radius, then use 2 times this value as the
!        C     vertex magnitude.
!        C
!              CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
!
!              MAXR = DSKDSC(MX3IDX)
!              R    = 2.D0 * MAXR
!
!        C
!        C     Now generate the intercept points.  We generate
!        C     intercepts along latitude bounds, working from
!        C     north to south. Latitude ranges from +80 to
!        C     -80 degrees. Longitude ranges from 0 to 320
!        C     degrees. The increment is 20 degrees for
!        C     latitude and 40 degrees for longitude.
!        C
!              DO I = 1, NLAT
!
!                 LAT = RPD() * ( 100.D0 - 20.D0*I )
!
!                 DO J = 1, NLON
!
!                    LON = RPD() * 40.D0 * (J-1)
!        C
!        C           Produce a ray vertex for the current
!        C           lat/lon value.  Negate the vertex to
!        C           produce the ray's direction vector.
!        C
!                    CALL LATREC ( R, LON, LAT, VERTEX )
!                    CALL VMINUS ( VERTEX,      RAYDIR )
!        C
!        C           Find the surface intercept for this
!        C           ray.
!        C
!                    CALL DSKX02 ( HANDLE, DLADSC, VERTEX,
!             .                    RAYDIR, PLID,   XPT,    FOUND  )
!        C
!        C           Since the ray passes through the origin on
!        C           the body-fixed frame associated with the
!        C           target body, we'd rarely expect to find that
!        C           the ray failed to intersect the surface.
!        C           For safety, we check the FOUND flag.  (A
!        C           "not found" condition could be a sign of
!        C           a bug.)
!        C
!                    IF ( .NOT. FOUND ) THEN
!
!                       WRITE(*,*) ' '
!                       WRITE(*,*) 'Intercept not found!'
!                       WRITE(*,*) '   Ray vertex:'
!                       WRITE(*,*) '   Longitude (deg): ', LON/RPD()
!                       WRITE(*,*) '   Latitude  (deg): ', LAT/RPD()
!                       WRITE(*,*) '   Range      (km): ', R
!                       WRITE(*,*) ' '
!
!                    ELSE
!        C
!        C              This is the normal case.  Display the
!        C              intercept plate ID and the intercept
!        C              point in both Cartesian and latitudinal
!        C              coordinates.  Show the corresponding ray
!        C              vertex to facilitate validation of results.
!        C
!        C              Use RECRAD rather than RECLAT to produce
!        C              non-negative longitudes.
!        C
!                       CALL RECRAD ( XPT, XR, XLON, XLAT )
!
!                       WRITE(*,*) ' '
!                       WRITE(*,*) 'Intercept found:'
!                       WRITE(*,*) '   Plate ID:             ', PLID
!                       WRITE(*, '(1X,A,3F12.8)' )
!             .         '   Cartesian coordinates:', XPT
!                       WRITE(*,*) '   Latitudinal coordinates:'
!                       WRITE(*,*) '   Longitude (deg): ', XLON/RPD()
!                       WRITE(*,*) '   Latitude  (deg): ', XLAT/RPD()
!                       WRITE(*,*) '   Range      (km): ', XR
!                       WRITE(*,*)
!                       WRITE(*,*) '   Ray vertex:'
!                       WRITE(*,*) '   Longitude (deg): ', LON/RPD()
!                       WRITE(*,*) '   Latitude  (deg): ', LAT/RPD()
!                       WRITE(*,*) '   Range      (km): ', R
!                       WRITE(*,*) ' '
!
!                    END IF
!
!                 END DO
!
!              END DO
!        C
!        C     Close the kernel.  This isn't necessary in a stand-
!        C     alone program, but it's good practice in subroutines
!        C     because it frees program and system resources.
!        C
!              CALL DASCLS ( HANDLE )
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos_3_3.bds, the output
!        was:
!
!
!        Enter DSK name > phobos_3_3.bds
!
!         Intercept found:
!            Plate ID:                   306238
!            Cartesian coordinates:  1.52087789  0.00000000  8.62532711
!            Latitudinal coordinates:
!            Longitude (deg):    0.0000000000000000
!            Latitude  (deg):    80.000000000000014
!            Range      (km):    8.7583866856211490
!
!            Ray vertex:
!            Longitude (deg):    0.0000000000000000
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    28.023536291251524
!
!
!         Intercept found:
!            Plate ID:                   317112
!            Cartesian coordinates:  1.18970365  0.99827989  8.80777185
!            Latitudinal coordinates:
!            Longitude (deg):    40.000000000000000
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    8.9436459265318629
!
!            Ray vertex:
!            Longitude (deg):    40.000000000000000
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    28.023536291251524
!
!
!         Intercept found:
!            Plate ID:                   324141
!            Cartesian coordinates:  0.27777518  1.57534131  9.07202903
!            Latitudinal coordinates:
!            Longitude (deg):    80.000000000000028
!            Latitude  (deg):    80.000000000000014
!            Range      (km):    9.2119797003191017
!
!            Ray vertex:
!            Longitude (deg):    80.000000000000000
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    28.023536291251524
!
!
!         Intercept found:
!            Plate ID:                   327994
!            Cartesian coordinates: -0.81082405  1.40438846  9.19682344
!            Latitudinal coordinates:
!            Longitude (deg):    120.00000000000001
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    9.3386992651610452
!
!            Ray vertex:
!            Longitude (deg):    119.99999999999999
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    28.023536291251524
!
!
!         Intercept found:
!            Plate ID:                   329431
!            Cartesian coordinates: -1.47820193  0.53802150  8.92132122
!            Latitudinal coordinates:
!            Longitude (deg):    160.00000000000006
!            Latitude  (deg):    80.000000000000014
!            Range      (km):    9.0589469760393797
!
!            Ray vertex:
!            Longitude (deg):    160.00000000000000
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    28.023536291251524
!
!
!         Intercept found:
!            Plate ID:                   196042
!            Cartesian coordinates: -1.49854761 -0.54542673  9.04411256
!            Latitudinal coordinates:
!            Longitude (deg):    200.00000000000000
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    9.1836325764960041
!
!            Ray vertex:
!            Longitude (deg):    200.00000000000000
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    28.023536291251524
!
!
!         Intercept found:
!            Plate ID:                   235899
!            Cartesian coordinates: -0.78240454 -1.35516441  8.87447325
!            Latitudinal coordinates:
!            Longitude (deg):    239.99999999999991
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    9.0113763066160804
!
!            Ray vertex:
!            Longitude (deg):    239.99999999999997
!            Latitude  (deg):    80.000000000000000
!            Range      (km):    28.023536291251524
!
!
!
!        [...]
!
!
!        Warning: incomplete output. Only 100 out of 1135 lines have
!        been provided.
!
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  A. Woo, "Fast Ray-Box Intersection", Graphic Gems I,
!          395-396, Aug. 1990
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J.A. Bytof         (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 13-JAN-2021 (NJB) (JDR) (BVS)
!
!        Bug fix: in some cases the previous version of this routine
!        could still return an intercept outside of the segment
!        boundaries by more than the allowed margin. In those cases,
!        the returned plate ID was invalid. Both problems have been
!        corrected.
!
!        See $Revisions for details.
!
!        Edited the header to comply with NAIF standard. Updated the
!        example code to retrieve directly from the DSK descriptor the
!        upper bound for the target radius. Added record in
!        $Index_Entries.
!
!-    SPICELIB Version 1.0.0, 04-APR-2017 (NJB) (EDW) (JAB)
!
!        Added test for containment of intersection point
!        within segment boundaries. Updated logic for saving
!        segment attributes so that errors won't cause saved
!        values to be improperly re-used on subsequent calls.
!
!        24-FEB-2016 (NJB)
!
!           Replaced call to TOGRID with call to ZZTOGRID.
!           Replaced call to PLTREC with call to PLTNRM.
!           Now obtains plate expansion fraction from DSKGTL.
!
!        25-FEB-2015 (NJB)
!
!           Bug fix: now ray-voxel grid intercept is displaced toward
!           the input ray's vertex only when the vertex is outside
!           the target body's voxel grid.
!
!        10-SEP-2014 (NJB)
!
!           Bug fix: during an intercept search over the voxel list
!           returned by XDDA, if an intercept outside the current
!           voxel---by more than a small tolerance---is found, the
!           search rejects that intercept and continues until a
!           valid intercept is found and all plates in the voxel
!           containing that intercept have been checked for an
!           intersection. The rejected intercept may later be
!           determined to be a valid solution during a check of
!           plates associated with a voxel that contains that
!           intercept; in fact it is the correct solution if no
!           other plates contain a solution closer to the ray's
!           vertex. (Usually there is a unique voxel containing the
!           intercept, but this is not so if the intercept lies on
!           a voxel boundary not on an edge of the voxel grid.)
!
!           Note that there's no need to look for intersections in
!           voxels further out in the voxel list than the first one
!           that contains an intercept.
!
!           The previous version of the routine terminated the
!           search after checking all plates in the current voxel,
!           after an intercept was found on any plate associated
!           with that voxel. The intercept was not required to be
!           contained in the voxel.
!
!           See the $Revisions section for details.
!
!        30-JUN-2014 (NJB)
!
!           Bug fix: renamed "found" flag returned by ZZRAYBOX.
!
!           Added code to test for empty voxel list after
!           voxel list compression.
!
!        15-JUN-2014 (NJB)
!
!           Made some minor edits to in-line comments, and removed
!           comments that had become inapplicable due to code changes.
!
!        06-JUN-2014 (NJB)
!
!           Now expands plates slightly before performing ray-plate
!           intersection computations.
!
!           Bug fix: now calls ZZRAYBOX to find the ray-box intercept.
!           This reduces round-off error in the variable COORD.
!
!        02-MAY-2014 (NJB)
!
!           Bug fix: added FAILED checks after each DSKI02 and DSKD02
!           call.
!
!           Some precautionary measures were added: a backstop
!           check for an empty voxel list was added after the XDDA
!           call. A backstop initialization of PNTR was added
!           before the plate collection loop. This initialization
!           is needed only if the voxel list returned by XDDA is
!           empty. The list should never be empty.
!
!        25-MAR-2014 (NJB)
!
!           Bug fix: duplicate plates are now marked so that the
!           unmarked instance is the one in the closest voxel to
!           the ray's origin.
!
!           Bug fix: corrected buffer overflow error detection for
!           insertion of plate IDs into plate ID array.
!
!        20-JUL-2011 (NJB)
!
!           Bug fix: this routine now tests FAILED after its
!           call to XDDA.
!
!           Header correction: the detailed input section
!           now says that the ray's vertex *is* required to
!           be outside the target body.
!
!        09-JUN-2011 (NJB)
!
!           All large local arrays are now saved in order to support
!           calling a C translation of this routine from Java.
!
!           The buffer VIDXS is now initialized prior to its
!           first use in an argument list. This was done to
!           to suppress compiler warnings. The original code was
!           correct, since along with the buffer, an array size
!           of zero was passed to the called function.
!
!           The example program was updated for compatibility with
!           the final DSK descriptor layout. The output format was
!           adjusted. Sample output from the program is now shown
!           in the header.
!
!        13-MAY-2010 (NJB)
!
!           No longer uses plate records to weed out
!           plates prior to ray-plate intercept tests.
!           Now uses local vertex buffer. Logic for choosing
!           plate when intercept is on edge or vertex has
!           been simplified.
!
!        06-MAY-2010 (NJB)
!
!           Now calls DSKB02 rather than DSKP02.
!
!        20-APR-2010 (NJB)
!
!           Updated header section order.
!
!        26-SEP-2008 (NJB)
!
!           Moved OBSMAT computation out of loop.
!
!        27-DEC-2006 (NJB) (EDW)
!
!           Header example was updated to show maximum radius
!           being obtained from DSK descriptor rather than via
!           all to DSKD02.
!
!        31-OCT-2006 (NJB) (EDW)
!
!           Modified to work with DLA-based kernels. Many
!           changes were made to the algorithm to improve
!           execution speed.
!
!        19-AUG-2004 (EDW)
!
!           Implemented "Fast Ray-Box Intersection" algorithm.
!           Renamed routine DSKX02 from PLBORE_3.
!
!        25-FEB-1999 (JAB)
!
!           Based on PLBORE and PLBORE_2.
!
!-&


! $ Index_Entries
!
!     plate and plate model point intersected by ray
!     intersection of ray and surface
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 13-JAN-2021 (NJB)
!
!        Bug fix: in some cases the previous version of this
!        routine could return an intercept outside of the segment
!        boundaries (the "outside intercept") by more than the
!        allowed margin. In those cases, the returned plate ID was
!        invalid.
!
!        Both problems have been corrected.
!
!        Description of the bug
!        ----------------------
!
!        In the case where all of the follow conditions hold:
!
!           - the input ray's intercepts exist both within and
!             outside the segment's boundaries
!
!           - the outside intercept is considered the nearest
!             solution to the vertex at the time the intercept
!             is found
!
!           - the intercept that should have been selected was
!             found before the outside intercept
!
!           - both intercepts lie on plates belonging to the same voxel
!
!        the outside intercept will overwrite the correct intercept.
!
!        In the situation described above, the plate ID returned
!        will not be that of the outside plate.
!
!        Solution
!        --------
!
!        Each intercept that passes the test for being closest, of
!        all intercepts seen so far, to the ray's vertex is stored
!        in a temporary variable. The output XPT is updated only
!        when the intercept is found to lie within the segment's
!        coordinate bounds, or outside the bounds by no more than
!        the allowed margin.
!
!-    SPICELIB Version 1.0.0, 04-APR-2017 (NJB)
!
!        10-SEP-2014 (NJB)
!
!           Bug fix: during an intercept search over the voxel
!           list returned by XDDA, if an intercept outside the
!           current voxel---by more than a small tolerance---is
!           found, the search rejects that intercept and continues
!           until a valid intercept is found and all plates in the
!           voxel containing that intercept have been checked for
!           an intersection. The rejected intercept may later be
!           determined to be a valid solution during a check of
!           plates associated with a voxel that contains that
!           intercept; in fact it is the correct solution if no
!           other plates contain a solution closer to the ray's
!           vertex. (Usually there is a unique voxel containing
!           the intercept, but this is not so if the intercept
!           lies on a voxel boundary not on an edge of the voxel
!           grid.)
!
!           Note that there's no need to look for intersections in
!           voxels further out in the voxel list than the first
!           one that contains an intercept.
!
!           The previous version of the routine terminated the
!           search after checking all plates in the current voxel,
!           after an intercept was found on any plate associated
!           with that voxel. The intercept was not required to be
!           contained in the voxel.
!
!           In the previous version of the routine, an intercept
!           found outside of the current voxel could effectively
!           mask an intercept closer to the ray's vertex, as shown
!           in the diagram below.
!
!           In this diagram, "V" represents the vertex of the ray.
!           The letter sequences "Q*" and "P*" represent plates.
!           Here the ray hits voxel 1 and finds an intercept on
!           plate P* at the point marked by "@." No other
!           intercepts on plates in voxel 1 exist, so the search
!           terminates. The intercept marked by "$" is closer to
!           the vertex but is not seen.
!
!                                   V
!                                  /
!                                 /
!                                /
!              +--------------+-/------------+
!              |    voxel 2   |/    voxel 1  |
!              |              /              |
!              |        QQQQQ$|              |
!              |            / |              |
!              |           /  |              |
!              |          /   |              |
!              |       PP@PPPPPPPPPPPPPPP    |
!              +--------------+--------------+
!
!
!           The updated algorithm, when presented with the
!           situation shown above, will check all plates in voxel
!           2 before terminating.
!
!           Note that the problem could occur in cases where
!           voxels 1 and 2 are not adjacent.
!
!-&


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDOT

      LOGICAL               RETURN
      LOGICAL               FAILED
      LOGICAL               VZERO

!
!     Statement function type declarations
!
      INTEGER               VOX2ID


!
!     Local parameters
!


!
!     Tolerance used for vertex-voxel grid distance test:
!
      DOUBLE PRECISION      VTXTOL
      PARAMETER           ( VTXTOL = 1.D-12 )

!
!     Maximum number of voxels we can accept for
!     one XDDA call.
!
      INTEGER               NVXLST
      PARAMETER           ( NVXLST = 50000 )

!
!     Maximum number of plates we work with
!     at any time.
!
      INTEGER               MAXPL
      PARAMETER           ( MAXPL  =  MAXPLT/125 )

!
!     Parameter indicating no coordinates are to be excluded
!     in the test for a point being within segment boundaries.
!
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

!
!     Local Variables
!
      INTEGER               ISRCHI

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 200 )


      DOUBLE PRECISION      COORD  ( 3 )
      DOUBLE PRECISION      EDGES  ( 3, 3 )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      GRDEXT ( 3 )
      DOUBLE PRECISION      GRDTOL
      DOUBLE PRECISION      GREEDM
      DOUBLE PRECISION      HITCOR ( 3 )
      DOUBLE PRECISION      NEAR
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OBSMAT ( 3, 3 )
      DOUBLE PRECISION      POINTS ( 3, 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      VBUFF  ( 3, BUFSIZ )
      DOUBLE PRECISION      VOXORI ( 3 )
      DOUBLE PRECISION      VOXSIZ
      DOUBLE PRECISION      VTX2   ( 3 )
      DOUBLE PRECISION      VTXBDS ( 2, 3 )
      DOUBLE PRECISION      VTXOFF ( 3 )
      DOUBLE PRECISION      XPDFRC
      DOUBLE PRECISION      XPNTS  ( 3, 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      XTOL

      INTEGER               CGREXT ( 3 )
      INTEGER               CGRPTR ( MAXCGR )
      INTEGER               CGSCAL
      INTEGER               CGSCL2
      INTEGER               CGXYZ  ( 3 )
      INTEGER               CORSYS
      INTEGER               CVID
      INTEGER               DIM
      INTEGER               DIM1
      INTEGER               DIM2
      INTEGER               FINAL
      INTEGER               FX
      INTEGER               FY
      INTEGER               FZ
      INTEGER               GROUP
      INTEGER               GRPBEG
      INTEGER               GRPEND
      INTEGER               GRPSIZ
      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               J
      INTEGER               K
      INTEGER               MINIDX
      INTEGER               NCGR
      INTEGER               NGROUP
      INTEGER               NP
      INTEGER               NPLATE
      INTEGER               NV
      INTEGER               NVBUF
      INTEGER               NVXOUT
      INTEGER               NVXTOT
      INTEGER               OFFSET
      INTEGER               ORDVEC ( MAXPL )
      INTEGER               PLATID ( MAXPL )
      INTEGER               PLROOM
      INTEGER               PNTR
      INTEGER               PRVDSC ( DLADSZ )
      INTEGER               PRVHAN
      INTEGER               SOURCE ( MAXPL )
      INTEGER               START
      INTEGER               TO
      INTEGER               TOTPLT
      INTEGER               VGREXT ( 3 )
      INTEGER               VI
      INTEGER               VIDS   ( 3 )
      INTEGER               VIDXS  (    BUFSIZ )
      INTEGER               VLOC
      INTEGER               VOXLST ( 3, NVXLST )
      INTEGER               VOXNPL
      INTEGER               VOXNPT
      INTEGER               VOXPTR
      INTEGER               VTXNPL
      INTEGER               VXC1
      INTEGER               VXC2
      INTEGER               VXC3
      INTEGER               VXLCG  ( 3, NVXLST )
      INTEGER               VXLOUT ( NVXLST )
      INTEGER               VXLSTR ( NVXLST )
      INTEGER               W

      LOGICAL               BOXHIT
      LOGICAL               EXTRA
      LOGICAL               HAVE
      LOGICAL               HITS
      LOGICAL               INSEG
      LOGICAL               INVOX
      LOGICAL               NEWSEG

!
!     Saved variables
!
      SAVE                  CGREXT
      SAVE                  CGRPTR
      SAVE                  CGSCAL
      SAVE                  CGSCL2
      SAVE                  CORSYS
      SAVE                  DSKDSC
      SAVE                  GRDEXT
      SAVE                  GRDTOL
      SAVE                  NCGR
      SAVE                  NP
      SAVE                  NV
      SAVE                  NVXTOT
      SAVE                  ORDVEC
      SAVE                  PLATID
      SAVE                  PRVDSC
      SAVE                  PRVHAN
      SAVE                  SOURCE
      SAVE                  VBUFF
      SAVE                  VIDXS
      SAVE                  VGREXT
      SAVE                  VOXLST
      SAVE                  VOXNPL
      SAVE                  VOXNPT
      SAVE                  VOXORI
      SAVE                  VOXSIZ
      SAVE                  VTXBDS
      SAVE                  VTXNPL
      SAVE                  VXLCG
      SAVE                  VXLOUT
      SAVE                  VXLSTR
      SAVE                  XTOL

!
!     Initial values
!
      DATA                  GRDEXT  / 3 * -1.D0 /
      DATA                  PRVHAN  / 0 /
      DATA                  PRVDSC  / DLADSZ * 0 /

!
!     Statement functions
!
      VOX2ID ( I1, I2, I3, DIM1, DIM2 ) =  I1 + DIM1*( I2 +  I3* DIM2 &
                                                          - (1 + DIM2) )

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DSKX02' )

!
!     Until we have better knowledge we assume there is no intersection.
!
      PLID   =  0
      FOUND  = .FALSE.
      HAVE   = .FALSE.
      NEAR   = DPMAX()

!
!     Initialize the vertex buffer.
!
      CALL CLEARI( BUFSIZ, VIDXS )

!
!     Check whether the ray direction vector is the zero vector.
!
      IF ( VZERO( RAYDIR ) ) THEN

         CALL SETMSG ( 'Ray direction is the zero vector.' )
         CALL SIGERR ( 'SPICE(RAYISZEROVECTOR)'            )
         CALL CHKOUT ( 'DSKX02'                            )
         RETURN

      END IF

!
!     Obtain the unit vector of the ray from the observer.
!
      CALL VHAT ( RAYDIR, UDIR )

!
!     Decide whether we're looking at a new segment.
!
      NEWSEG = .TRUE.

      IF ( HANDLE .EQ. PRVHAN ) THEN
!
!        The input handle matches the previous handle.  Note that the
!        initial value of PRVHAN is 0, which is never a valid handle,
!        so on the first pass, this test will fail.
!
         IF (       ( DLADSC(IBSIDX) .EQ. PRVDSC(IBSIDX) ) &
              .AND. ( DLADSC(DBSIDX) .EQ. PRVDSC(DBSIDX) ) &
              .AND. ( DLADSC(CBSIDX) .EQ. PRVDSC(CBSIDX) )  ) THEN
!
!           All of the DLA segment base addresses match.
!
            NEWSEG = .FALSE.

         END IF

      END IF


      IF ( NEWSEG ) THEN
!
!        Make sure we can't have a match with an earlier
!        segment on a subsequent call, if we exit this
!        block due to an error.
!
         PRVHAN = 0

!
!        Retrieve the voxel grid origin in model
!        units and calculate the farthest extent of the
!        voxel grid in voxel space.
!
         CALL DSKB02 ( HANDLE, DLADSC, NV,     NP,     NVXTOT, &
                       VTXBDS, VOXSIZ, VOXORI, VGREXT, &
                       CGSCAL, VTXNPL, VOXNPT, VOXNPL         )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DSKX02' )
            RETURN
         END IF

!
!        Compute the grid dimensions in units of km. First check
!        the voxel size.
!
         IF ( VOXSIZ .EQ. 0 ) THEN
            CALL SETMSG ( 'Voxel size is zero. This is an error ' &
            //            'in the DSK file attached to handle #.' )
            CALL ERRINT ( '#', HANDLE                             )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                   )
            CALL CHKOUT ( 'DSKX02'                                )
            RETURN

         END IF

         DO I = 1, 3
            GRDEXT(I) = VGREXT(I) * VOXSIZ
         END DO

!
!        Set the margin used for checking whether the ray's vertex
!        is close to the voxel grid.
!
         GRDTOL = VTXTOL * MAX( GRDEXT(1), GRDEXT(2), GRDEXT(3) )

!
!        Check the coarse grid voxel scale.
!
         IF ( CGSCAL .LT. 1 ) THEN

            CALL SETMSG ( 'Coarse grid scale = #; should be >= 1.' )
            CALL ERRINT ( '#', CGSCAL                              )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                 )
            CALL CHKOUT ( 'DSKX02'                                 )
            RETURN

         END IF

!
!        Get the coarse voxel grid dimensions and the coarse voxel
!        occupancy flags.
!
         DO I = 1, 3
            CGREXT(I) = VGREXT(I) / CGSCAL
         END DO

         CGSCL2 = CGSCAL * CGSCAL

         NCGR   = NVXTOT / CGSCAL**3

         IF ( NCGR .GT. MAXCGR ) THEN

            CALL SETMSG ( 'Coarse grid size NCGR = #. Buffer size = #' )
            CALL ERRINT ( '#', NCGR                                    )
            CALL ERRINT ( '#', MAXCGR                                  )
            CALL SIGERR ( 'SPICE(GRIDTOOLARGE)'                        )
            CALL CHKOUT ( 'DSKX02'                                     )
            RETURN

         END IF

         CALL DSKI02 ( HANDLE, DLADSC, KWCGPT, 1, MAXCGR, DIM, CGRPTR )

         CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DSKX02' )
            RETURN
         END IF

         CORSYS = NINT( DSKDSC(SYSIDX) )


         PRVHAN = HANDLE

         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC )

      END IF

!
!     Compute tolerance used for determining whether an intercept
!     is inside a voxel. The expansion fraction must be fetched
!     on every call to DSKX02.
!
      CALL DSKGTL ( KEYXFR, XPDFRC )

      XTOL   = XPDFRC * MAX( ABS( GRDEXT(1) ), &
                             ABS( GRDEXT(2) ), &
                             ABS( GRDEXT(3) )  )

!
!     Find the ray intercept on the surface of the fine voxel grid,
!     if the intercept exists.
!
      CALL ZZRAYBOX ( VERTEX, RAYDIR, VOXORI, GRDEXT, VTX2, BOXHIT )

      IF ( .NOT. BOXHIT ) THEN
         CALL CHKOUT ( 'DSKX02' )
         RETURN
      END IF

!
!     Convert the grid intercept to voxel space coordinates.
!     The result COORD will be used as the ray's vertex in XDDA.
!
      CALL ZZTOGRID ( VTX2, VOXORI, VOXSIZ, COORD )

!
!     Determine the voxels hit by the ray.
!
      CALL XDDA ( COORD, UDIR, VGREXT, NVXLST, NVXOUT, VOXLST )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DSKX02' )
         RETURN
      END IF

!
!     We don't expect the voxel list to be empty, but leave now
!     if it is.
!
      IF ( NVXOUT .EQ. 0 ) THEN
         CALL CHKOUT ( 'DSKX02' )
         RETURN
      END IF

!
!     Rather than using the original observer's location, we use a
!     location derived from COORD, which is the intercept of the ray
!     and the surface of the voxel grid.  We start with COORD, convert
!     this location to the model coordinate system, and back outward a
!     bit to make sure we obtain a location outside the grid (we don't
!     want to miss any plates that might be located right on the grid's
!     surface).
!
!     This vertex change is not performed if the vertex is already
!     inside, or within a small margin away from, the voxel grid.
!
      CALL VSUB ( VERTEX, VOXORI, VTXOFF )

      IF (      (  VTXOFF(1) .LT.   -GRDTOL               ) &
           .OR. (  VTXOFF(1) .GT.  ( GRDTOL + GRDEXT(1) ) ) &
           .OR. (  VTXOFF(2) .LT.   -GRDTOL               ) &
           .OR. (  VTXOFF(2) .GT.  ( GRDTOL + GRDEXT(2) ) ) &
           .OR. (  VTXOFF(3) .LT.   -GRDTOL               ) &
           .OR. (  VTXOFF(3) .GT.  ( GRDTOL + GRDEXT(3) ) )   ) THEN

!
!        The vertex is outside of the voxel grid by more than the
!        margin. Move the ray-grid intercept away from the grid to
!        improve numeric performance.
!
         CALL VLCOM3 (  1.D0, VOXORI, VOXSIZ, COORD, &
                       -1.D0, UDIR,   VTX2           )

      END IF

!
!     We are going to need to subtract the location of the observer
!     from vertices of a plate. To speed things up a tiny bit, we'll
!     make 3 copies of the observer's location so that we make a single
!     subroutine call to handle the 3 subtractions.
!
      CALL VEQU ( VTX2, OBSMAT(1,1) )
      CALL VEQU ( VTX2, OBSMAT(1,2) )
      CALL VEQU ( VTX2, OBSMAT(1,3) )

!
!     Use the coarse voxel grid to compress the voxel list. We
!     remove all voxels belonging to empty coarse voxels.
!
      TO = 0

      DO I = 1, NVXOUT
!
!        Find the coordinates, then the ID, of the coarse voxel
!        containing this voxel.
!
         DO J = 1, 3
            CGXYZ(J) = ( ( VOXLST(J,I) - 1 ) / CGSCAL ) + 1
         END DO


         CVID = VOX2ID ( CGXYZ(1),  CGXYZ(2), CGXYZ(3), &
                         CGREXT(1), CGREXT(2)           )

         IF ( CGRPTR(CVID) .GT. 0 ) THEN
!
!           This coarse voxel is non-empty; add the index of the
!           current voxel to the output list.  Save the coordinates of
!           the parent coarse voxel as well.
!
            TO         = TO + 1
            VXLOUT(TO) = I

            DO J = 1, 3
               VXLCG (J,TO) = CGXYZ (J)
            END DO
!
!           Save the coarse voxel start pointer as well.
!
            VXLSTR(TO) = CGRPTR(CVID)

         END IF

      END DO

!
!     Update NVXOUT to be the number of voxels in the compressed list.
!
      NVXOUT = TO
!
!     If the voxel list became empty after compression, we're
!     done.
!
      IF ( NVXOUT .EQ. 0 ) THEN
         CALL CHKOUT( 'DSKX02' )
         RETURN
      END IF

!
!     Initialize PNTR in case the voxel list is empty.
!     (This is a backstop precaution: the voxel list
!     should never be empty at this point.) PNTR will
!     be referenced after the end of the outer loop below.
!
      PNTR   = 1

!
!     The vertex buffer is empty so far.
!
      NVBUF  = 0

!
!     Break up the list of voxels into groups; process each
!     group in turn until we find an intersection or run out
!     of voxels.
!
      GRPSIZ =  MAX ( 1,  (NVXOUT+1)/2)

      NGROUP =  (NVXOUT-1)/GRPSIZ + 1

      GROUP  =  1


      DO WHILE (  ( GROUP .LE. NGROUP )  .AND.  ( .NOT. HAVE )  )


         PNTR   = 1

         GRPBEG = (GROUP-1)*GRPSIZ + 1

         GRPEND = MIN ( GRPBEG + GRPSIZ - 1,  NVXOUT )

         PLROOM = MAXPL

         DO VI = GRPBEG, GRPEND
!
!           Look up the plate list pointer for this voxel.
!
!
!           We begin by finding the offset of the voxel from
!           the base of its parent coarse voxel.
!
            J      =  VXLOUT(VI)

            FX     =  VOXLST(1,J)  -  CGSCAL * ( VXLCG(1,VI) - 1 )
            FY     =  VOXLST(2,J)  -  CGSCAL * ( VXLCG(2,VI) - 1 )
            FZ     =  VOXLST(3,J)  -  CGSCAL * ( VXLCG(3,VI) - 1 )

            OFFSET = FX + CGSCAL*(FY-1) + CGSCL2*(FZ-1)

!
!           Now compute the index of voxel-plate list pointer in
!           the pointer array, and look up the pointer.
!
            J = VXLSTR(VI) + OFFSET - 1

            CALL DSKI02 ( HANDLE, DLADSC, KWVXPT, J, 1, DIM, VOXPTR )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DSKX02' )
               RETURN
            END IF

            IF ( VOXPTR .EQ. -1 ) THEN

               NPLATE = 0

            ELSE
!
!              Get the plate count for this voxel.
!
               CALL DSKI02 ( HANDLE, DLADSC, KWVXPL, VOXPTR, &
                             1,      DIM,    NPLATE          )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'DSKX02' )
                  RETURN
               END IF

            END IF


            IF ( NPLATE .GT. 0 ) THEN

               IF ( NPLATE .LE. PLROOM ) THEN
!
!                 Get the plate list for this voxel.
!
                  CALL DSKI02 ( HANDLE,   DLADSC, KWVXPL, &
                                VOXPTR+1, NPLATE, DIM,    PLATID(PNTR) )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'DSKX02' )
                     RETURN
                  END IF

                  PLROOM = PLROOM - NPLATE

               ELSE

                  CALL SETMSG ( 'NPLATE = #. Available room in ' &
                  //            'PLATID array = #. Array size = #.' )
                  CALL ERRINT ( '#', NPLATE                         )
                  CALL ERRINT ( '#', PLROOM                         )
                  CALL ERRINT ( '#', MAXPL                          )
                  CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'              )
                  CALL CHKOUT ( 'DSKX02'                            )
                  RETURN

               END IF

!
!              Fill in the corresponding elements of the parallel
!              "source" array with the current voxel loop index.
!              XDDA lists these voxels in the order the ray hits
!              them, so the lowest indexed voxels are hit first.
!
               CALL FILLI ( VI, NPLATE, SOURCE(PNTR) )

            END IF

!
!           NPLATE returns zero or greater.
!
            PNTR = PNTR + NPLATE

         END DO
!
!        We've collected all plate info for the current voxel group.
!
         TOTPLT = PNTR-1

!
!        We want to sort the plate ID array and remove duplicates.
!        However, we want to keep the plates ordered according to the
!        sequence in which their containing voxels were hit by the ray.
!        So we find the order vector for the plate ID array, then use
!        this vector to mark duplicates.
!
         CALL ORDERI ( PLATID, TOTPLT, ORDVEC )

!
!        Negate the plate ID of every duplicate we find, leaving
!        the instance in the voxel closest to the ray's origin
!        unmarked. For every pair of plates with the same ID,
!        we'll mark the one with the greater index in the plate
!        ID array.
!
!        We use MINDIX to identify the index, in the plate ID array,
!        of the current unmarked plate ID. MINIDX is re-used for
!        each set of distinct plate IDs.
!
!        The following loop considers plate IDs in increasing order.
!
         MINIDX = ORDVEC(1)

         DO I = 2, TOTPLT
!
!           The condition below uses absolute value because the plate
!           ID at index I-1 may have been "marked" via negation.
!
            IF (              PLATID( ORDVEC(I  ) ) &
                 .EQ.  ABS (  PLATID( ORDVEC(I-1) )  )    ) THEN
!
!              The plates having indices ORDVEC(I-1) and ORDVEC(I) are
!              duplicates.
!
!              At this point MINIDX is the lowest index in the plate ID
!              array of any plate seen so far having an ID equal to
!              PLATID(ORDVEC(I)).
!
!              If ORDVEC(I) is the new "minimum," negate the plate ID
!              at the old "minimum"; otherwise negate the plate ID at
!              index ORDVEC(I).
!
               IF ( ORDVEC(I) .LT. MINIDX ) THEN
!
!                 The plate that was previously at the minimum index is
!                 now considered a duplicate. The new minimum index for
!                 the current plate ID value is ORDVEC(I).
!
                  PLATID(MINIDX)  =  - PLATID(MINIDX)
                  MINIDX          =    ORDVEC(I)

               ELSE
!
!                 The current plate is a duplicate; mark it.
!
                  PLATID( ORDVEC(I) ) =  - PLATID( ORDVEC(I) )

               END IF

            ELSE
!
!              We're looking at a new plate ID. For the moment, this
!              ID has no duplicates.
!
               MINIDX = ORDVEC(I)

            END IF

         END DO

!
!        If something went wrong up above there is no point in
!        going on from here.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DSKX02' )
            RETURN
         END IF

!
!        Now examine each plate in the PLATID list for this voxel group.
!        PNTR has the value of the index available for data in
!        PLATID, so the final location of data is at index PNTR - 1.
!
         EXTRA  = .FALSE.
         FINAL  = 0
         NEAR   = 0.D0

         I      = 1

         DO WHILE (  ( I .LE. TOTPLT ) .AND. ( .NOT. EXTRA )  )
!
!           Retrieve the current plate ID.
!
            J = PLATID(I)


            IF ( J .GT. 0 ) THEN
!
!              This is not a duplicate plate; consider it.
!
               IF ( HAVE ) THEN
!
!                 We already have a hit. See whether this plate resides
!                 in the voxel in which the last hit was found, or in a
!                 later voxel.
!
                  IF ( SOURCE(I) .GT. FINAL ) THEN
!
!                    This is a "late plate": it occurs in a voxel later
!                    than that in which the first valid hit was found.
!
                     EXTRA = .TRUE.

                  END IF

               END IF


               IF ( .NOT. EXTRA ) THEN
!
!                 Fetch the vertex IDs of this plate.
!
                  START = 3*(J-1) + 1

                  CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, &
                                START,  3,      DIM,    VIDS )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'DSKX02' )
                     RETURN
                  END IF

!
!                 Fetch the vertices themselves.
!
                  DO K = 1, 3
!
!                    Any vertex may be buffered already. Look in
!                    the vertex buffer before reading the vertex.
!
                     VLOC = ISRCHI ( VIDS(K), NVBUF, VIDXS )

                     IF ( VLOC .GT. 0 ) THEN
!
!                       The vertex was buffered; just copy it.
!
                        CALL VEQU ( VBUFF(1,VLOC), POINTS(1,K) )

                     ELSE
!
!                       Read in the vertex.
!
                        START = 3*( VIDS(K) - 1 ) + 1

                        CALL DSKD02 ( HANDLE, DLADSC, KWVERT, &
                                      START,  3,      DIM, &
                                      POINTS(1,K)            )

                        IF ( FAILED() ) THEN
                           CALL CHKOUT ( 'DSKX02' )
                           RETURN
                        END IF

!
!                       If there's room, buffer this vertex.
!
                        IF ( NVBUF .LT. BUFSIZ ) THEN

                           NVBUF = NVBUF + 1

                           CALL VEQU ( POINTS(1,K), VBUFF(1,NVBUF) )

                           VIDXS( NVBUF ) = VIDS(K)

                        END IF

                     END IF

                  END DO

               END IF


               IF ( .NOT. EXTRA ) THEN
!
!                 The current plate qualifies for testing using INSANG.
!
!                 Retrieve the model coordinates of the J'th plate's
!                 three vertices. Expand the plate slightly to prevent
!                 round-off error from causing the ray to miss the
!                 plate. Compute the edges of the tetrahedral angle
!                 with the observer as the apex and the vertices as
!                 members of the edge rays. Finally see if the ray
!                 lies inside the tetrahedron.
!
                  CALL VSUBG  ( POINTS,     OBSMAT, 9,  EDGES )

                  CALL PLTEXP ( EDGES,      XPDFRC,     XPNTS )

                  CALL INSANG ( UDIR, &
                                XPNTS(1,1), XPNTS(1,2), XPNTS(1,3), &
                                HITS,       SCALE )

                  IF ( HITS ) THEN
!
!                    Reject intersections with plates that face away
!                    from the ray. Accept intersections with plates
!                    that face toward the ray.
!
                     CALL PLTNRM ( POINTS(1,1), POINTS(1,2), &
                                   POINTS(1,3), NORMAL      )

                     HITS = VDOT( UDIR, NORMAL ) .LE. 0.D0

                  END IF

                  IF ( HITS ) THEN
!
!                    The ray intersects this plate.
!
                     IF ( ( .NOT. HAVE ) .OR. ( SCALE .LT. NEAR ) ) THEN
!
!                       Either this is the first intersection we've
!                       found, or this is the closest intersection to
!                       the vertex we've found. Compute the intercept
!                       coordinates and see whether the intercept is
!                       within the current voxel. Use a small tolerance
!                       for the comparison.

!                       If this intersection point is closer to the
!                       ray's vertex than the last one, pick this point
!                       and the plate it's on.
!
!                       Note that we don't yet know that this solution
!                       is valid.
!
!                          ___    ____   __________
!                          XPT2 = VTX2 + SCALE*UDIR
!
                        CALL VLCOM ( 1.D0, VTX2, SCALE, UDIR, XPT2 )

!
!                       Compute the voxel grid coordinates of the
!                       intercept. HITCOR is a double precision vector
!                       having units of voxels (voxel edge length, to
!                       be precise). Note that the components of HITCOR
!                       are zero-based.
!
                        CALL ZZTOGRID ( XPT2, VOXORI, VOXSIZ, HITCOR )
!
!                       Look up the voxel grid coordinates (integer,
!                       1-based) of the current voxel.
!
                        K     = VXLOUT ( SOURCE(I) )

                        VXC1  = VOXLST(1,K)
                        VXC2  = VOXLST(2,K)
                        VXC3  = VOXLST(3,K)

                        INVOX =       ( HITCOR(1) .GT. (VXC1-XTOL-1) ) &
                                .AND. ( HITCOR(1) .LT. (VXC1+XTOL  ) ) &
                                .AND. ( HITCOR(2) .GT. (VXC2-XTOL-1) ) &
                                .AND. ( HITCOR(2) .LT. (VXC2+XTOL  ) ) &
                                .AND. ( HITCOR(3) .GT. (VXC3-XTOL-1) ) &
                                .AND. ( HITCOR(3) .LT. (VXC3+XTOL  ) )


                        IF ( INVOX ) THEN
!
!                          Reject solutions that are outside of the
!                          segment's boundaries, where the boundaries
!                          are extended using the "greedy" margin.
!
                           CALL DSKGTL ( KEYSGR, GREEDM )

                           CALL ZZINVELT ( XPT2, &
                                           CORSYS, &
                                           DSKDSC(PARIDX), &
                                           DSKDSC(MN1IDX), &
                                           GREEDM, &
                                           NONE, &
                                           INSEG          )

                           IF ( INSEG ) THEN
!
!                             We have a viable intercept. Record the
!                             scale, plate ID, and source voxel index
!                             in the compressed voxel list pointer
!                             array VXLOUT. We won't look for
!                             intercepts beyond the voxel designated by
!                             FINAL.
!
                              CALL VEQU ( XPT2, XPT )

                              HAVE   = .TRUE.
                              NEAR   = SCALE
                              PLID   = J
                              FINAL  = SOURCE(I)
!
!                             Indicate that a solution was found. We'll
!                             keep looking for a better one if PLID is
!                             not the last plate of the current voxel.
!
                              FOUND  = .TRUE.

                           END IF

                        ELSE
!
!                          We must re-consider this plate if we
!                          encounter it in a voxel later in the voxel
!                          list. Remove all duplication markings for
!                          this plate.
!
                           W = ABS(J)

                           DO K = 1, TOTPLT

                              IF ( ABS(PLATID(K)) .EQ. W ) THEN
                                 PLATID(K) = W
                              END IF

                           END DO

                        END IF

                     END IF
!
!                    End of case of checking possible solution
!                    intercept.
!
                  END IF
!
!                 We're done with processing the current hit.
!
               END IF
!
!              We're done with processing the current qualifying plate.
!
            END IF
!
!           We're done with the current plate.
!
!           Fetch the next plate for this voxel group.
!
            I = I + 1

         END DO
!
!        We're done with the current voxel group.
!
         GROUP = GROUP + 1

      END DO
!
!     We've either found an intercept or have run out of voxel groups
!     to check.
!
!     That's all folks.
!
      CALL CHKOUT ( 'DSKX02' )
      RETURN
      END
