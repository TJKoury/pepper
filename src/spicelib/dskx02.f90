!$Procedure DSKX02 ( DSK, ray-surface intercept, type 2 )

      subroutine dskx02 ( handle, dladsc, vertex, &
                          raydir, plid,   xpt,    found )
      use pepper_kinds, only: ip, wp

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

      implicit none

      include              'dla.inc'
      include              'dskdsc.inc'
      include              'dsk02.inc'
      include              'dsktol.inc'

      integer(ip)           handle
      integer(ip)           dladsc   ( * )
      real(wp)              vertex   ( 3 )
      real(wp)              raydir   ( 3 )
      integer(ip)           plid
      real(wp)              xpt      ( 3 )
      logical               found

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
!        Example code begins here.
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
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos_3_3.bds, the output
!        was:
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
!        [...]
!
!        Warning: incomplete output. Only 100 out of 1135 lines have
!        been provided.
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
      real(wp)              dpmax
      real(wp)              vdot

      logical               return
      logical               failed
      logical               vzero

!
!     Statement function type declarations
!
      integer(ip)           vox2id

!
!     Local parameters
!

!
!     Tolerance used for vertex-voxel grid distance test:
!
      real(wp), parameter :: vtxtol = 1.0e-12_wp

!
!     Maximum number of voxels we can accept for
!     one XDDA call.
!
      integer(ip), parameter :: nvxlst = 50000

!
!     Maximum number of plates we work with
!     at any time.
!
      integer(ip), parameter :: maxpl = maxplt/125

!
!     Parameter indicating no coordinates are to be excluded
!     in the test for a point being within segment boundaries.
!
      integer(ip), parameter :: none = 0

!
!     Local Variables
!
      integer(ip)           isrchi

      integer(ip), parameter :: bufsiz = 200

      real(wp)              coord  ( 3 )
      real(wp)              edges  ( 3, 3 )
      real(wp)              dskdsc ( dskdsz )
      real(wp)              grdext ( 3 )
      real(wp)              grdtol
      real(wp)              greedm
      real(wp)              hitcor ( 3 )
      real(wp)              near
      real(wp)              normal ( 3 )
      real(wp)              obsmat ( 3, 3 )
      real(wp)              points ( 3, 3 )
      real(wp)              scale
      real(wp)              udir   ( 3 )
      real(wp)              vbuff  ( 3, bufsiz )
      real(wp)              voxori ( 3 )
      real(wp)              voxsiz
      real(wp)              vtx2   ( 3 )
      real(wp)              vtxbds ( 2, 3 )
      real(wp)              vtxoff ( 3 )
      real(wp)              xpdfrc
      real(wp)              xpnts  ( 3, 3 )
      real(wp)              xpt2   ( 3 )
      real(wp)              xtol

      integer(ip)           cgrext ( 3 )
      integer(ip)           cgrptr ( maxcgr )
      integer(ip)           cgscal
      integer(ip)           cgscl2
      integer(ip)           cgxyz  ( 3 )
      integer(ip)           corsys
      integer(ip)           cvid
      integer(ip)           dim
      integer(ip)           dim1
      integer(ip)           dim2
      integer(ip)           final
      integer(ip)           fx
      integer(ip)           fy
      integer(ip)           fz
      integer(ip)           group
      integer(ip)           grpbeg
      integer(ip)           grpend
      integer(ip)           grpsiz
      integer(ip)           i
      integer(ip)           i1
      integer(ip)           i2
      integer(ip)           i3
      integer(ip)           j
      integer(ip)           k
      integer(ip)           minidx
      integer(ip)           ncgr
      integer(ip)           ngroup
      integer(ip)           np
      integer(ip)           nplate
      integer(ip)           nv
      integer(ip)           nvbuf
      integer(ip)           nvxout
      integer(ip)           nvxtot
      integer(ip)           offset
      integer(ip)           ordvec ( maxpl )
      integer(ip)           platid ( maxpl )
      integer(ip)           plroom
      integer(ip)           pntr
      integer(ip)           prvdsc ( dladsz )
      integer(ip)           prvhan
      integer(ip)           source ( maxpl )
      integer(ip)           start
      integer(ip)           to
      integer(ip)           totplt
      integer(ip)           vgrext ( 3 )
      integer(ip)           vi
      integer(ip)           vids   ( 3 )
      integer(ip)           vidxs  (    bufsiz )
      integer(ip)           vloc
      integer(ip)           voxlst ( 3, nvxlst )
      integer(ip)           voxnpl
      integer(ip)           voxnpt
      integer(ip)           voxptr
      integer(ip)           vtxnpl
      integer(ip)           vxc1
      integer(ip)           vxc2
      integer(ip)           vxc3
      integer(ip)           vxlcg  ( 3, nvxlst )
      integer(ip)           vxlout ( nvxlst )
      integer(ip)           vxlstr ( nvxlst )
      integer(ip)           w

      logical               boxhit
      logical               extra
      logical               have
      logical               hits
      logical               inseg
      logical               invox
      logical               newseg

!
!     Saved variables
!
      save                  cgrext
      save                  cgrptr
      save                  cgscal
      save                  cgscl2
      save                  corsys
      save                  dskdsc
      save                  grdext
      save                  grdtol
      save                  ncgr
      save                  np
      save                  nv
      save                  nvxtot
      save                  ordvec
      save                  platid
      save                  prvdsc
      save                  prvhan
      save                  source
      save                  vbuff
      save                  vidxs
      save                  vgrext
      save                  voxlst
      save                  voxnpl
      save                  voxnpt
      save                  voxori
      save                  voxsiz
      save                  vtxbds
      save                  vtxnpl
      save                  vxlcg
      save                  vxlout
      save                  vxlstr
      save                  xtol

!
!     Initial values
!
      data                  grdext  / 3 * -1.0_wp /
      data                  prvhan  / 0 /
      data                  prvdsc  / dladsz * 0 /

!
!     Statement functions
!
      vox2id ( i1, i2, i3, dim1, dim2 ) =  i1 + dim1*( i2 +  i3* dim2 &
                                                          - (1 + dim2) )

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Until we have better knowledge we assume there is no intersection.
!
      plid   =  0
      found  = .false.
      have   = .false.
      near   = dpmax()

!
!     Initialize the vertex buffer.
!
      call cleari( bufsiz, vidxs )

!
!     Check whether the ray direction vector is the zero vector.
!
      if ( vzero( raydir ) ) then

         call setmsg ( 'Ray direction is the zero vector.' )
         call sigerr ( 'SPICE(RAYISZEROVECTOR)'            )
         return

      end if

!
!     Obtain the unit vector of the ray from the observer.
!
      call vhat ( raydir, udir )

!
!     Decide whether we're looking at a new segment.
!
      newseg = .true.

      if ( handle == prvhan ) then
!
!        The input handle matches the previous handle.  Note that the
!        initial value of PRVHAN is 0, which is never a valid handle,
!        so on the first pass, this test will fail.
!
         if (       ( dladsc(ibsidx) == prvdsc(ibsidx) ) &
              .and. ( dladsc(dbsidx) == prvdsc(dbsidx) ) &
              .and. ( dladsc(cbsidx) == prvdsc(cbsidx) )  ) then
!
!           All of the DLA segment base addresses match.
!
            newseg = .false.

         end if

      end if

      if ( newseg ) then
!
!        Make sure we can't have a match with an earlier
!        segment on a subsequent call, if we exit this
!        block due to an error.
!
         prvhan = 0

!
!        Retrieve the voxel grid origin in model
!        units and calculate the farthest extent of the
!        voxel grid in voxel space.
!
         call dskb02 ( handle, dladsc, nv,     np,     nvxtot, &
                       vtxbds, voxsiz, voxori, vgrext, &
                       cgscal, vtxnpl, voxnpt, voxnpl         )

         if ( failed() ) return

!
!        Compute the grid dimensions in units of km. First check
!        the voxel size.
!
         if ( voxsiz == 0 ) then
            call setmsg ( 'Voxel size is zero. This is an error ' &
            //            'in the DSK file attached to handle #.' )
            call errint ( '#', handle                             )
            call sigerr ( 'SPICE(INVALIDVALUE)'                   )
            return

         end if

         do i = 1, 3
            grdext(i) = vgrext(i) * voxsiz
         end do

!
!        Set the margin used for checking whether the ray's vertex
!        is close to the voxel grid.
!
         grdtol = vtxtol * max( grdext(1), grdext(2), grdext(3) )

!
!        Check the coarse grid voxel scale.
!
         if ( cgscal < 1 ) then

            call setmsg ( 'Coarse grid scale = #; should be >= 1.' )
            call errint ( '#', cgscal                              )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
            return

         end if

!
!        Get the coarse voxel grid dimensions and the coarse voxel
!        occupancy flags.
!
         do i = 1, 3
            cgrext(i) = vgrext(i) / cgscal
         end do

         cgscl2 = cgscal * cgscal

         ncgr   = nvxtot / cgscal**3

         if ( ncgr > maxcgr ) then

            call setmsg ( 'Coarse grid size NCGR = #. Buffer size = #' )
            call errint ( '#', ncgr                                    )
            call errint ( '#', maxcgr                                  )
            call sigerr ( 'SPICE(GRIDTOOLARGE)'                        )
            return

         end if

         call dski02 ( handle, dladsc, kwcgpt, 1, maxcgr, dim, cgrptr )

         call dskgd  ( handle, dladsc, dskdsc )

         if ( failed() ) return

         corsys = nint( dskdsc(sysidx) )

         prvhan = handle

         call movei ( dladsc, dladsz, prvdsc )

      end if

!
!     Compute tolerance used for determining whether an intercept
!     is inside a voxel. The expansion fraction must be fetched
!     on every call to DSKX02.
!
      call dskgtl ( keyxfr, xpdfrc )

      xtol   = xpdfrc * max( abs( grdext(1) ), &
                             abs( grdext(2) ), &
                             abs( grdext(3) )  )

!
!     Find the ray intercept on the surface of the fine voxel grid,
!     if the intercept exists.
!
      call zzraybox ( vertex, raydir, voxori, grdext, vtx2, boxhit )

      if ( .not. boxhit ) return

!
!     Convert the grid intercept to voxel space coordinates.
!     The result COORD will be used as the ray's vertex in XDDA.
!
      call zztogrid ( vtx2, voxori, voxsiz, coord )

!
!     Determine the voxels hit by the ray.
!
      call xdda ( coord, udir, vgrext, nvxlst, nvxout, voxlst )

      if ( failed() ) return

!
!     We don't expect the voxel list to be empty, but leave now
!     if it is.
!
      if ( nvxout == 0 ) return

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
      call vsub ( vertex, voxori, vtxoff )

      if (      (  vtxoff(1) <   -grdtol               ) &
           .or. (  vtxoff(1) >  ( grdtol + grdext(1) ) ) &
           .or. (  vtxoff(2) <   -grdtol               ) &
           .or. (  vtxoff(2) >  ( grdtol + grdext(2) ) ) &
           .or. (  vtxoff(3) <   -grdtol               ) &
           .or. (  vtxoff(3) >  ( grdtol + grdext(3) ) )   ) then

!
!        The vertex is outside of the voxel grid by more than the
!        margin. Move the ray-grid intercept away from the grid to
!        improve numeric performance.
!
         call vlcom3 (  1.0_wp, voxori, voxsiz, coord, &
                       -1.0_wp, udir,   vtx2           )

      end if

!
!     We are going to need to subtract the location of the observer
!     from vertices of a plate. To speed things up a tiny bit, we'll
!     make 3 copies of the observer's location so that we make a single
!     subroutine call to handle the 3 subtractions.
!
      call vequ ( vtx2, obsmat(1,1) )
      call vequ ( vtx2, obsmat(1,2) )
      call vequ ( vtx2, obsmat(1,3) )

!
!     Use the coarse voxel grid to compress the voxel list. We
!     remove all voxels belonging to empty coarse voxels.
!
      to = 0

      do i = 1, nvxout
!
!        Find the coordinates, then the ID, of the coarse voxel
!        containing this voxel.
!
         do j = 1, 3
            cgxyz(j) = ( ( voxlst(j,i) - 1 ) / cgscal ) + 1
         end do

         cvid = vox2id ( cgxyz(1),  cgxyz(2), cgxyz(3), &
                         cgrext(1), cgrext(2)           )

         if ( cgrptr(cvid) > 0 ) then
!
!           This coarse voxel is non-empty; add the index of the
!           current voxel to the output list.  Save the coordinates of
!           the parent coarse voxel as well.
!
            to         = to + 1
            vxlout(to) = i

            do j = 1, 3
               vxlcg (j,to) = cgxyz (j)
            end do
!
!           Save the coarse voxel start pointer as well.
!
            vxlstr(to) = cgrptr(cvid)

         end if

      end do

!
!     Update NVXOUT to be the number of voxels in the compressed list.
!
      nvxout = to
!
!     If the voxel list became empty after compression, we're
!     done.
!
      if ( nvxout == 0 ) return

!
!     Initialize PNTR in case the voxel list is empty.
!     (This is a backstop precaution: the voxel list
!     should never be empty at this point.) PNTR will
!     be referenced after the end of the outer loop below.
!
      pntr   = 1

!
!     The vertex buffer is empty so far.
!
      nvbuf  = 0

!
!     Break up the list of voxels into groups; process each
!     group in turn until we find an intersection or run out
!     of voxels.
!
      grpsiz =  max ( 1,  (nvxout+1)/2)

      ngroup =  (nvxout-1)/grpsiz + 1

      group  =  1

      do while (  ( group <= ngroup )  .and.  ( .not. have )  )

         pntr   = 1

         grpbeg = (group-1)*grpsiz + 1

         grpend = min ( grpbeg + grpsiz - 1,  nvxout )

         plroom = maxpl

         do vi = grpbeg, grpend
!
!           Look up the plate list pointer for this voxel.
!
!           We begin by finding the offset of the voxel from
!           the base of its parent coarse voxel.
!
            j      =  vxlout(vi)

            fx     =  voxlst(1,j)  -  cgscal * ( vxlcg(1,vi) - 1 )
            fy     =  voxlst(2,j)  -  cgscal * ( vxlcg(2,vi) - 1 )
            fz     =  voxlst(3,j)  -  cgscal * ( vxlcg(3,vi) - 1 )

            offset = fx + cgscal*(fy-1) + cgscl2*(fz-1)

!
!           Now compute the index of voxel-plate list pointer in
!           the pointer array, and look up the pointer.
!
            j = vxlstr(vi) + offset - 1

            call dski02 ( handle, dladsc, kwvxpt, j, 1, dim, voxptr )

            if ( failed() ) return

            if ( voxptr == -1 ) then

               nplate = 0

            else
!
!              Get the plate count for this voxel.
!
               call dski02 ( handle, dladsc, kwvxpl, voxptr, &
                             1,      dim,    nplate          )

               if ( failed() ) return

            end if

            if ( nplate > 0 ) then

               if ( nplate <= plroom ) then
!
!                 Get the plate list for this voxel.
!
                  call dski02 ( handle,   dladsc, kwvxpl, &
                                voxptr+1, nplate, dim,    platid(pntr) )

                  if ( failed() ) return

                  plroom = plroom - nplate

               else

                  call setmsg ( 'NPLATE = #. Available room in ' &
                  //            'PLATID array = #. Array size = #.' )
                  call errint ( '#', nplate                         )
                  call errint ( '#', plroom                         )
                  call errint ( '#', maxpl                          )
                  call sigerr ( 'SPICE(ARRAYTOOSMALL)'              )
                  return

               end if

!
!              Fill in the corresponding elements of the parallel
!              "source" array with the current voxel loop index.
!              XDDA lists these voxels in the order the ray hits
!              them, so the lowest indexed voxels are hit first.
!
               call filli ( vi, nplate, source(pntr) )

            end if

!
!           NPLATE returns zero or greater.
!
            pntr = pntr + nplate

         end do
!
!        We've collected all plate info for the current voxel group.
!
         totplt = pntr-1

!
!        We want to sort the plate ID array and remove duplicates.
!        However, we want to keep the plates ordered according to the
!        sequence in which their containing voxels were hit by the ray.
!        So we find the order vector for the plate ID array, then use
!        this vector to mark duplicates.
!
         call orderi ( platid, totplt, ordvec )

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
         minidx = ordvec(1)

         do i = 2, totplt
!
!           The condition below uses absolute value because the plate
!           ID at index I-1 may have been "marked" via negation.
!
            if (              platid( ordvec(i  ) ) &
                 ==  abs (  platid( ordvec(i-1) )  )    ) then
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
               if ( ordvec(i) < minidx ) then
!
!                 The plate that was previously at the minimum index is
!                 now considered a duplicate. The new minimum index for
!                 the current plate ID value is ORDVEC(I).
!
                  platid(minidx)  =  - platid(minidx)
                  minidx          =    ordvec(i)

               else
!
!                 The current plate is a duplicate; mark it.
!
                  platid( ordvec(i) ) =  - platid( ordvec(i) )

               end if

            else
!
!              We're looking at a new plate ID. For the moment, this
!              ID has no duplicates.
!
               minidx = ordvec(i)

            end if

         end do

!
!        If something went wrong up above there is no point in
!        going on from here.
!
         if ( failed() ) return

!
!        Now examine each plate in the PLATID list for this voxel group.
!        PNTR has the value of the index available for data in
!        PLATID, so the final location of data is at index PNTR - 1.
!
         extra  = .false.
         final  = 0
         near   = 0.0_wp

         i      = 1

         do while (  ( i <= totplt ) .and. ( .not. extra )  )
!
!           Retrieve the current plate ID.
!
            j = platid(i)

            if ( j > 0 ) then
!
!              This is not a duplicate plate; consider it.
!
               if ( have ) then
!
!                 We already have a hit. See whether this plate resides
!                 in the voxel in which the last hit was found, or in a
!                 later voxel.
!
                  if ( source(i) > final ) then
!
!                    This is a "late plate": it occurs in a voxel later
!                    than that in which the first valid hit was found.
!
                     extra = .true.

                  end if

               end if

               if ( .not. extra ) then
!
!                 Fetch the vertex IDs of this plate.
!
                  start = 3*(j-1) + 1

                  call dski02 ( handle, dladsc, kwplat, &
                                start,  3,      dim,    vids )

                  if ( failed() ) return

!
!                 Fetch the vertices themselves.
!
                  do k = 1, 3
!
!                    Any vertex may be buffered already. Look in
!                    the vertex buffer before reading the vertex.
!
                     vloc = isrchi ( vids(k), nvbuf, vidxs )

                     if ( vloc > 0 ) then
!
!                       The vertex was buffered; just copy it.
!
                        call vequ ( vbuff(1,vloc), points(1,k) )

                     else
!
!                       Read in the vertex.
!
                        start = 3*( vids(k) - 1 ) + 1

                        call dskd02 ( handle, dladsc, kwvert, &
                                      start,  3,      dim, &
                                      points(1,k)            )

                        if ( failed() ) return

!
!                       If there's room, buffer this vertex.
!
                        if ( nvbuf < bufsiz ) then

                           nvbuf = nvbuf + 1

                           call vequ ( points(1,k), vbuff(1,nvbuf) )

                           vidxs( nvbuf ) = vids(k)

                        end if

                     end if

                  end do

               end if

               if ( .not. extra ) then
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
                  call vsubg  ( points,     obsmat, 9,  edges )

                  call pltexp ( edges,      xpdfrc,     xpnts )

                  call insang ( udir, &
                                xpnts(1,1), xpnts(1,2), xpnts(1,3), &
                                hits,       scale )

                  if ( hits ) then
!
!                    Reject intersections with plates that face away
!                    from the ray. Accept intersections with plates
!                    that face toward the ray.
!
                     call pltnrm ( points(1,1), points(1,2), &
                                   points(1,3), normal      )

                     hits = vdot( udir, normal ) <= 0.0_wp

                  end if

                  if ( hits ) then
!
!                    The ray intersects this plate.
!
                     if ( ( .not. have ) .or. ( scale < near ) ) then
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
                        call vlcom ( 1.0_wp, vtx2, scale, udir, xpt2 )

!
!                       Compute the voxel grid coordinates of the
!                       intercept. HITCOR is a double precision vector
!                       having units of voxels (voxel edge length, to
!                       be precise). Note that the components of HITCOR
!                       are zero-based.
!
                        call zztogrid ( xpt2, voxori, voxsiz, hitcor )
!
!                       Look up the voxel grid coordinates (integer,
!                       1-based) of the current voxel.
!
                        k     = vxlout ( source(i) )

                        vxc1  = voxlst(1,k)
                        vxc2  = voxlst(2,k)
                        vxc3  = voxlst(3,k)

                        invox =       ( hitcor(1) > (vxc1-xtol-1) ) &
                                .and. ( hitcor(1) < (vxc1+xtol  ) ) &
                                .and. ( hitcor(2) > (vxc2-xtol-1) ) &
                                .and. ( hitcor(2) < (vxc2+xtol  ) ) &
                                .and. ( hitcor(3) > (vxc3-xtol-1) ) &
                                .and. ( hitcor(3) < (vxc3+xtol  ) )

                        if ( invox ) then
!
!                          Reject solutions that are outside of the
!                          segment's boundaries, where the boundaries
!                          are extended using the "greedy" margin.
!
                           call dskgtl ( keysgr, greedm )

                           call zzinvelt ( xpt2, &
                                           corsys, &
                                           dskdsc(paridx), &
                                           dskdsc(mn1idx), &
                                           greedm, &
                                           none, &
                                           inseg          )

                           if ( inseg ) then
!
!                             We have a viable intercept. Record the
!                             scale, plate ID, and source voxel index
!                             in the compressed voxel list pointer
!                             array VXLOUT. We won't look for
!                             intercepts beyond the voxel designated by
!                             FINAL.
!
                              call vequ ( xpt2, xpt )

                              have   = .true.
                              near   = scale
                              plid   = j
                              final  = source(i)
!
!                             Indicate that a solution was found. We'll
!                             keep looking for a better one if PLID is
!                             not the last plate of the current voxel.
!
                              found  = .true.

                           end if

                        else
!
!                          We must re-consider this plate if we
!                          encounter it in a voxel later in the voxel
!                          list. Remove all duplication markings for
!                          this plate.
!
                           w = abs(j)

                           do k = 1, totplt

                              if ( abs(platid(k)) == w ) then
                                 platid(k) = w
                              end if

                           end do

                        end if

                     end if
!
!                    End of case of checking possible solution
!                    intercept.
!
                  end if
!
!                 We're done with processing the current hit.
!
               end if
!
!              We're done with processing the current qualifying plate.
!
            end if
!
!           We're done with the current plate.
!
!           Fetch the next plate for this voxel group.
!
            i = i + 1

         end do
!
!        We're done with the current voxel group.
!
         group = group + 1

      end do
!
!     We've either found an intercept or have run out of voxel groups
!     to check.
!
!     That's all folks.
!
      return
      end
