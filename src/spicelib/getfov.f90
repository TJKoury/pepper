!$Procedure GETFOV ( Get instrument FOV parameters )

      subroutine getfov ( instid, &
                          room, &
                          shape, &
                          frame, &
                          bsight, &
                          n, &
                          bounds  )

! $ Abstract
!
!     Return the field-of-view (FOV) parameters for a specified
!     instrument. The instrument is specified by its NAIF ID code.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     FOV
!     INSTRUMENT
!
! $ Declarations

      implicit none

      double precision      mincos
      parameter           ( mincos = 1.0d-15 )

      integer               instid
      integer               room
      character*(*)         shape
      character*(*)         frame
      double precision      bsight ( 3    )
      integer               n
      double precision      bounds ( 3, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INSTID     I   NAIF ID of an instrument.
!     ROOM       I   Maximum number of vectors that can be returned.
!     SHAPE      O   Instrument FOV shape.
!     FRAME      O   Name of the frame in which FOV vectors are defined.
!     BSIGHT     O   Boresight vector.
!     N          O   Number of boundary vectors returned.
!     BOUNDS     O   FOV boundary vectors.
!
! $ Detailed_Input
!
!     INSTID   is the NAIF ID of an instrument.
!
!     ROOM     is the maximum number of 3-dimensional vectors that can
!              be returned in BOUNDS.
!
! $ Detailed_Output
!
!     SHAPE    is a character string that describes the "shape" of
!              the field of view. Possible values returned are:
!
!                 'POLYGON'
!                 'RECTANGLE'
!                 'CIRCLE'
!                 'ELLIPSE'
!
!              If the value of SHAPE is 'POLYGON' the field of view
!              of the instrument is a pyramidal polyhedron. The
!              vertex of the pyramid is at the instrument focal
!              point. The rays along the edges of the pyramid are
!              parallel to the vectors returned in BOUNDS.
!
!              If the value of SHAPE is 'RECTANGLE' the field of view
!              of the instrument is a rectangular pyramid. The vertex
!              of the pyramid is at the instrument focal point. The
!              rays along the edges of the pyramid are parallel to
!              the vectors returned in BOUNDS. Moreover, in this
!              case, the boresight points along the axis of symmetry
!              of the rectangular pyramid.
!
!              If the value of SHAPE is 'CIRCLE' the field of view of
!              the instrument is a circular cone centered on the
!              boresight vector. The vertex of the cone is at the
!              instrument focal point. A single vector will be
!              returned in BOUNDS. This vector will be parallel to a
!              ray that lies in the cone that makes up the boundary
!              of the field of view.
!
!              If the value of SHAPE is 'ELLIPSE' the field of view
!              of the instrument is an elliptical cone with the
!              boresight vector as the axis of the cone. In this
!              case two vectors are returned in BOUNDS. One of the
!              vectors returned in BOUNDS points to the end of the
!              semi-major axis of a perpendicular cross section of
!              the elliptic cone. The other vector points to the end
!              of the semi-minor axis of a perpendicular cross
!              section of the cone.
!
!     FRAME    is the name of the reference frame in which the field
!              of view boundary vectors are defined.
!
!     BSIGHT   is a vector representing the principal instrument view
!              direction that can be
!
!                 -  the central pixel view direction,
!                 -  the optical axis direction,
!                 -  the FOV geometric center view direction,
!                 -  an axis of the FOV frame,
!
!              or any other vector specified for this purpose
!              in the IK FOV definition. The length of BSIGHT
!              is not specified other than being non-zero.
!
!     N        is the number of boundary vectors returned.
!
!     BOUNDS   is an array of vectors that point to the "corners" of
!              the instrument field of view. (See the discussion
!              accompanying SHAPE for an expansion of the term
!              "corner of the field of view.") Note that the vectors
!              returned in BOUNDS are not necessarily unit vectors.
!              Their magnitudes will be as set in the IK (for
!              'CORNERS'-style FOV specifications) or the same as the
!              magnitude of the boresight (for 'ANGLES'-style FOV
!              specifications.)
!
! $ Parameters
!
!     MINCOS   is the lower limit on the value of the cosine of the
!              cross or reference angles in the 'ANGLES' specification
!              cases. The current value for MINCOS is 1.0D-15.
!
! $ Exceptions
!
!     1)  If the frame associated with the instrument can not be found,
!         the error SPICE(FRAMEMISSING) is signaled.
!
!     2)  If the shape of the instrument field of view can not be found
!         in the kernel pool, the error SPICE(SHAPEMISSING) is signaled
!         signaled.
!
!     3)  If the FOV_SHAPE specified by the instrument kernel is not
!         one of the four values: 'CIRCLE', 'POLYGON', 'ELLIPSE', or
!         'RECTANGLE', the error SPICE(SHAPENOTSUPPORTED) is signaled.
!         If the 'ANGLES' specification is used, FOV_SHAPE must be
!         one of the three values: 'CIRCLE', 'ELLIPSE', or 'RECTANGLE'.
!
!     4)  If the direction of the boresight cannot be located in the
!         kernel pool, the error SPICE(BORESIGHTMISSING) is signaled.
!
!     5)  If the number of components for the boresight vector in the
!         kernel pool is not 3, or they are not numeric, the error
!         SPICE(BADBORESIGHTSPEC) is signaled.
!
!     6)  If the boresight vector is the zero vector, the error
!         SPICE(ZEROBORESIGHT) is signaled.
!
!     7)  If the 'ANGLES' specification is not present in the kernel
!         pool and the boundary vectors for the edge of the field of
!         view cannot be found in the kernel pool, the error
!         SPICE(BOUNDARYMISSING) is signaled.
!
!     8)  If there is insufficient room (as specified by the argument
!         ROOM) to return all of the vectors associated with the
!         boundary of the field of view, the error
!         SPICE(BOUNDARYTOOBIG) is signaled.
!
!     9)  If the number of components of vectors making up the field of
!         view is not a multiple of 3, the error SPICE(BADBOUNDARY) is
!         signaled.
!
!     10) If the number of components of vectors making up the field of
!         view is not compatible with the shape specified for the field
!         of view, the error SPICE(BADBOUNDARY) is signaled.
!
!     11) If the reference vector for the 'ANGLES' specification can not
!         be found in the kernel pool, the error SPICE(REFVECTORMISSING)
!         is signaled.
!
!     12) If the reference vector stored in the kernel pool to support
!         the 'ANGLES' specification contains an incorrect number of
!         components, contains 3 character components, or is parallel to
!         the boresight, the error SPICE(BADREFVECTORSPEC) is signaled.
!
!     13) If the 'ANGLES' specification is present in the kernel pool
!         and the reference angle stored in the kernel pool to support
!         the 'ANGLES' specification is absent from the kernel pool, the
!         error SPICE(REFANGLEMISSING) is signaled.
!
!     14) If the keyword that stores the angular units for the angles
!         used in the 'ANGLES' specification is absent from the kernel
!         pool, the error SPICE(UNITSMISSING) is signaled.
!
!     15) If the value used for the units in the 'ANGLES' specification
!         is not one of the supported angular units of CONVRT, an error
!         is signaled by a routine in the call tree of this routine.
!
!     16) If the keyword that stores the cross angle for the 'ANGLES'
!         specification is needed and is absent from the kernel pool,
!         the error SPICE(CROSSANGLEMISSING) is signaled.
!
!     17) If the angles for the 'RECTANGLE'/'ANGLES' specification case
!         have cosines that are less than those stored in the parameter
!         MINCOS, the error SPICE(BADBOUNDARY) is signaled.
!
!     18) If the class specification contains something other than
!         'ANGLES' or 'CORNERS', the error SPICE(UNSUPPORTEDSPEC) is
!         signaled.
!
!     19) In the event that the CLASS_SPEC keyword is absent from the
!         kernel pool for the instrument whose FOV is sought, this
!         module assumes the 'CORNERS' specification is to be utilized.
!
! $ Files
!
!     This routine relies upon having successfully loaded an instrument
!     kernel (IK file) via the routine FURNSH prior to calling this
!     routine.
!
! $ Particulars
!
!     This routine provides a common interface for retrieving from the
!     kernel pool the geometric characteristics of an instrument field
!     of view for a wide variety of remote sensing instruments
!     across many different space missions.
!
!     Given the NAIF instrument ID, (and having "loaded" the
!     instrument field of view description via the routine FURNSH)
!     this routine returns the boresight of the instrument, the
!     "shape" of the field of view, a collection of vectors
!     that point along the edges of the field of view, and the
!     name of the reference frame in which these vectors are defined.
!
!     Currently this routine supports two classes of specifications
!     for FOV definitions: "corners" and "angles".
!
!     The "corners" specification requires that the following keywords
!     defining the shape, boresight, boundary vectors, and reference
!     frame of the FOV be provided in one of the text kernel files
!     (normally an IK file) loaded into the kernel pool (in the
!     keywords below <INSTID> is replaced with the instrument ID as
!     passed into the module):
!
!        INS<INSTID>_FOV_CLASS_SPEC         must be set to 'CORNERS' or
!                                           omitted to indicate the
!                                           "corners"-class
!                                           specification.
!
!        INS<INSTID>_FOV_SHAPE              must be set to one of these
!                                           values:
!
!                                              'CIRCLE'
!                                              'ELLIPSE'
!                                              'RECTANGLE'
!                                              'POLYGON'
!
!        INS<INSTID>_FOV_FRAME              must contain the name of
!                                           the frame in which the
!                                           boresight and boundary
!                                           corner vectors are defined.
!
!        INS<INSTID>_BORESIGHT              must be set to a 3D vector
!                                           defining the boresight in
!                                           the FOV frame specified in
!                                           the FOV_FRAME keyword.
!
!        INS<INSTID>_FOV_BOUNDARY   or
!        INS<INSTID>_FOV_BOUNDARY_CORNERS   must be set to one (for
!                                           FOV_SHAPE = 'CIRCLE'), two
!                                           (for FOV_SHAPE =
!                                           'ELLIPSE'), four (for
!                                           FOV_SHAPE = 'RECTANGLE'),
!                                           or three or more (for
!                                           'POLYGON') 3D vectors
!                                           defining the corners of the
!                                           FOV in the FOV frame
!                                           specified in the FOV_FRAME
!                                           keyword. The vectors should
!                                           be listed in either
!                                           clockwise or
!                                           counterclockwise order.
!                                           This is required by some
!                                           SPICE routines that make
!                                           use of FOV specifications.
!
!     The "angles" specification requires the following keywords
!     defining the shape, boresight, reference vector, reference and
!     cross angular extents of the FOV be provided in one of the text
!     kernel files (normally an IK file) loaded into the kernel
!     pool (in the keywords below <INSTID> is replaced with the
!     instrument ID as passed into the module):
!
!        INS<INSTID>_FOV_CLASS_SPEC         must be set to 'ANGLES' to
!                                           indicate the "angles"-class
!                                           specification.
!
!        INS<INSTID>_FOV_SHAPE              must be set to one of these
!                                           values:
!
!                                              'CIRCLE'
!                                              'ELLIPSE'
!                                              'RECTANGLE'
!
!        INS<INSTID>_FOV_FRAME              must contain the name of
!                                           the frame in which the
!                                           boresight and the computed
!                                           boundary corner vectors are
!                                           defined.
!
!        INS<INSTID>_BORESIGHT              must be set to a 3D vector
!                                           defining the boresight in
!                                           the FOV frame specified in
!                                           the FOV_FRAME keyword.
!
!        INS<INSTID>_FOV_REF_VECTOR         must be set to a 3D vector
!                                           that together with the
!                                           boresight vector defines
!                                           the plane in which the
!                                           first angular extent of the
!                                           FOV specified in the
!                                           FOV_REF_ANGLE keyword is
!                                           measured.
!
!        INS<INSTID>_FOV_REF_ANGLE          must be set to the angle
!                                           that is 1/2 of the total
!                                           FOV angular extent in the
!                                           plane defined by the
!                                           boresight and the vector
!                                           specified in the
!                                           FOV_REF_VECTOR keyword. The
!                                           the FOV angular half-extents
!                                           are measured from the
!                                           boresight vector.
!
!        INS<INSTID>_FOV_CROSS_ANGLE        must be set to the angle
!                                           that is 1/2 of the total
!                                           FOV angular extent in the
!                                           plane containing the
!                                           boresight and perpendicular
!                                           to the plane defined by the
!                                           boresight and the vector
!                                           specified in the
!                                           FOV_REF_VECTOR keyword. The
!                                           the FOV angular half-extents
!                                           are measured from the
!                                           boresight vector. This
!                                           keyword is not required for
!                                           FOV_SHAPE = 'CIRCLE'.
!
!        INS<INSTID>_FOV_ANGLE_UNITS        must specify units for the
!                                           angles given in the
!                                           FOV_REF_ANGLE and
!                                           FOV_CROSS_ANGLE keywords.
!                                           Any angular units
!                                           recognized by CONVRT are
!                                           acceptable.
!
!     The INS<INSTID>_FOV_REF_ANGLE and INS<INSTID>_FOV_CROSS_ANGLE
!     keywords can have any values for the 'CIRCLE' and 'ELLIPSE'
!     FOV shapes but must satisfy the condition COS( ANGLE ) > 0 for
!     the 'RECTANGLE' shape.
!
!     This routine is intended to be an intermediate level routine.
!     It is expected that users of this routine will be familiar
!     with the SPICE frames subsystem and will be comfortable writing
!     software to further manipulate the vectors retrieved by this
!     routine.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input,
!     the compiler and supporting libraries, and the machine specific
!     arithmetic implementation.
!
!     1) Load an IK, fetch the parameters for each of the FOVs defined
!        within and print these parameters to the screen.
!
!        Use the kernel shown below, an IK defining four FOVs of
!        various shapes and sizes, to load the FOV definitions.
!
!
!           KPL/IK
!
!           File name: getfov_ex1.ti
!
!           The keywords below define a circular, 10-degree wide FOV
!           with the boresight along the +Z axis of the 'SC999_INST001'
!           frame for an instrument with ID -999001 using the
!           "angles"-class specification.
!
!           \begindata
!              INS-999001_FOV_CLASS_SPEC       = 'ANGLES'
!              INS-999001_FOV_SHAPE            = 'CIRCLE'
!              INS-999001_FOV_FRAME            = 'SC999_INST001'
!              INS-999001_BORESIGHT            = ( 0.0, 0.0, 1.0 )
!              INS-999001_FOV_REF_VECTOR       = ( 1.0, 0.0, 0.0 )
!              INS-999001_FOV_REF_ANGLE        = ( 5.0 )
!              INS-999001_FOV_ANGLE_UNITS      = ( 'DEGREES' )
!           \begintext
!
!           The keywords below define an elliptical FOV with 2- and
!           4-degree angular extents in the XZ and XY planes and the
!           boresight along the +X axis of the 'SC999_INST002' frame for
!           an instrument with ID -999002 using the "corners"-class
!           specification.
!
!           \begindata
!              INS-999002_FOV_SHAPE            = 'ELLIPSE'
!              INS-999002_FOV_FRAME            = 'SC999_INST002'
!              INS-999002_BORESIGHT            = ( 1.0, 0.0, 0.0 )
!              INS-999002_FOV_BOUNDARY_CORNERS = (
!                                      1.0,  0.0,        0.01745506,
!                                      1.0,  0.03492077, 0.0        )
!           \begintext
!
!           The keywords below define a rectangular FOV with 1.2- and
!           0.2-degree angular extents in the ZX and ZY planes and the
!           boresight along the +Z axis of the 'SC999_INST003' frame for
!           an instrument with ID -999003 using the "angles"-class
!           specification.
!
!           \begindata
!              INS-999003_FOV_CLASS_SPEC       = 'ANGLES'
!              INS-999003_FOV_SHAPE            = 'RECTANGLE'
!              INS-999003_FOV_FRAME            = 'SC999_INST003'
!              INS-999003_BORESIGHT            = ( 0.0, 0.0, 1.0 )
!              INS-999003_FOV_REF_VECTOR       = ( 1.0, 0.0, 0.0 )
!              INS-999003_FOV_REF_ANGLE        = ( 0.6 )
!              INS-999003_FOV_CROSS_ANGLE      = ( 0.1 )
!              INS-999003_FOV_ANGLE_UNITS      = ( 'DEGREES' )
!           \begintext
!
!           The keywords below define a triangular FOV with the
!           boresight along the +Y axis of the 'SC999_INST004' frame
!           for an instrument with ID -999004 using the "corners"-class
!           specification.
!
!           \begindata
!              INS-999004_FOV_SHAPE            = 'POLYGON'
!              INS-999004_FOV_FRAME            = 'SC999_INST004'
!              INS-999004_BORESIGHT            = (  0.0,  1.0,  0.0 )
!              INS-999004_FOV_BOUNDARY_CORNERS = (  0.0,  0.8,  0.5,
!                                                   0.4,  0.8, -0.2,
!                                                  -0.4,  0.8, -0.2 )
!           \begintext
!
!           End of IK
!
!
!        Example code begins here.
!
!
!              PROGRAM GETFOV_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters
!        C
!              INTEGER               MAXBND
!              PARAMETER           ( MAXBND = 4 )
!
!              INTEGER               NUMINS
!              PARAMETER           ( NUMINS = 4 )
!
!              INTEGER               WDSIZE
!              PARAMETER           ( WDSIZE = 32 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(WDSIZE)    FRAME
!              CHARACTER*(WDSIZE)    SHAPE
!
!              DOUBLE PRECISION      BOUNDS ( 3, MAXBND )
!              DOUBLE PRECISION      BSIGHT ( 3 )
!
!              INTEGER               I
!              INTEGER               INSIDS ( NUMINS )
!              INTEGER               J
!              INTEGER               N
!
!        C
!        C     Define the instrument IDs.
!        C
!              DATA                  INSIDS  /  -999001, -999002,
!             .                                 -999003, -999004  /
!
!        C
!        C     Load the IK file.
!        C
!              CALL FURNSH( 'getfov_ex1.ti' )
!
!        C
!        C     For each instrument ...
!        C
!              WRITE (*,'(A)') '--------------------------------------'
!              DO I = 1, NUMINS
!
!        C
!        C        ... fetch FOV parameters and ...
!        C
!                 CALL GETFOV ( INSIDS(I), MAXBND,
!             .                 SHAPE, FRAME, BSIGHT, N, BOUNDS )
!
!        C
!        C        ... print them to the screen.
!        C
!                 WRITE (*,'(A,I7)')     'Instrument ID: ', INSIDS(I)
!                 WRITE (*,'(2A)')       '    FOV shape: ', SHAPE
!                 WRITE (*,'(2A)')       '    FOV frame: ', frame
!                 WRITE (*,'(A,3F12.8)') 'FOV boresight: ', BSIGHT
!
!                 WRITE (*,'(A)') '  FOV corners: '
!                 DO J = 1, N
!                    WRITE (*,'(A,3F12.8)') '               ',
!             .                  BOUNDS(1,J), BOUNDS(2,J), BOUNDS(3,J)
!                 END DO
!                 WRITE (*,'(A)')
!             .            '--------------------------------------'
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
!        --------------------------------------
!        Instrument ID: -999001
!            FOV shape: CIRCLE
!            FOV frame: SC999_INST001
!        FOV boresight:   0.00000000  0.00000000  1.00000000
!          FOV corners:
!                         0.08715574  0.00000000  0.99619470
!        --------------------------------------
!        Instrument ID: -999002
!            FOV shape: ELLIPSE
!            FOV frame: SC999_INST002
!        FOV boresight:   1.00000000  0.00000000  0.00000000
!          FOV corners:
!                         1.00000000  0.00000000  0.01745506
!                         1.00000000  0.03492077  0.00000000
!        --------------------------------------
!        Instrument ID: -999003
!            FOV shape: RECTANGLE
!            FOV frame: SC999_INST003
!        FOV boresight:   0.00000000  0.00000000  1.00000000
!          FOV corners:
!                         0.01047177  0.00174523  0.99994365
!                        -0.01047177  0.00174523  0.99994365
!                        -0.01047177 -0.00174523  0.99994365
!                         0.01047177 -0.00174523  0.99994365
!        --------------------------------------
!        Instrument ID: -999004
!            FOV shape: POLYGON
!            FOV frame: SC999_INST004
!        FOV boresight:   0.00000000  1.00000000  0.00000000
!          FOV corners:
!                         0.00000000  0.80000000  0.50000000
!                         0.40000000  0.80000000 -0.20000000
!                        -0.40000000  0.80000000 -0.20000000
!        --------------------------------------
!
!
! $ Restrictions
!
!     1)  This routine will not operate unless an I-kernel for the
!         instrument with the NAIF ID specified in INSTID have been
!         loaded via a call to FURNSH prior to calling this routine and
!         this IK contains the specification for the instrument field of
!         view consistent with the expectations of this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     M. Liukis          (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.3.0, 17-DEC-2021 (JDR) (ML) (BVS)
!
!        Bug fix: added missing exception for the boresight vector
!        being the zero vector.
!
!        Updated long error message for 'BADBOUNDARY' exception to
!        correctly describe the check that's actually done.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Updated entry #5 and added entries #14 and #18 to $Exceptions
!        section. Updated $Restrictions section.
!
!        Updated $Particulars to describe the actual condition that
!        reference and cross angles values must satisfy.
!
!        Updated to check FAILED() after calls to CONVRT to prevent
!        use of uninitialized values in subsequent calls.
!
!-    SPICELIB Version 2.2.0, 22-MAR-2017 (JDR) (BVS)
!
!        Header updates: made various header changes to make it
!        compliant with the SPICE standard header format; updated
!        BSIGHT description; added explanation of output boundary
!        vector magnitudes; made other minor header corrections.
!
!        Updated code to remove unnecessary lines in the SPICE
!        error handling IF-THEN-ELSE statements.
!
!-    SPICELIB Version 2.1.1, 05-FEB-2009 (BVS)
!
!        Header updates: added information about required IK keywords;
!        replaced old example with a new one more focused on GETFOV and
!        IK keywords.
!
!-    SPICELIB Version 2.1.0, 23-OCT-2005 (NJB) (BVS)
!
!        Fixed bug causing incorrect computation of the boundary
!        vectors for a rectangular FOV specified using the angular
!        extents method if the reference vector was provided as a
!        non-unit vector and/or was non-perpendicular to the
!        specified boresight.
!
!        Updated to remove non-standard use of duplicate arguments
!        in CONVRT, UNORM, VHAT, VSCL and VCROSS calls.
!
!        Replaced header reference to LDPOOL with reference to FURNSH.
!
!-    SPICELIB Version 2.0.1, 29-JUL-2003 (NJB) (CHA)
!
!        Various header changes were made to improve clarity. Some
!        minor header corrections were made.
!
!-    SPICELIB Version 2.0.0, 15-MAY-2001 (FST)
!
!        Updated the routine to support the new ANGLES specification
!        for RECTANGLE, ELLIPSE, and CIRCLE.
!
!-    SPICELIB Version 1.1.2, 10-MAY-2000 (WLT)
!
!        Removed the unused variable INDEX.
!
!-    SPICELIB Version 1.1.1, 13-APR-2000 (WLT)
!
!        This routine was harvested from the NEAR specific routine
!        of the same name. It was enhanced to support the 'RECTANGLE'
!        shape for a field of view (a special case of 'POLYGON'
!        added for the sake of Cassini users).
!
!-&


! $ Index_Entries
!
!     return instrument's FOV parameters
!
!-&


!
!     SPICELIB functions
!
      double precision      vnorm
      integer               bsrchc
      integer               rtrim
      logical               eqstr
      logical               failed
      logical               return
      logical               vzero

!
!     Local parameters
!

!
!     Keyword Name Length.
!
      integer               kwdnln
      parameter           ( kwdnln = 32 )

!
!     Maximum Number of Normal Vectors.
!
      integer               maxnml
      parameter           ( maxnml =  4 )

!
!     Number of CORNER Shapes Supported.
!
      integer               nshaps
      parameter           ( nshaps =  4 )

!
!     Number of ANGLE Shapes Supported.
!
      integer               numasp
      parameter           ( numasp =  3 )

!
!     Maximum Length of String Data from the kernel pool.
!
      integer               vallen
      parameter           ( vallen = 80 )

!
!     Local variables
!
      character*(kwdnln)    angshp ( numasp )
      character*(vallen)    angunt
      character*(kwdnln)    kwaunt
      character*(kwdnln)    kwbore
      character*(kwdnln)    kwboun
      character*(kwdnln)    kwcang
      character*(kwdnln)    kwfram
      character*(kwdnln)    kword
      character*(kwdnln)    kwrang
      character*(kwdnln)    kwrvec
      character*(kwdnln)    kwshap
      character*(kwdnln)    kwspec
      character*(kwdnln)    shapid ( nshaps )
      character*(vallen)    spec
      character*(1)         type

      double precision      b      ( 3 )
      double precision      b1     ( 3 )
      double precision      b2     ( 3 )
      double precision      bmag
      double precision      coscan
      double precision      cosran
      double precision      crsang
      double precision      normal ( 3, maxnml )
      double precision      refang
      double precision      refvec ( 3 )
      double precision      sincan
      double precision      sinran
      double precision      tmpang
      double precision      tmpvec ( 3 )
      double precision      vmag

      integer               i
      integer               mxcmp

      logical               found

      save                  shapid
      save                  angshp

!
!     Allowed values of shape identifier. Note that these must be
!     supplied in ascending order
!
      data  ( shapid (i), i =  1, nshaps ) &
           / &
           'CIRCLE', &
           'ELLIPSE', &
           'POLYGON', &
           'RECTANGLE' &
           /

!
!     Allowed values of the shape identifier for the ANGLES
!     specification.  Note that these must be supplied in ascending
!     order.
!
      data  ( angshp (i), i =  1, numasp ) &
           / &
           'CIRCLE', &
           'ELLIPSE', &
           'RECTANGLE' &
           /

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'GETFOV' )

      kwboun = 'INS#_FOV_BOUNDARY'
      kwbore = 'INS#_BORESIGHT'
      kwshap = 'INS#_FOV_SHAPE'
      kwfram = 'INS#_FOV_FRAME'
      kwspec = 'INS#_FOV_CLASS_SPEC'
      kwrvec = 'INS#_FOV_REF_VECTOR'
      kwrang = 'INS#_FOV_REF_ANGLE'
      kwcang = 'INS#_FOV_CROSS_ANGLE'
      kwaunt = 'INS#_FOV_ANGLE_UNITS'

      mxcmp  = 3*room

!
!     Look for the frame keyword and get frame name if found,
!     complain if not.
!
      call repmi  ( kwfram, '#',    instid, kword )
      call gcpool ( kword, 1, 1, i, frame,  found )

      if ( .not. found ) then

         call setmsg ( 'The variable, ''#'', specifying the frame ' &
         //            'which instrument # FOV components are ' &
         //            'defined relative to was not found in the ' &
         //            'kernel pool. Check whether IK file for ' &
         //            'the instrument was loaded into the program ' &
         //            'and whether this variable is specified in ' &
         //            'that file.'                                  )
         call errch  ( '#', kword(1:rtrim(kword))                    )
         call errint ( '#', instid                                   )
         call sigerr ( 'SPICE(FRAMEMISSING)'                         )
         call chkout ( 'GETFOV'                                      )
         return

      end if

!
!     Look for the shape keyword and get shape identifier if found,
!     complain if not.
!
      call repmi  ( kwshap, '#', instid, kword )
      call gcpool ( kword, 1, 1, i, shape, found )

      if ( .not. found ) then

         call setmsg ( 'The variable, ''#'', specifying the shape ' &
         //            'of the instrument # FOV was not found in the ' &
         //            'kernel pool. Check whether IK file for ' &
         //            'the instrument was loaded into the program ' &
         //            'and whether this variable is specified in ' &
         //            'that file.' )
         call errch  ( '#', kword(1:rtrim(kword))                      )
         call errint ( '#', instid                                     )
         call sigerr ( 'SPICE(SHAPEMISSING)'                           )
         call chkout ( 'GETFOV'                                        )
         return

      end if

!
!     Create an upper case, left justified value for SHAPE.  This will
!     provide the desired case-insensitivity to the keyword value.
!
      call ucase ( shape, shape )
      call ljust ( shape, shape )

!
!     Check whether shape identified that we got is one from the list
!     of supported, complain if not.
!
      if ( bsrchc( shape(1:rtrim(shape)), nshaps, shapid ) .eq. 0) then

         call setmsg ( 'The FOV shape, ''#'', specified in the ' &
         //            'keyword, ''#'', for the instrument # ' &
         //            'is not supported. See GETFOV subroutine ' &
         //            'header for the list of supported instrument ' &
         //            'FOV shapes.' )
         call errch  ( '#', shape(1:rtrim(shape))                     )
         call errch  ( '#', kword(1:rtrim(kword))                     )
         call errint ( '#', instid                                    )
         call sigerr ( 'SPICE(SHAPENOTSUPPORTED)'                     )
         call chkout ( 'GETFOV'                                       )
         return

      end if

!
!     Look for the boresight keyword and get boresight vector if found,
!     complain if not.
!
      call repmi  ( kwbore, '#',    instid, kword )
      call dtpool ( kword,   found, i,      type )

      if ( .not. found ) then

         call setmsg ( 'The variable, ''#'', specifying the ' &
         //            'boresight of the instrument # was not found ' &
         //            'in the kernel pool. Check whether IK file ' &
         //            'for the instrument was loaded into the ' &
         //            'program and whether this variable is ' &
         //            'specified in that file.'                      )
         call errch  ( '#', kword(1:rtrim(kword))                     )
         call errint ( '#', instid                                    )
         call sigerr ( 'SPICE(BORESIGHTMISSING)'                      )
         call chkout ( 'GETFOV'                                       )
         return

      end if

!
!     Check whether boresight specified by three coordinates;
!     complain if not.
!
      if ( i .ne. 3 ) then

         call setmsg ( 'The number of the boresight vector ' &
         //            'components specified in the ''#'' ' &
         //            'variable is not 3, it is #. Correct it ' &
         //            'in the corresponding IK file to be a ' &
         //            '3-dimensional vector. '                      )
         call errch  ( '#', kword(1:rtrim(kword))                    )
         call errint ( '#', i                                        )
         call sigerr ( 'SPICE(BADBORESIGHTSPEC)'                     )
         call chkout ( 'GETFOV'                                      )
         return

      else if ( type .ne. 'N' ) then

         call setmsg ( 'The boresight vector, stored in the ''#'' ' &
         //            'variable, has not been stored as a vector ' &
         //            'of three numbers.  It has been stored as a ' &
         //            'vector of three strings. '                   )
         call errch  ( '#', kword(1:rtrim(kword))                    )
         call sigerr ( 'SPICE(BADBORESIGHTSPEC)'                     )
         call chkout ( 'GETFOV'                                      )
         return

      end if

      call gdpool ( kword, 1, 3, i, bsight, found )

!
!     Check whether boresight is a zero vector; complain if it is.
!
      if ( vzero(bsight) ) then

         call setmsg ( 'The boresight vector, stored in the ''#'' ' &
         //            'variable, is the zero vector.'               )
         call errch  ( '#', kword(1:rtrim(kword))                    )
         call sigerr ( 'SPICE(ZEROBORESIGHT)'                        )
         call chkout ( 'GETFOV'                                      )
         return

      end if

!
!     At this point we have gotten all the specification independent
!     information.  Now check for the presence of the FOV class
!     specification keyword.  If it's absent, we default to CORNERS.
!
      spec = 'CORNERS'

      call repmi  ( kwspec, '#',    instid, kword )
      call gcpool ( kword, 1, 1, i, spec,   found )

      if ( eqstr('CORNERS', spec) ) then

!
!        Look for the FOV boundary vectors, check whether output array
!        is big enough to hold them; complain if not.
!
         call repmi  ( kwboun, '#', instid, kword )
         call dtpool ( kword, found, n, type )

         if ( .not. found ) then
            call suffix ( '_CORNERS', 0, kword )
            call dtpool ( kword, found, n, type )
         end if

         if ( .not. found ) then

            call repmi  ( kwboun, '#', instid, kword )
            call setmsg ( 'The variable, ''#'', specifying the ' &
            //            'boundary vectors of the instrument # ' &
            //            'FOV was not found in the kernel pool. ' &
            //            'Check whether IK file for the instrument ' &
            //            'was loaded into the program and whether ' &
            //            'this variable is specified in that file.'  )
            call errch  ( '#', kword(1:rtrim(kword))                  )
            call errint ( '#', instid                                 )
            call sigerr ( 'SPICE(BOUNDARYMISSING)'                    )
            call chkout ( 'GETFOV'                                    )
            return

         end if

!
!        Check whether we have enough room to get all boundary vectors,
!        complain if not.
!
         if ( n .gt. mxcmp ) then

            call setmsg ( 'The number of boundary vector components ' &
            //            'specified in the ''#'' pool variable is ' &
            //            'bigger than room to hold them in output ' &
            //            'array specified by the ROOM input ' &
            //            'variable of the GETFOV subroutine.'        )
            call errch  ( '#', kword(1:rtrim(kword))                  )
            call sigerr ( 'SPICE(BOUNDARYTOOBIG)'                     )
            call chkout ( 'GETFOV'                                    )
            return

         end if

!
!        Check whether number of boundary components can be divided by 3
!        without reminder.
!
         if ( mod( n, 3 ) .ne. 0 ) then

            call setmsg ( 'The boundary vector components ' &
            //            'specified in the ''#'' pool variable do  ' &
            //            'not represent a set of 3-dimensional ' &
            //            'vectors. Number of components assigned ' &
            //            'to the variable cannot be divided by 3 ' &
            //            'without reminder. '                        )
            call errch  ( '#', kword(1:rtrim(kword))                  )
            call sigerr ( 'SPICE(BADBOUNDARY)'                        )
            call chkout ( 'GETFOV'                                    )
            return

         end if

!
!        Boundaries are OK. Get them.
!
         call gdpool ( kword, 1, mxcmp, n, bounds, found )

         n = n/3

         if      ( shape .eq. 'CIRCLE'      .and. n .ne. 1 ) then

            call setmsg ( 'The boundary is specified to be ' &
            //            'circular, and as such, the values ' &
            //            'associated with keyword, ''#'', ' &
            //            'should contain one vector.  There ' &
            //            'are #.'                              )
            call errch  ( '#', kword(1:rtrim(kword))            )
            call errint ( '#', n                                )
            call sigerr ( 'SPICE(BADBOUNDARY)'                  )
            call chkout ( 'GETFOV'                              )
            return

         else if ( shape .eq. 'ELLIPSE'     .and. n .ne. 2 ) then

            call setmsg ( 'The boundary is specified to be ' &
            //            'elliptical, and as such, the values ' &
            //            'associated with keyword, ''#'', ' &
            //            'should contain two vectors.  There ' &
            //            'are #.'                               )
            call errch  ( '#', kword(1:rtrim(kword))             )
            call errint ( '#', n                                 )
            call sigerr ( 'SPICE(BADBOUNDARY)'                   )
            call chkout ( 'GETFOV'                               )
            return

         else if ( shape .eq. 'RECTANGLE' .and. n .ne. 4 ) then

            call setmsg ( 'The boundary is specified to be ' &
            //            'rectangular, and as such, the values ' &
            //            'associated with keyword, ''#'', ' &
            //            'should contain four vectors.  There ' &
            //            'are #.'                                )
            call errch  ( '#', kword(1:rtrim(kword))              )
            call errint ( '#', n                                  )
            call sigerr ( 'SPICE(BADBOUNDARY)'                    )
            call chkout ( 'GETFOV'                                )
            return

         else if ( shape .eq. 'POLYGON' .and. n .lt. 3 ) then

            call setmsg ( 'The boundary is specified to be ' &
            //            'polygonal, and as such, the values ' &
            //            'associated with keyword, ''#'', ' &
            //            'should contain at least three ' &
            //            'vectors.  There are #.'              )
            call errch  ( '#', kword(1:rtrim(kword))            )
            call errint ( '#', n                                )
            call sigerr ( 'SPICE(BADBOUNDARY)'                  )
            call chkout ( 'GETFOV'                              )
            return

         end if

!
!     Now check to see if the FOV specification is ANGLES and
!     compute the boundary corner vectors.
!
      else if ( eqstr('ANGLES', spec) ) then

!
!        Check whether shape identified that we got is one from the list
!        of supported shapes for the ANGLE specification; complain
!        if not.
!
         if (bsrchc(shape(1:rtrim(shape)), numasp, angshp) .eq. 0) then

            call setmsg ( 'The FOV shape, ''#'', specified in the ' &
            //            'keyword, ''#'', for the instrument # ' &
            //            'is not supported for the ANGLES ' &
            //            'specification.'                          )
            call errch  ( '#', shape(1:rtrim(shape))                )
            call errch  ( '#', kword(1:rtrim(kword))                )
            call errint ( '#', instid                               )
            call sigerr ( 'SPICE(SHAPENOTSUPPORTED)'                )
            call chkout ( 'GETFOV'                                  )
            return

         end if

!
!        Now fetch all of the elements independent of shape from the
!        ANGLES specification.  Start by looking for the reference
!        vector keyword.  If found, fetch it otherwise complain.
!
         call repmi  ( kwrvec, '#',    instid, kword )
         call dtpool ( kword,   found, i,      type )

         if ( .not. found ) then

            call setmsg ( 'The variable, ''#'', specifying the ' &
            //            'FOV reference vector of the instrument ' &
            //            '# was not found in the kernel pool. ' &
            //            'Check whether IK file for the instrument ' &
            //            'was loaded into the program and whether ' &
            //            'this variable is specified in that file.'  )
            call errch  ( '#', kword(1:rtrim(kword))                  )
            call errint ( '#', instid                                 )
            call sigerr ( 'SPICE(REFVECTORMISSING)'                   )
            call chkout ( 'GETFOV'                                    )
            return

         end if

!
!        Now check whether reference vector is specified by three
!        coordinates; complain if not.
!
         if ( i .ne. 3 ) then

            call setmsg ( 'The number of the reference vector ' &
            //            'components specified in the ''#'' ' &
            //            'keyword is not 3, it is #. Check the ' &
            //            'corresponding IK FOV definition for ' &
            //            'errors.'                               )
            call errch  ( '#', kword(1:rtrim(kword))              )
            call errint ( '#', i                                  )
            call sigerr ( 'SPICE(BADREFVECTORSPEC)'               )
            call chkout ( 'GETFOV'                                )
            return

         else if ( type .ne. 'N' ) then

            call setmsg ( 'The reference vector, stored in ''#'', ' &
            //            'has not been stored as a vector of ' &
            //            'three numbers.  It has been stored as ' &
            //            'a vector of three strings. '             )
            call errch  ( '#', kword(1:rtrim(kword))                )
            call sigerr ( 'SPICE(BADREFVECTORSPEC)'                 )
            call chkout ( 'GETFOV'                                  )
            return

         end if

         call gdpool ( kword, 1, 3, i, refvec, found )

!
!        We require that the reference vector is not parallel
!        to the boresight vector. Use NORMAL(1,1) to temporarily
!        store the result of the cross product.
!
         call vcrss ( bsight, refvec, normal(1,1) )

         if ( vnorm ( normal(1,1) ) .eq. 0.0d0 ) then

            call setmsg ( 'The reference vector, stored in ''#'', ' &
            //            'is parallel to the instrument boresight ' &
            //            'vector.  This is not allowed by the ' &
            //            'ANGLES FOV specification.'                )
            call errch  ( '#', kword(1:rtrim(kword))                 )
            call sigerr ( 'SPICE(BADREFVECTORSPEC)'                  )
            call chkout ( 'GETFOV'                                   )
            return

         end if

!
!        Retrieve the reference angle from the kernel pool.
!
         call repmi  ( kwrang, '#',    instid, kword )
         call gdpool ( kword, 1, 1, i, refang, found )

         if ( .not. found ) then

            call setmsg ( 'The variable, ''#'', specifying the ' &
            //            'reference angle which describes ' &
            //            'instrument # FOV angular extent was ' &
            //            'not found in the kernel pool. Check ' &
            //            'whether IK file for the instrument ' &
            //            'was loaded into the program and ' &
            //            'whether this variable is specified ' &
            //            'in that file.'                        )
            call errch  ( '#', kword(1:rtrim(kword))             )
            call errint ( '#', instid                            )
            call sigerr ( 'SPICE(REFANGLEMISSING)'               )
            call chkout ( 'GETFOV'                               )
            return

         end if

!
!        Retrieve the angle units from the kernel pool.
!
         call repmi  ( kwaunt, '#',    instid, kword )
         call gcpool ( kword, 1, 1, i, angunt, found )

         if ( .not. found ) then

            call setmsg ( 'The variable, ''#'', specifying the ' &
            //            'angular units in which instrument # ' &
            //            'FOV extent is defined was not found ' &
            //            'in the kernel pool. Check whether ' &
            //            'IK file for the instrument was ' &
            //            'loaded into the program and whether ' &
            //            'this variable is specified in that ' &
            //            'file.'                                )
            call errch  ( '#', kword(1:rtrim(kword))             )
            call errint ( '#', instid                            )
            call sigerr ( 'SPICE(UNITSMISSING)'                  )
            call chkout ( 'GETFOV'                               )
            return

         end if

!
!        Convert the reference angle to radians.
!
         call convrt ( refang, angunt, 'RADIANS', tmpang )

         if ( failed() ) then
            call chkout ( 'GETFOV' )
            return
         end if

         refang = tmpang

!
!        Branch to shape specific code.
!
         if ( shape .eq. 'CIRCLE' ) then

!
!           First check to see that the caller left enough room
!           to store the required number of boundary corner
!           vectors.
!
            if ( room .lt. 1 ) then
               call setmsg ( 'The FOV shape for instrument # ' &
               //            'is specified to be circular.  ' &
               //            'There should be room for at ' &
               //            'least one boundary vector.  There ' &
               //            'is room for #. '                    )
               call errint ( '#', instid                          )
               call errint ( '#', room                            )
               call sigerr ( 'SPICE(BOUNDARYTOOBIG)'              )
               call chkout ( 'GETFOV'                             )
               return
            end if

!
!           The plan to compute the boundary corner vector is to
!           rotate the BSIGHT by REFANG towards REFVEC.  To do
!           this first compute the axis we need to rotate about.
!
            call vcrss ( bsight, refvec, normal(1,1) )

!
!           Now rotate by REFANG about NORMAL(1,1) using the routine
!           VROTV.
!
            call vrotv ( bsight, normal(1,1), refang, bounds(1,1) )

!
!           Lastly, since we computed a single boundary corner vector,
!           set N = 1.
!
            n = 1

         else if ( shape .eq. 'ELLIPSE' ) then

!
!           The elliptical case requires the additional cross angle
!           keyword's presence in the kernel pool. Attempt to
!           retrieve it.
!
            call repmi  ( kwcang, '#',    instid, kword )
            call gdpool ( kword, 1, 1, i, crsang, found )

            if ( .not. found ) then

               call setmsg ( 'The variable, ''#'', specifying the ' &
               //            'cross angle which describes instrument ' &
               //            '# FOV angular extent was not found in ' &
               //            'the kernel pool. Check whether IK file ' &
               //            'for the instrument was loaded into the ' &
               //            'program and whether this variable is ' &
               //            'specified in that file.'                 )
               call errch  ( '#', kword(1:rtrim(kword))                )
               call errint ( '#', instid                               )
               call sigerr ( 'SPICE(CROSSANGLEMISSING)'                )
               call chkout ( 'GETFOV'                                  )
               return

            end if

!
!           Convert the cross angle to radians.
!
            call convrt ( crsang, angunt, 'RADIANS', tmpang )

            if ( failed() ) then
               call chkout ( 'GETFOV' )
               return
            end if

            crsang = tmpang
!
!           Now check to see that the caller left enough room
!           to store the required number of boundary corner
!           vectors.
!
            if ( room .lt. 2 ) then
               call setmsg ( 'The FOV shape for instrument # ' &
               //            'is specified to be elliptical.  ' &
               //            'There should be room for at ' &
               //            'least two boundary vectors.  There ' &
               //            'is room for #. '                     )
               call errint ( '#', instid                           )
               call errint ( '#', room                             )
               call sigerr ( 'SPICE(BOUNDARYTOOBIG)'               )
               call chkout ( 'GETFOV'                              )
               return
            end if

!
!           The plan to compute the first boundary corner vector is
!           to rotate the BSIGHT by REFANG towards REFVEC.  To
!           do this first compute the axis we need to rotate about.
!
            call vcrss ( bsight, refvec, normal(1,1) )

!
!           Now rotate by REFANG about NORMAL(1,1) using the routine
!           VROTV.
!
            call vrotv ( bsight, normal(1,1), refang, bounds(1,1) )

!
!           At this point we have one boundary vector.  We need the
!           second and final one.  The strategy we will use is the
!           following: rotate BSIGHT by CRSANG towards NORMAL(1,1).
!           This will give us boundary corner vectors listed in a
!           counter-clockwise fashion about the boresight.
!
            call vcrss ( bsight, normal(1,1), tmpvec      )
            call vequ  ( tmpvec,              normal(1,2) )
!
!           Now rotate BSIGHT by CRSANG about the NORMAL(1,2) using
!           the routine VROTV.
!
            call vrotv ( bsight, normal(1,2), crsang, bounds(1,2) )

!
!           Lastly, since we computed two boundary corner vectors,
!           set N = 2.
!
            n = 2

         else if ( shape .eq. 'RECTANGLE' ) then

!
!           The rectangular case requires the additional cross angle
!           keyword's presence in the kernel pool. Attempt to
!           retrieve it.
!
            call repmi  ( kwcang, '#',    instid, kword )
            call gdpool ( kword, 1, 1, i, crsang, found )

            if ( .not. found ) then

               call setmsg ( 'The variable, ''#'', specifying the ' &
               //            'cross angle which describes instrument ' &
               //            '# FOV angular extent was not found in ' &
               //            'the kernel pool. Check whether IK file ' &
               //            'for the instrument was loaded into the ' &
               //            'program and whether this variable is ' &
               //            'specified in that file.'                 )
               call errch  ( '#', kword(1:rtrim(kword))                )
               call errint ( '#', instid                               )
               call sigerr ( 'SPICE(CROSSANGLEMISSING)'                )
               call chkout ( 'GETFOV'                                  )
               return

            end if

!
!           Convert the cross angle to radians.
!
            call convrt ( crsang, angunt, 'RADIANS', tmpang )

            if ( failed() ) then
               call chkout ( 'GETFOV' )
               return
            end if

            crsang = tmpang
!
!           Now check to see that the caller left enough room
!           to store the required number of boundary corner
!           vectors.
!
            if ( room .lt. 4 ) then
               call setmsg ( 'The FOV shape for instrument # ' &
               //            'is specified to be rectangular.  ' &
               //            'There should be room for at ' &
               //            'least four boundary vectors.  There ' &
               //            'is room for #. '                      )
               call errint ( '#', instid                            )
               call errint ( '#', room                              )
               call sigerr ( 'SPICE(BOUNDARYTOOBIG)'                )
               call chkout ( 'GETFOV'                               )
               return
            end if

!
!           Here's the general strategy laid out in simple terms:
!
!           (1) Normalize BSIGHT, label it B.
!
!           (2) Compute the unit vector in the plane defined by REFVEC
!               and B that is normal to B and pointing towards
!               REFVEC, label this B1.
!
!           (3) Cross B and B1 to obtain B2. These three vectors
!               form a basis that is 'aligned' with the FOV cone.
!
!           (4) Compute the inward normals to the sides of the
!               rectangular cone in a counter-clockwise order
!               about the boresight:
!
!                 NORMAL(1) = -COS(REFANG)*B1 + SIN(REFANG)*B
!                 NORMAL(2) = -COS(CRSANG)*B2 + SIN(CRSANG)*B
!                 NORMAL(3) =  COS(REFANG)*B1 + SIN(REFANG)*B
!                 NORMAL(4) =  COS(CRSANG)*B2 + SIN(CRSANG)*B
!
!           (5) Compute the appropriate cross products to obtain
!               a set of boundary corner vectors:
!
!                 BOUNDS(1) = NORMAL(1) x NORMAL(2)
!                 BOUNDS(2) = NORMAL(2) x NORMAL(3)
!                 BOUNDS(3) = NORMAL(3) x NORMAL(4)
!                 BOUNDS(4) = NORMAL(4) x NORMAL(1)
!
!           (6) Unitize and scale BOUNDS to match the length
!               of the BSIGHT.
!
!           Start with step (1).
!
            call unorm ( bsight, b, bmag )

!
!           Now proceed to (2). Since we already know that REFVEC
!           and BSIGHT are not parallel, the following yields a
!           non-zero vector:
!
            call vperp ( refvec, bsight, b1 )

!
!           Unitize B1.
!
            call vhat  ( b1,     tmpvec )
            call vequ  ( tmpvec, b1     )

!
!           Step (3), compute B2 by crossing B and B1.
!
            call vcrss ( b, b1, b2 )

!
!           Before proceeding onto step (4), verify that the
!           results of the calculations in step (4) will make
!           sense.  Check the cosines of CRSANG and REFANG.
!           Signal an error if both are not positive numbers.
!           Use MINCOS as a tolerance.
!
            cosran = dcos ( refang )
            coscan = dcos ( crsang )

            if ( ( cosran .lt. mincos ) .or. &
                 ( coscan .lt. mincos )      ) then

               call setmsg ( 'The angular extents specified ' &
               //            'in the FOV definition for ' &
               //            'instrument # result in degenerate ' &
               //            'or improper boundary corner ' &
               //            'vectors. This usually happens ' &
               //            'when one (or both) of the angles ' &
               //            'results in the angular separation ' &
               //            'between the boresight and the FOV ' &
               //            'side plane that it defines being ' &
               //            'equal to or greater than 90 ' &
               //            'degrees.'                            )
               call errint ( '#', instid                           )
               call sigerr ( 'SPICE(BADBOUNDARY)'                  )
               call chkout ( 'GETFOV'                              )
               return

            end if

!
!           Compute the NORMAL vectors to complete step (4).
!
            sinran = dsin ( refang )
            sincan = dsin ( crsang )

            call vlcom ( -cosran, b1, sinran, b, normal(1,1) )
            call vlcom ( -coscan, b2, sincan, b, normal(1,2) )
            call vlcom (  cosran, b1, sinran, b, normal(1,3) )
            call vlcom (  coscan, b2, sincan, b, normal(1,4) )

!
!           We are almost finished. Compute the boundary corner
!           vectors completing step (5).
!
            call vcrss ( normal(1,1), normal(1,2), bounds(1,1) )
            call vcrss ( normal(1,2), normal(1,3), bounds(1,2) )
            call vcrss ( normal(1,3), normal(1,4), bounds(1,3) )
            call vcrss ( normal(1,4), normal(1,1), bounds(1,4) )

!
!           Step (6), normalize the boundary corner vectors
!           and scale by BMAG, the magnitude of BSIGHT.
!
            do i = 1, 4

               call unorm ( bounds(1,i),  tmpvec,  vmag        )
               call vscl  ( bmag,         tmpvec,  bounds(1,i) )

            end do

!
!           Lastly since we are returning 4 boundary corner vectors,
!           set N = 4.
!
            n = 4

         else

!
!           If we end up here something is terribly wrong with
!           this module or SPICE in general.
!
            call setmsg ( 'This error is never supposed to ' &
            //            'occur. We have an undefined shape ' &
            //            'for the ANGLES specification that ' &
            //            'passed the shape check.'            )
            call sigerr ( 'SPICE(BUG)'                         )
            call chkout ( 'GETFOV'                             )
            return

         end if

      else

         call setmsg ( 'The FOV class specification is set to ''#'' ' &
         //            'which is currently unsupported. See the ' &
         //            'GETFOV subroutine header for more ' &
         //            'information.'                                 )
         call errch  ( '#', spec                                      )
         call sigerr ( 'SPICE(UNSUPPORTEDSPEC)'                       )
         call chkout ( 'GETFOV'                                       )
         return

      end if

!
!     Standard SPICE error handling.
!
      call chkout ( 'GETFOV' )
      return

      end
