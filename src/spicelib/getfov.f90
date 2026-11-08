!$Procedure GETFOV ( Get instrument FOV parameters )

      SUBROUTINE GETFOV ( INSTID, &
                          ROOM, &
                          SHAPE, &
                          FRAME, &
                          BSIGHT, &
                          N, &
                          BOUNDS  )

! $ Abstract
!
!     Return the field-of-view (FOV) parameters for a specified
!     instrument. The instrument is specified by its NAIF ID code.
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
!     NAIF_IDS
!
! $ Keywords
!
!     FOV
!     INSTRUMENT
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      MINCOS
      PARAMETER           ( MINCOS = 1.0D-15 )

      INTEGER               INSTID
      INTEGER               ROOM
      CHARACTER*(*)         SHAPE
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      BSIGHT ( 3    )
      INTEGER               N
      DOUBLE PRECISION      BOUNDS ( 3, * )

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
      DOUBLE PRECISION      VNORM
      INTEGER               BSRCHC
      INTEGER               RTRIM
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!

!
!     Keyword Name Length.
!
      INTEGER               KWDNLN
      PARAMETER           ( KWDNLN = 32 )

!
!     Maximum Number of Normal Vectors.
!
      INTEGER               MAXNML
      PARAMETER           ( MAXNML =  4 )

!
!     Number of CORNER Shapes Supported.
!
      INTEGER               NSHAPS
      PARAMETER           ( NSHAPS =  4 )

!
!     Number of ANGLE Shapes Supported.
!
      INTEGER               NUMASP
      PARAMETER           ( NUMASP =  3 )

!
!     Maximum Length of String Data from the kernel pool.
!
      INTEGER               VALLEN
      PARAMETER           ( VALLEN = 80 )

!
!     Local variables
!
      CHARACTER*(KWDNLN)    ANGSHP ( NUMASP )
      CHARACTER*(VALLEN)    ANGUNT
      CHARACTER*(KWDNLN)    KWAUNT
      CHARACTER*(KWDNLN)    KWBORE
      CHARACTER*(KWDNLN)    KWBOUN
      CHARACTER*(KWDNLN)    KWCANG
      CHARACTER*(KWDNLN)    KWFRAM
      CHARACTER*(KWDNLN)    KWORD
      CHARACTER*(KWDNLN)    KWRANG
      CHARACTER*(KWDNLN)    KWRVEC
      CHARACTER*(KWDNLN)    KWSHAP
      CHARACTER*(KWDNLN)    KWSPEC
      CHARACTER*(KWDNLN)    SHAPID ( NSHAPS )
      CHARACTER*(VALLEN)    SPEC
      CHARACTER*(1)         TYPE

      DOUBLE PRECISION      B      ( 3 )
      DOUBLE PRECISION      B1     ( 3 )
      DOUBLE PRECISION      B2     ( 3 )
      DOUBLE PRECISION      BMAG
      DOUBLE PRECISION      COSCAN
      DOUBLE PRECISION      COSRAN
      DOUBLE PRECISION      CRSANG
      DOUBLE PRECISION      NORMAL ( 3, MAXNML )
      DOUBLE PRECISION      REFANG
      DOUBLE PRECISION      REFVEC ( 3 )
      DOUBLE PRECISION      SINCAN
      DOUBLE PRECISION      SINRAN
      DOUBLE PRECISION      TMPANG
      DOUBLE PRECISION      TMPVEC ( 3 )
      DOUBLE PRECISION      VMAG

      INTEGER               I
      INTEGER               MXCMP

      LOGICAL               FOUND

      SAVE                  SHAPID
      SAVE                  ANGSHP

!
!     Allowed values of shape identifier. Note that these must be
!     supplied in ascending order
!
      DATA  ( SHAPID (I), I =  1, NSHAPS ) &
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
      DATA  ( ANGSHP (I), I =  1, NUMASP ) &
           / &
           'CIRCLE', &
           'ELLIPSE', &
           'RECTANGLE' &
           /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'GETFOV' )

      KWBOUN = 'INS#_FOV_BOUNDARY'
      KWBORE = 'INS#_BORESIGHT'
      KWSHAP = 'INS#_FOV_SHAPE'
      KWFRAM = 'INS#_FOV_FRAME'
      KWSPEC = 'INS#_FOV_CLASS_SPEC'
      KWRVEC = 'INS#_FOV_REF_VECTOR'
      KWRANG = 'INS#_FOV_REF_ANGLE'
      KWCANG = 'INS#_FOV_CROSS_ANGLE'
      KWAUNT = 'INS#_FOV_ANGLE_UNITS'

      MXCMP  = 3*ROOM

!
!     Look for the frame keyword and get frame name if found,
!     complain if not.
!
      CALL REPMI  ( KWFRAM, '#',    INSTID, KWORD )
      CALL GCPOOL ( KWORD, 1, 1, I, FRAME,  FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The variable, ''#'', specifying the frame ' &
         //            'which instrument # FOV components are ' &
         //            'defined relative to was not found in the ' &
         //            'kernel pool. Check whether IK file for ' &
         //            'the instrument was loaded into the program ' &
         //            'and whether this variable is specified in ' &
         //            'that file.'                                  )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                    )
         CALL ERRINT ( '#', INSTID                                   )
         CALL SIGERR ( 'SPICE(FRAMEMISSING)'                         )
         CALL CHKOUT ( 'GETFOV'                                      )
         RETURN

      END IF

!
!     Look for the shape keyword and get shape identifier if found,
!     complain if not.
!
      CALL REPMI  ( KWSHAP, '#', INSTID, KWORD )
      CALL GCPOOL ( KWORD, 1, 1, I, SHAPE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The variable, ''#'', specifying the shape ' &
         //            'of the instrument # FOV was not found in the ' &
         //            'kernel pool. Check whether IK file for ' &
         //            'the instrument was loaded into the program ' &
         //            'and whether this variable is specified in ' &
         //            'that file.' )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                      )
         CALL ERRINT ( '#', INSTID                                     )
         CALL SIGERR ( 'SPICE(SHAPEMISSING)'                           )
         CALL CHKOUT ( 'GETFOV'                                        )
         RETURN

      END IF

!
!     Create an upper case, left justified value for SHAPE.  This will
!     provide the desired case-insensitivity to the keyword value.
!
      CALL UCASE ( SHAPE, SHAPE )
      CALL LJUST ( SHAPE, SHAPE )

!
!     Check whether shape identified that we got is one from the list
!     of supported, complain if not.
!
      IF ( BSRCHC( SHAPE(1:RTRIM(SHAPE)), NSHAPS, SHAPID ) .EQ. 0) THEN

         CALL SETMSG ( 'The FOV shape, ''#'', specified in the ' &
         //            'keyword, ''#'', for the instrument # ' &
         //            'is not supported. See GETFOV subroutine ' &
         //            'header for the list of supported instrument ' &
         //            'FOV shapes.' )
         CALL ERRCH  ( '#', SHAPE(1:RTRIM(SHAPE))                     )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                     )
         CALL ERRINT ( '#', INSTID                                    )
         CALL SIGERR ( 'SPICE(SHAPENOTSUPPORTED)'                     )
         CALL CHKOUT ( 'GETFOV'                                       )
         RETURN

      END IF

!
!     Look for the boresight keyword and get boresight vector if found,
!     complain if not.
!
      CALL REPMI  ( KWBORE, '#',    INSTID, KWORD )
      CALL DTPOOL ( KWORD,   FOUND, I,      TYPE )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The variable, ''#'', specifying the ' &
         //            'boresight of the instrument # was not found ' &
         //            'in the kernel pool. Check whether IK file ' &
         //            'for the instrument was loaded into the ' &
         //            'program and whether this variable is ' &
         //            'specified in that file.'                      )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                     )
         CALL ERRINT ( '#', INSTID                                    )
         CALL SIGERR ( 'SPICE(BORESIGHTMISSING)'                      )
         CALL CHKOUT ( 'GETFOV'                                       )
         RETURN

      END IF

!
!     Check whether boresight specified by three coordinates;
!     complain if not.
!
      IF ( I .NE. 3 ) THEN

         CALL SETMSG ( 'The number of the boresight vector ' &
         //            'components specified in the ''#'' ' &
         //            'variable is not 3, it is #. Correct it ' &
         //            'in the corresponding IK file to be a ' &
         //            '3-dimensional vector. '                      )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                    )
         CALL ERRINT ( '#', I                                        )
         CALL SIGERR ( 'SPICE(BADBORESIGHTSPEC)'                     )
         CALL CHKOUT ( 'GETFOV'                                      )
         RETURN

      ELSE IF ( TYPE .NE. 'N' ) THEN

         CALL SETMSG ( 'The boresight vector, stored in the ''#'' ' &
         //            'variable, has not been stored as a vector ' &
         //            'of three numbers.  It has been stored as a ' &
         //            'vector of three strings. '                   )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                    )
         CALL SIGERR ( 'SPICE(BADBORESIGHTSPEC)'                     )
         CALL CHKOUT ( 'GETFOV'                                      )
         RETURN

      END IF

      CALL GDPOOL ( KWORD, 1, 3, I, BSIGHT, FOUND )

!
!     Check whether boresight is a zero vector; complain if it is.
!
      IF ( VZERO(BSIGHT) ) THEN

         CALL SETMSG ( 'The boresight vector, stored in the ''#'' ' &
         //            'variable, is the zero vector.'               )
         CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                    )
         CALL SIGERR ( 'SPICE(ZEROBORESIGHT)'                        )
         CALL CHKOUT ( 'GETFOV'                                      )
         RETURN

      END IF

!
!     At this point we have gotten all the specification independent
!     information.  Now check for the presence of the FOV class
!     specification keyword.  If it's absent, we default to CORNERS.
!
      SPEC = 'CORNERS'

      CALL REPMI  ( KWSPEC, '#',    INSTID, KWORD )
      CALL GCPOOL ( KWORD, 1, 1, I, SPEC,   FOUND )

      IF ( EQSTR('CORNERS', SPEC) ) THEN

!
!        Look for the FOV boundary vectors, check whether output array
!        is big enough to hold them; complain if not.
!
         CALL REPMI  ( KWBOUN, '#', INSTID, KWORD )
         CALL DTPOOL ( KWORD, FOUND, N, TYPE )

         IF ( .NOT. FOUND ) THEN
            CALL SUFFIX ( '_CORNERS', 0, KWORD )
            CALL DTPOOL ( KWORD, FOUND, N, TYPE )
         END IF

         IF ( .NOT. FOUND ) THEN

            CALL REPMI  ( KWBOUN, '#', INSTID, KWORD )
            CALL SETMSG ( 'The variable, ''#'', specifying the ' &
            //            'boundary vectors of the instrument # ' &
            //            'FOV was not found in the kernel pool. ' &
            //            'Check whether IK file for the instrument ' &
            //            'was loaded into the program and whether ' &
            //            'this variable is specified in that file.'  )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                  )
            CALL ERRINT ( '#', INSTID                                 )
            CALL SIGERR ( 'SPICE(BOUNDARYMISSING)'                    )
            CALL CHKOUT ( 'GETFOV'                                    )
            RETURN

         END IF

!
!        Check whether we have enough room to get all boundary vectors,
!        complain if not.
!
         IF ( N .GT. MXCMP ) THEN

            CALL SETMSG ( 'The number of boundary vector components ' &
            //            'specified in the ''#'' pool variable is ' &
            //            'bigger than room to hold them in output ' &
            //            'array specified by the ROOM input ' &
            //            'variable of the GETFOV subroutine.'        )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                  )
            CALL SIGERR ( 'SPICE(BOUNDARYTOOBIG)'                     )
            CALL CHKOUT ( 'GETFOV'                                    )
            RETURN

         END IF

!
!        Check whether number of boundary components can be divided by 3
!        without reminder.
!
         IF ( MOD( N, 3 ) .NE. 0 ) THEN

            CALL SETMSG ( 'The boundary vector components ' &
            //            'specified in the ''#'' pool variable do  ' &
            //            'not represent a set of 3-dimensional ' &
            //            'vectors. Number of components assigned ' &
            //            'to the variable cannot be divided by 3 ' &
            //            'without reminder. '                        )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                  )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                        )
            CALL CHKOUT ( 'GETFOV'                                    )
            RETURN

         END IF

!
!        Boundaries are OK. Get them.
!
         CALL GDPOOL ( KWORD, 1, MXCMP, N, BOUNDS, FOUND )

         N = N/3

         IF      ( SHAPE .EQ. 'CIRCLE'      .AND. N .NE. 1 ) THEN

            CALL SETMSG ( 'The boundary is specified to be ' &
            //            'circular, and as such, the values ' &
            //            'associated with keyword, ''#'', ' &
            //            'should contain one vector.  There ' &
            //            'are #.'                              )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))            )
            CALL ERRINT ( '#', N                                )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                  )
            CALL CHKOUT ( 'GETFOV'                              )
            RETURN

         ELSE IF ( SHAPE .EQ. 'ELLIPSE'     .AND. N .NE. 2 ) THEN

            CALL SETMSG ( 'The boundary is specified to be ' &
            //            'elliptical, and as such, the values ' &
            //            'associated with keyword, ''#'', ' &
            //            'should contain two vectors.  There ' &
            //            'are #.'                               )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))             )
            CALL ERRINT ( '#', N                                 )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                   )
            CALL CHKOUT ( 'GETFOV'                               )
            RETURN

         ELSE IF ( SHAPE .EQ. 'RECTANGLE' .AND. N .NE. 4 ) THEN

            CALL SETMSG ( 'The boundary is specified to be ' &
            //            'rectangular, and as such, the values ' &
            //            'associated with keyword, ''#'', ' &
            //            'should contain four vectors.  There ' &
            //            'are #.'                                )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))              )
            CALL ERRINT ( '#', N                                  )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                    )
            CALL CHKOUT ( 'GETFOV'                                )
            RETURN

         ELSE IF ( SHAPE .EQ. 'POLYGON' .AND. N .LT. 3 ) THEN

            CALL SETMSG ( 'The boundary is specified to be ' &
            //            'polygonal, and as such, the values ' &
            //            'associated with keyword, ''#'', ' &
            //            'should contain at least three ' &
            //            'vectors.  There are #.'              )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))            )
            CALL ERRINT ( '#', N                                )
            CALL SIGERR ( 'SPICE(BADBOUNDARY)'                  )
            CALL CHKOUT ( 'GETFOV'                              )
            RETURN

         END IF

!
!     Now check to see if the FOV specification is ANGLES and
!     compute the boundary corner vectors.
!
      ELSE IF ( EQSTR('ANGLES', SPEC) ) THEN

!
!        Check whether shape identified that we got is one from the list
!        of supported shapes for the ANGLE specification; complain
!        if not.
!
         IF (BSRCHC(SHAPE(1:RTRIM(SHAPE)), NUMASP, ANGSHP) .EQ. 0) THEN

            CALL SETMSG ( 'The FOV shape, ''#'', specified in the ' &
            //            'keyword, ''#'', for the instrument # ' &
            //            'is not supported for the ANGLES ' &
            //            'specification.'                          )
            CALL ERRCH  ( '#', SHAPE(1:RTRIM(SHAPE))                )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
            CALL ERRINT ( '#', INSTID                               )
            CALL SIGERR ( 'SPICE(SHAPENOTSUPPORTED)'                )
            CALL CHKOUT ( 'GETFOV'                                  )
            RETURN

         END IF

!
!        Now fetch all of the elements independent of shape from the
!        ANGLES specification.  Start by looking for the reference
!        vector keyword.  If found, fetch it otherwise complain.
!
         CALL REPMI  ( KWRVEC, '#',    INSTID, KWORD )
         CALL DTPOOL ( KWORD,   FOUND, I,      TYPE )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'The variable, ''#'', specifying the ' &
            //            'FOV reference vector of the instrument ' &
            //            '# was not found in the kernel pool. ' &
            //            'Check whether IK file for the instrument ' &
            //            'was loaded into the program and whether ' &
            //            'this variable is specified in that file.'  )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                  )
            CALL ERRINT ( '#', INSTID                                 )
            CALL SIGERR ( 'SPICE(REFVECTORMISSING)'                   )
            CALL CHKOUT ( 'GETFOV'                                    )
            RETURN

         END IF

!
!        Now check whether reference vector is specified by three
!        coordinates; complain if not.
!
         IF ( I .NE. 3 ) THEN

            CALL SETMSG ( 'The number of the reference vector ' &
            //            'components specified in the ''#'' ' &
            //            'keyword is not 3, it is #. Check the ' &
            //            'corresponding IK FOV definition for ' &
            //            'errors.'                               )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))              )
            CALL ERRINT ( '#', I                                  )
            CALL SIGERR ( 'SPICE(BADREFVECTORSPEC)'               )
            CALL CHKOUT ( 'GETFOV'                                )
            RETURN

         ELSE IF ( TYPE .NE. 'N' ) THEN

            CALL SETMSG ( 'The reference vector, stored in ''#'', ' &
            //            'has not been stored as a vector of ' &
            //            'three numbers.  It has been stored as ' &
            //            'a vector of three strings. '             )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
            CALL SIGERR ( 'SPICE(BADREFVECTORSPEC)'                 )
            CALL CHKOUT ( 'GETFOV'                                  )
            RETURN

         END IF

         CALL GDPOOL ( KWORD, 1, 3, I, REFVEC, FOUND )

!
!        We require that the reference vector is not parallel
!        to the boresight vector. Use NORMAL(1,1) to temporarily
!        store the result of the cross product.
!
         CALL VCRSS ( BSIGHT, REFVEC, NORMAL(1,1) )

         IF ( VNORM ( NORMAL(1,1) ) .EQ. 0.0D0 ) THEN

            CALL SETMSG ( 'The reference vector, stored in ''#'', ' &
            //            'is parallel to the instrument boresight ' &
            //            'vector.  This is not allowed by the ' &
            //            'ANGLES FOV specification.'                )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                 )
            CALL SIGERR ( 'SPICE(BADREFVECTORSPEC)'                  )
            CALL CHKOUT ( 'GETFOV'                                   )
            RETURN

         END IF

!
!        Retrieve the reference angle from the kernel pool.
!
         CALL REPMI  ( KWRANG, '#',    INSTID, KWORD )
         CALL GDPOOL ( KWORD, 1, 1, I, REFANG, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'The variable, ''#'', specifying the ' &
            //            'reference angle which describes ' &
            //            'instrument # FOV angular extent was ' &
            //            'not found in the kernel pool. Check ' &
            //            'whether IK file for the instrument ' &
            //            'was loaded into the program and ' &
            //            'whether this variable is specified ' &
            //            'in that file.'                        )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))             )
            CALL ERRINT ( '#', INSTID                            )
            CALL SIGERR ( 'SPICE(REFANGLEMISSING)'               )
            CALL CHKOUT ( 'GETFOV'                               )
            RETURN

         END IF

!
!        Retrieve the angle units from the kernel pool.
!
         CALL REPMI  ( KWAUNT, '#',    INSTID, KWORD )
         CALL GCPOOL ( KWORD, 1, 1, I, ANGUNT, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'The variable, ''#'', specifying the ' &
            //            'angular units in which instrument # ' &
            //            'FOV extent is defined was not found ' &
            //            'in the kernel pool. Check whether ' &
            //            'IK file for the instrument was ' &
            //            'loaded into the program and whether ' &
            //            'this variable is specified in that ' &
            //            'file.'                                )
            CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))             )
            CALL ERRINT ( '#', INSTID                            )
            CALL SIGERR ( 'SPICE(UNITSMISSING)'                  )
            CALL CHKOUT ( 'GETFOV'                               )
            RETURN

         END IF

!
!        Convert the reference angle to radians.
!
         CALL CONVRT ( REFANG, ANGUNT, 'RADIANS', TMPANG )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'GETFOV' )
            RETURN
         END IF

         REFANG = TMPANG

!
!        Branch to shape specific code.
!
         IF ( SHAPE .EQ. 'CIRCLE' ) THEN

!
!           First check to see that the caller left enough room
!           to store the required number of boundary corner
!           vectors.
!
            IF ( ROOM .LT. 1 ) THEN
               CALL SETMSG ( 'The FOV shape for instrument # ' &
               //            'is specified to be circular.  ' &
               //            'There should be room for at ' &
               //            'least one boundary vector.  There ' &
               //            'is room for #. '                    )
               CALL ERRINT ( '#', INSTID                          )
               CALL ERRINT ( '#', ROOM                            )
               CALL SIGERR ( 'SPICE(BOUNDARYTOOBIG)'              )
               CALL CHKOUT ( 'GETFOV'                             )
               RETURN
            END IF

!
!           The plan to compute the boundary corner vector is to
!           rotate the BSIGHT by REFANG towards REFVEC.  To do
!           this first compute the axis we need to rotate about.
!
            CALL VCRSS ( BSIGHT, REFVEC, NORMAL(1,1) )

!
!           Now rotate by REFANG about NORMAL(1,1) using the routine
!           VROTV.
!
            CALL VROTV ( BSIGHT, NORMAL(1,1), REFANG, BOUNDS(1,1) )

!
!           Lastly, since we computed a single boundary corner vector,
!           set N = 1.
!
            N = 1

         ELSE IF ( SHAPE .EQ. 'ELLIPSE' ) THEN

!
!           The elliptical case requires the additional cross angle
!           keyword's presence in the kernel pool. Attempt to
!           retrieve it.
!
            CALL REPMI  ( KWCANG, '#',    INSTID, KWORD )
            CALL GDPOOL ( KWORD, 1, 1, I, CRSANG, FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL SETMSG ( 'The variable, ''#'', specifying the ' &
               //            'cross angle which describes instrument ' &
               //            '# FOV angular extent was not found in ' &
               //            'the kernel pool. Check whether IK file ' &
               //            'for the instrument was loaded into the ' &
               //            'program and whether this variable is ' &
               //            'specified in that file.'                 )
               CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
               CALL ERRINT ( '#', INSTID                               )
               CALL SIGERR ( 'SPICE(CROSSANGLEMISSING)'                )
               CALL CHKOUT ( 'GETFOV'                                  )
               RETURN

            END IF

!
!           Convert the cross angle to radians.
!
            CALL CONVRT ( CRSANG, ANGUNT, 'RADIANS', TMPANG )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'GETFOV' )
               RETURN
            END IF

            CRSANG = TMPANG
!
!           Now check to see that the caller left enough room
!           to store the required number of boundary corner
!           vectors.
!
            IF ( ROOM .LT. 2 ) THEN
               CALL SETMSG ( 'The FOV shape for instrument # ' &
               //            'is specified to be elliptical.  ' &
               //            'There should be room for at ' &
               //            'least two boundary vectors.  There ' &
               //            'is room for #. '                     )
               CALL ERRINT ( '#', INSTID                           )
               CALL ERRINT ( '#', ROOM                             )
               CALL SIGERR ( 'SPICE(BOUNDARYTOOBIG)'               )
               CALL CHKOUT ( 'GETFOV'                              )
               RETURN
            END IF

!
!           The plan to compute the first boundary corner vector is
!           to rotate the BSIGHT by REFANG towards REFVEC.  To
!           do this first compute the axis we need to rotate about.
!
            CALL VCRSS ( BSIGHT, REFVEC, NORMAL(1,1) )

!
!           Now rotate by REFANG about NORMAL(1,1) using the routine
!           VROTV.
!
            CALL VROTV ( BSIGHT, NORMAL(1,1), REFANG, BOUNDS(1,1) )

!
!           At this point we have one boundary vector.  We need the
!           second and final one.  The strategy we will use is the
!           following: rotate BSIGHT by CRSANG towards NORMAL(1,1).
!           This will give us boundary corner vectors listed in a
!           counter-clockwise fashion about the boresight.
!
            CALL VCRSS ( BSIGHT, NORMAL(1,1), TMPVEC      )
            CALL VEQU  ( TMPVEC,              NORMAL(1,2) )
!
!           Now rotate BSIGHT by CRSANG about the NORMAL(1,2) using
!           the routine VROTV.
!
            CALL VROTV ( BSIGHT, NORMAL(1,2), CRSANG, BOUNDS(1,2) )

!
!           Lastly, since we computed two boundary corner vectors,
!           set N = 2.
!
            N = 2

         ELSE IF ( SHAPE .EQ. 'RECTANGLE' ) THEN

!
!           The rectangular case requires the additional cross angle
!           keyword's presence in the kernel pool. Attempt to
!           retrieve it.
!
            CALL REPMI  ( KWCANG, '#',    INSTID, KWORD )
            CALL GDPOOL ( KWORD, 1, 1, I, CRSANG, FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL SETMSG ( 'The variable, ''#'', specifying the ' &
               //            'cross angle which describes instrument ' &
               //            '# FOV angular extent was not found in ' &
               //            'the kernel pool. Check whether IK file ' &
               //            'for the instrument was loaded into the ' &
               //            'program and whether this variable is ' &
               //            'specified in that file.'                 )
               CALL ERRCH  ( '#', KWORD(1:RTRIM(KWORD))                )
               CALL ERRINT ( '#', INSTID                               )
               CALL SIGERR ( 'SPICE(CROSSANGLEMISSING)'                )
               CALL CHKOUT ( 'GETFOV'                                  )
               RETURN

            END IF

!
!           Convert the cross angle to radians.
!
            CALL CONVRT ( CRSANG, ANGUNT, 'RADIANS', TMPANG )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'GETFOV' )
               RETURN
            END IF

            CRSANG = TMPANG
!
!           Now check to see that the caller left enough room
!           to store the required number of boundary corner
!           vectors.
!
            IF ( ROOM .LT. 4 ) THEN
               CALL SETMSG ( 'The FOV shape for instrument # ' &
               //            'is specified to be rectangular.  ' &
               //            'There should be room for at ' &
               //            'least four boundary vectors.  There ' &
               //            'is room for #. '                      )
               CALL ERRINT ( '#', INSTID                            )
               CALL ERRINT ( '#', ROOM                              )
               CALL SIGERR ( 'SPICE(BOUNDARYTOOBIG)'                )
               CALL CHKOUT ( 'GETFOV'                               )
               RETURN
            END IF

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
            CALL UNORM ( BSIGHT, B, BMAG )

!
!           Now proceed to (2). Since we already know that REFVEC
!           and BSIGHT are not parallel, the following yields a
!           non-zero vector:
!
            CALL VPERP ( REFVEC, BSIGHT, B1 )

!
!           Unitize B1.
!
            CALL VHAT  ( B1,     TMPVEC )
            CALL VEQU  ( TMPVEC, B1     )

!
!           Step (3), compute B2 by crossing B and B1.
!
            CALL VCRSS ( B, B1, B2 )

!
!           Before proceeding onto step (4), verify that the
!           results of the calculations in step (4) will make
!           sense.  Check the cosines of CRSANG and REFANG.
!           Signal an error if both are not positive numbers.
!           Use MINCOS as a tolerance.
!
            COSRAN = DCOS ( REFANG )
            COSCAN = DCOS ( CRSANG )

            IF ( ( COSRAN .LT. MINCOS ) .OR. &
                 ( COSCAN .LT. MINCOS )      ) THEN

               CALL SETMSG ( 'The angular extents specified ' &
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
               CALL ERRINT ( '#', INSTID                           )
               CALL SIGERR ( 'SPICE(BADBOUNDARY)'                  )
               CALL CHKOUT ( 'GETFOV'                              )
               RETURN

            END IF

!
!           Compute the NORMAL vectors to complete step (4).
!
            SINRAN = DSIN ( REFANG )
            SINCAN = DSIN ( CRSANG )

            CALL VLCOM ( -COSRAN, B1, SINRAN, B, NORMAL(1,1) )
            CALL VLCOM ( -COSCAN, B2, SINCAN, B, NORMAL(1,2) )
            CALL VLCOM (  COSRAN, B1, SINRAN, B, NORMAL(1,3) )
            CALL VLCOM (  COSCAN, B2, SINCAN, B, NORMAL(1,4) )

!
!           We are almost finished. Compute the boundary corner
!           vectors completing step (5).
!
            CALL VCRSS ( NORMAL(1,1), NORMAL(1,2), BOUNDS(1,1) )
            CALL VCRSS ( NORMAL(1,2), NORMAL(1,3), BOUNDS(1,2) )
            CALL VCRSS ( NORMAL(1,3), NORMAL(1,4), BOUNDS(1,3) )
            CALL VCRSS ( NORMAL(1,4), NORMAL(1,1), BOUNDS(1,4) )

!
!           Step (6), normalize the boundary corner vectors
!           and scale by BMAG, the magnitude of BSIGHT.
!
            DO I = 1, 4

               CALL UNORM ( BOUNDS(1,I),  TMPVEC,  VMAG        )
               CALL VSCL  ( BMAG,         TMPVEC,  BOUNDS(1,I) )

            END DO

!
!           Lastly since we are returning 4 boundary corner vectors,
!           set N = 4.
!
            N = 4

         ELSE

!
!           If we end up here something is terribly wrong with
!           this module or SPICE in general.
!
            CALL SETMSG ( 'This error is never supposed to ' &
            //            'occur. We have an undefined shape ' &
            //            'for the ANGLES specification that ' &
            //            'passed the shape check.'            )
            CALL SIGERR ( 'SPICE(BUG)'                         )
            CALL CHKOUT ( 'GETFOV'                             )
            RETURN

         END IF

      ELSE

         CALL SETMSG ( 'The FOV class specification is set to ''#'' ' &
         //            'which is currently unsupported. See the ' &
         //            'GETFOV subroutine header for more ' &
         //            'information.'                                 )
         CALL ERRCH  ( '#', SPEC                                      )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDSPEC)'                       )
         CALL CHKOUT ( 'GETFOV'                                       )
         RETURN

      END IF

!
!     Standard SPICE error handling.
!
      CALL CHKOUT ( 'GETFOV' )
      RETURN

      END
