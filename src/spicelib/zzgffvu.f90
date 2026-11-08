!$Procedure ZZGFFVU ( GF, instrument FOV utilities )

      SUBROUTINE ZZGFFVU ( INST,   TSHAPE, RAYDIR, TARGET, &
                           TFRAME, ABCORR, OBSRVR, TIME,   VISTAT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine contains the entry points that produce the
!     computations needed for solving for target visibility states
!     in the geometry finding routines.
!
! $ Required_Reading
!
!     CK
!     GF
!     NAIF_IDS
!     PCK
!     SCLK
!     SPK
!     TIME
!
! $ Keywords
!
!     EVENT
!     FOV
!     GEOMETRY
!     INSTRUMENT
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzocced.inc'
      INCLUDE 'gf.inc'

      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )

      CHARACTER*(*)         INST
      CHARACTER*(*)         TSHAPE
      DOUBLE PRECISION      RAYDIR ( 3 )
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         TFRAME
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      TIME
      LOGICAL               VISTAT

! $ Brief_I/O
!
!     VARIABLE  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     UBEL       P   All
!     UBPL       P   All
!     INST       I   ZZGFFVIN
!     TSHAPE     I   ZZGFFVIN
!     RAYDIR     I   ZZGFFVIN
!     TARGET     I   ZZGFFVIN
!     TFRAME     I   ZZGFFVIN
!     ABCORR     I   ZZGFFVIN
!     OBSRVR     I   ZZGFFVIN
!     TIME       I   ZZGFFVST
!     OCSTAT     O   ZZGFFVST
!
! $ Detailed_Input
!
!     See entry points.
!
! $ Detailed_Output
!
!     See entry points.
!
! $ Parameters
!
!     See INCLUDE files
!
!        gf.inc
!        zzgf.inc
!
! $ Exceptions
!
!     See entry points.
!
! $ Files
!
!     Appropriate SPK and instrument kernels must be loaded by the
!     calling program before this routine is called.  PCK, CK and SCLK
!     kernels may be required as well.
!
!     The following data are required:
!
!        - SPK data:  ephemeris data for target and observer that
!          describes the ephemeris of these objects for the period
!          defined by the confinement window, 'CNFINE' must be
!          loaded.  If aberration corrections are used, the states of
!          target and observer relative to the solar system barycenter
!          must be calculable from the available ephemeris data.
!          Typically ephemeris data are made available by loading one
!          or more SPK files via FURNSH.
!
!        - Frame data:  if a frame definition is required to convert
!          the observer and target states to the body-fixed frame of
!          the target, that definition must be available in the kernel
!          pool. Typically the definitions of frames not already
!          built-in to SPICE are supplied by loading a frame kernel.
!
!          Data defining the reference frame associated with the
!          instrument designated by INST must be available in the kernel
!          pool. Additionally the name INST must be associated with an
!          ID code. Normally these data are  made available by loading
!          a frame kernel via FURNSH.
!
!        - IK data: the kernel pool must contain data such that
!          the SPICELIB routine GETFOV may be called to obtain
!          parameters for INST. Normally such data are provided by
!          an IK via FURNSH.
!
!     The following data may be required:
!
!        - PCK data: bodies modeled as triaxial ellipsoids must have
!          orientation data provided by variables in the kernel pool.
!          Typically these data are made available by loading a text
!          PCK file via FURNSH.
!
!          Bodies modeled as triaxial ellipsoids must have semi-axis
!          lengths provided by variables in the kernel pool. Typically
!          these data are made available by loading a text PCK file via
!          FURNSH.
!
!        - CK data: if the instrument frame is fixed to a spacecraft,
!          at least one CK file will be needed to permit transformation
!          of vectors between that frame and both J2000 and the target
!          body-fixed frame.
!
!        - SCLK data:  if a CK file is needed, an associated SCLK
!          kernel is required to enable conversion between encoded SCLK
!          (used to time-tag CK data) and barycentric dynamical time
!          (TDB).
!
!     Kernel data are normally loaded once per program run, NOT every
!     time this routine is called.
!
! $ Particulars
!
!     This routine is designed to determine whether a specified
!     target intersects the space bounded by the FOV of a specified
!     instrument at a specified epoch. The target may be represented
!     by a ray, or the target may be an ephemeris object.
!
!     This routine contains two entry points that support searches
!     for target visibility periods performed using ZZGFSOLV:
!
!        ZZGFFVIN   Saves the user-supplied inputs defining the
!                   visibility computation to be performed.
!                   Initializes the visibility search.
!
!        ZZGFFVST   Returns the visibility state for a specified
!                   time.
!
! $ Examples
!
!     See GFFOVE.
!
! $ Restrictions
!
!     This is a SPICELIB private routine; it should not be called by
!     user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 24-NOV-2021 (NJB) (EDW)
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!        Bug fix: boresight is now scaled to unit length.
!
!        Bug fix: in the case of a circular or elliptical FOV, where
!        the target shape is an ellipsoid, this routine creates an
!        ellipsoid such that its limb coincides with the FOV.
!        Previously this ellipsoid was allowed to be very large when
!        the FOV itself was large. Now the sum of the maximum ellipsoid
!        radius plus the distance between the ellipsoid center and the
!        observer is limited to FVEMAX km. See entry point ZZGFFVIN for
!        details. These changes were made on 09-JUN-2017. Minor updates
!        were made on the date given by the version line.
!
!-    SPICELIB Version 1.1.0, 04-JUN-2013 (EDW)
!
!        Edit to ZZGFFVIN:
!
!        ABCORR now stripped of all spaces before saving.
!        Specifically, the call
!
!        CALL LJUST ( ABCORR, SVCORR )
!
!        replaced with
!
!        CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      INTEGER               ZZOCCED
      INTEGER               ZZWIND2D

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO


!
!     Local parameters
!

!
!     ATOL is a tolerance value for computing FOV angular radius.
!     The angular radius must not exceed pi/2 - ATOL radians.
!
      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL   = 1.D-6 )

      DOUBLE PRECISION      FVEMAX
      PARAMETER           ( FVEMAX = 1.D-5 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local variables
!
      CHARACTER*(FRNMLN)    SVIFRM
      CHARACTER*(BDNMLN)    SVINAM
      CHARACTER*(SHPLEN)    SVISHP
      CHARACTER*(FRNMLN)    SVTFRM
      CHARACTER*(BDNMLN)    SVTNAM
      CHARACTER*(SHPLEN)    SVTSHP
      CHARACTER*(CORLEN)    SVCORR

      DOUBLE PRECISION      BSITE  ( 3 )
      DOUBLE PRECISION      COORD  ( 2 )
      DOUBLE PRECISION      CTREXT
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      ELLRAD ( 3 )
      DOUBLE PRECISION      ESCALE
      DOUBLE PRECISION      ETTARG
      DOUBLE PRECISION      FOVPT  ( 3 )
      DOUBLE PRECISION      FVLIMB ( UBEL )
      DOUBLE PRECISION      INSMAT ( 3, 3 )
      DOUBLE PRECISION      L
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      M1     ( 3, 3 )
      DOUBLE PRECISION      M2     ( 3, 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      PNT2D  ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      SEMIPT ( 3, 2 )
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      SVARAD
      DOUBLE PRECISION      SVBNDS ( 3, MAXVRT )
      DOUBLE PRECISION      SVEDCT ( 3 )
      DOUBLE PRECISION      SVFAXI ( 3 )
      DOUBLE PRECISION      SVFOVM ( 3, 3 )
      DOUBLE PRECISION      SVFPOL ( 2, MAXVRT )
      DOUBLE PRECISION      SVFSMX ( 3, 3 )
      DOUBLE PRECISION      SVFVCT ( 3 )
      DOUBLE PRECISION      SVORIG ( 3 )
      DOUBLE PRECISION      SVPLAN ( UBPL )
      DOUBLE PRECISION      SVRDIR ( 3 )
      DOUBLE PRECISION      SVSEMI ( 3, 2 )
      DOUBLE PRECISION      SVTRAD ( 3 )
      DOUBLE PRECISION      SVXMAG ( 2 )
      DOUBLE PRECISION      TRGCTR ( 3 )
      DOUBLE PRECISION      TRGSMX ( 3, 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VTEMP2 ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               CLSSID
      INTEGER               FRAMID
      INTEGER               FRCENT
      INTEGER               FRCLSS
      INTEGER               I
      INTEGER               NXPTS
      INTEGER               OCSTAT
      INTEGER               SVINST
      INTEGER               SVNVRT
      INTEGER               SVOBS
      INTEGER               SVTARG
      INTEGER               W

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND
      LOGICAL               SVETRG
      LOGICAL               SVURAY
      LOGICAL               SVUSTL
      LOGICAL               SVXMIT

!
!     Saved variables
!
      SAVE                  SVARAD
      SAVE                  SVBNDS
      SAVE                  SVCORR
      SAVE                  SVEDCT
      SAVE                  SVETRG
      SAVE                  SVFAXI
      SAVE                  SVFOVM
      SAVE                  SVFPOL
      SAVE                  SVFSMX
      SAVE                  SVFVCT
      SAVE                  SVIFRM
      SAVE                  SVINAM
      SAVE                  SVINST
      SAVE                  SVISHP
      SAVE                  SVNVRT
      SAVE                  SVOBS
      SAVE                  SVORIG
      SAVE                  SVPLAN
      SAVE                  SVRDIR
      SAVE                  SVSEMI
      SAVE                  SVTARG
      SAVE                  SVTFRM
      SAVE                  SVTNAM
      SAVE                  SVTRAD
      SAVE                  SVTSHP
      SAVE                  SVURAY
      SAVE                  SVUSTL
      SAVE                  SVXMAG
      SAVE                  SVXMIT

!
!     Initial values
!
      DATA                  SVORIG / 3 * 0.D0 /

!
!     Below we initialize the list of visibility types.
!

!
!     This routine should never be called directly.
!
      CALL CHKIN  ( 'ZZGFFVU' )

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'ZZGFFVU' )
      RETURN




!$Procedure  ZZGFFVIN ( GF, visibility initialization )

      ENTRY ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                       TARGET, TFRAME, ABCORR, OBSRVR )

! $ Abstract
!
!    Perform initialization functions for visibility state
!    determination.
!
! $ Required_Reading
!
!     NAIF_IDS
!     FRAMES
!     PCK
!     SPK
!     TIME
!
! $ Keywords
!
!     SEARCH
!     GEOMETRY
!     VISIBILITY
!
! $ Declarations
!
!     CHARACTER*(*)         INST
!     CHARACTER*(*)         TSHAPE
!     DOUBLE PRECISION      RAYDIR ( 3 )
!     CHARACTER*(*)         TARGET
!     CHARACTER*(*)         TFRAME
!     CHARACTER*(*)         ABCORR
!     CHARACTER*(*)         OBSRVR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INST       I   Name of the instrument.
!     TSHAPE     I   Type of shape model used for target body.
!     RAYDIR     I   Ray's direction vector.
!     TARGET     I   Name of the target body.
!     TFRAME     I   Body-fixed, body-centered frame for target body.
!     ABCORR     I   Aberration correction flag.
!     OBSRVR     I   Name of the observing body.
!
! $ Detailed_Input
!
!
!     INST       indicates the name of the instrument, such as a
!                spacecraft-mounted framing camera, the field of view
!                (FOV) of which is to be used for a target intersection
!                search: times when the specified target intersects the
!                region of space corresponding to the FOV are sought.
!
!                The position of the instrument designated by INST is
!                considered to coincide with that of the ephemeris
!                object designated by the input argument OBSRVR (see
!                description below).
!
!                INST must have a corresponding NAIF ID and a frame
!                defined, as is normally done in a frame kernel. It
!                must also have an associated reference frame and a FOV
!                shape, boresight and boundary vertices (or reference
!                vector and reference angles) defined, as is usually
!                done in an instrument kernel.
!
!                See the header of the SPICELIB routine GETFOV for a
!                description of the required parameters associated with
!                an instrument.
!
!
!     TSHAPE     is a string indicating the geometric model used to
!                represent the location and shape of the target body.
!                The target body may be represented by either an
!                ephemeris object or a ray emanating from the observer.
!
!                The supported values of TSHAPE are:
!
!                   'ELLIPSOID'     The target is an ephemeris object.
!
!                                   The target's shape is represented
!                                   using triaxial ellipsoid model,
!                                   with radius values provided via the
!                                   kernel pool. A kernel variable
!                                   having a name of the form
!
!                                      'BODYnnn_RADII'
!
!                                   where nnn represents the NAIF
!                                   integer code associated with the
!                                   body, must be present in the kernel
!                                   pool. This variable must be
!                                   associated with three numeric
!                                   values giving the lengths of the
!                                   ellipsoid's X, Y, and Z semi-axes.
!
!                   'POINT'         The target is an ephemeris object.
!                                   The body is treated as a single
!                                   point.
!
!                   'RAY'           The target is NOT an ephemeris
!                                   object. Instead, the target is
!                                   considered to be represented by the
!                                   ray emanating from the observer's
!                                   location and having direction
!                                   vector RAYDIR. The target is
!                                   considered to be visible if and
!                                   only if the ray is contained within
!                                   the space bounded by the instrument
!                                   FOV.
!
!                Case and leading or trailing blanks are not
!                significant in the string TSHAPE.
!
!
!     RAYDIR     is the direction vector associated with a ray
!                representing the target. RAYDIR is used if and only
!                if TSHAPE (see description above) indicates the
!                target is modeled as a ray.
!
!
!     TARGET     is the name of the target body, the appearances of
!                which in the specified instrument's field of view are
!                sought. The body must be an ephemeris object.
!
!                Optionally, you may supply the integer NAIF ID code
!                for the body as a string. For example both 'MOON' and
!                '301' are legitimate strings that designate the Moon.
!
!                Case and leading or trailing blanks are not
!                significant in the string TARGET.
!
!                The input argument TARGET is used if and only if the
!                target is NOT modeled as ray; equivalently, the input
!                argument TSHAPE (see description above) does not
!                contain a string equivalent---that is, ignoring case
!                and leading and trailing blanks---to 'RAY'.
!
!                TARGET may be set to a blank string if the target is
!                modeled as a ray.
!
!
!     TFRAME     is the name of the reference frame associated with the
!                target. Examples of such names are 'IAU_SATURN'
!                (for Saturn) and 'ITRF93' (for the Earth).
!
!                If the target is an ephemeris object modeled as an
!                ellipsoid, TFRAME must designate a body-fixed
!                reference frame centered on the target body.
!
!                If the target is an ephemeris object modeled as a
!                point, TFRAME is ignored; TFRAME should be left blank.
!
!                If the target is modeled as a ray, TFRAME may
!                designate any reference frame. Since light time
!                corrections are not supported for rays, the
!                orientation of the frame is always evaluated at the
!                epoch associated with the observer, as opposed to the
!                epoch associated with the light-time corrected
!                position of the frame center.
!
!                Case and leading or trailing blanks bracketing a
!                non-blank frame name are not significant in the string
!                TFRAME.
!
!
!     ABCORR     indicates the aberration corrections to be applied
!                when computing the target's position and orientation.
!                The supported values of ABCORR depend on the target
!                representation.
!
!                If the target is represented by a ray, the aberration
!                correction options are
!
!                   'NONE'          No correction.
!                   'S'             Stellar aberration correction,
!                                   reception case.
!                   'XS'            Stellar aberration correction,
!                                   transmission case.
!
!                If the target is an ephemeris object, the aberration
!                correction options are those supported by the SPICE
!                SPK system. For remote sensing applications, where the
!                apparent position and orientation of the target seen
!                by the observer are desired, normally either of the
!                corrections
!
!                   'LT+S'
!                   'CN+S'
!
!                should be used. These and the other supported options
!                are described below.
!
!                Supported aberration correction options for
!                observation (case where radiation is received by
!                observer at ET) are:
!
!                   'NONE'       No correction.
!                   'LT'         Light time only
!                   'LT+S'       Light time and stellar aberration.
!                   'CN'         Converged Newtonian (CN) light time.
!                   'CN+S'       CN light time and stellar aberration.
!
!                Supported aberration correction options for
!                transmission (case where radiation is emitted from
!                observer at ET) are:
!
!                   'XLT'        Light time only.
!                   'XLT+S'      Light time and stellar aberration.
!                   'XCN'        Converged Newtonian (CN) light time.
!                   'XCN+S'      CN light time and stellar aberration.
!
!                For detailed information, see the geometry finder
!                required reading, gf.req.
!
!                Case, leading and trailing blanks are not significant
!                in the string ABCORR.
!
!
!     OBSRVR     is the name of the body from which the target is
!                observed. The instrument designated by INST is treated
!                as if it were co-located with the observer.
!
!                Optionally, you may supply the integer NAIF ID code
!                for the body as a string.
!
!                Case and leading or trailing blanks are not
!                significant in the string OBSRVR.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the observer's name cannot be mapped to an ID code, the
!         error SPICE(IDCODENOTFOUND) is signaled.
!
!     2)  If the target is an ephemeris object and its name cannot be
!         mapped to an ID code, the error SPICE(IDCODENOTFOUND) is
!         signaled. If the target is represented by a ray, the input
!         target name argument is ignored.
!
!     3)  If target is an ephemeris object, and the observer and
!         target have the same ID codes, the error
!         SPICE(BODIESNOTDISTINCT) is signaled.
!
!     4)  If target is an ephemeris object, and the target shape
!         is not equivalent to PTSHAP (point) or EDSHAP (ellipsoid),
!         the error SPICE(INVALIDSHAPE) is signaled.
!
!     5)  If target is an ephemeris object, the target shape is
!         equivalent to EDSHAP (ellipsoid), and the reference frame
!         argument TFRAME is blank, the error SPICE(INVALIDFRAME) is
!         signaled.
!
!     6)  If target is an ephemeris object, the target shape is
!         equivalent to EDSHAP (ellipsoid), and the reference frame
!         argument TFRAME cannot be mapped to a frame ID code, the
!         error SPICE(INVALIDFRAME) is signaled.
!
!     7)  If target is an ephemeris object, the target shape is
!         equivalent to EDSHAP (ellipsoid), and the reference frame
!         argument TFRAME's ID cannot be mapped to a frame description,
!         the error SPICE(FRAMEINFONOTFOUND) is signaled.
!
!     8)  If target is an ephemeris object, the target shape is
!         equivalent to EDSHAP (ellipsoid), and the reference frame
!         specified by TFRAME is not centered on the target body, the
!         error SPICE(INVALIDFRAME) is signaled.
!
!     9)  If the target is represented by a ray and the aberration
!         correction flag calls for light time correction, the error
!         SPICE(INVALIDOPTION) is signaled.
!
!     10) If target is an ephemeris object and the aberration
!         correction flag calls for a correction not supported by
!         the SPICE SPK system, the error is diagnosed by a routine
!         in the call tree of this routine.
!
!     11) If target is an ephemeris object, the target shape is
!         equivalent to EDSHAP (ellipsoid), and the kernel pool
!         does not contain radii for the target body,
!         not  target body, the error is diagnosed by a routine
!         in the call tree of this routine.
!
!     12) If target is an ephemeris object, the target shape is
!         equivalent to EDSHAP (ellipsoid), and the kernel pool
!         contains the wrong number of radii for the target body, the
!         error SPICE(INVALIDDIMENSION) is signaled.
!
!     13) If target is an ephemeris object, the target shape is
!         equivalent to EDSHAP (ellipsoid), and the kernel pool
!         contains one or more non-positive radii for the target body,
!         the error SPICE(BADAXISLENGTH) is signaled.
!
!     14) If the target is represented by a ray and the ray's
!         direction vector is zero, the error SPICE(ZEROVECTOR) is
!         signaled.
!
!     15) If the instrument name INST cannot be mapped to an ID code,
!         the error SPICE(IDCODENOTFOUND) is signaled.
!
!     16) If an error occurs while fetching the instrument parameters
!         from the kernel pool, the error will be diagnosed by a
!         routine in the call tree of this routine.
!
!     17) If any ray defined by the observer's position and one of
!         the instrument FOV's boundary vectors fails to intersect
!         the "FOV plane"---a plane normal to the instrument FOV axis
!         and intersected by the FOV axis at distance 1 km from the
!         observer---the error SPICE(DEGENERATECASE) is signaled.
!
!     18) If the FOV is circular or elliptical and the FOV's radius
!         or one of the FOV's semi-axis lengths is zero, the error
!         SPICE(DEGENERATECASE) is signaled.
!
!     19) If the maximum angular separation of the instrument
!         FOV axis and any FOV boundary vector exceeds the limit
!         (which is slightly less than 90 degrees), either the error
!         SPICE(FOVTOOWIDE) will be signaled or the error will be
!         diagnosed by a routine in the call tree of this routine.
!
!
! $ Files
!
!     See the header of the umbrella routine ZZGFFVU.
!
! $ Particulars
!
!     This entry point initializes the parameters needed by the
!     occultation state determination entry point ZZGFFVST.
!
! $ Examples
!
!     See implementation of GFFOVE.
!
! $ Restrictions
!
!     1) The reference frame associated with INST must be
!        centered at the observer or must be inertial. No check is done
!        to ensure this.
!
!     2) This is a SPICELIB private routine; it should not be called by
!        user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 1.3.0, 25-AUG-2021 (EDW)
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!-    SPICELIB Version 1.2.0, 01-NOV-2020 (NJB)
!
!        Bug fix: boresight is now scaled to unit length.
!
!        Bug fix: in the case of a circular or elliptical FOV, where
!        the target shape is an ellipsoid, this routine creates an
!        ellipsoid such that its limb coincides with the FOV.
!        Previously this ellipsoid was allowed to be very large when
!        the FOV itself was large. Now the sum of the maximum ellipsoid
!        radius plus the distance between the ellipsoid center and the
!        observer is limited to FVEMAX km. These changes were made on
!        09-JUN-2017. Minor updates were made on the date given by the
!        version line.
!
!-    SPICELIB Version 1.1.0, 04-JUN-2013 (EDW)
!
!        ABCORR now stripped of all spaces before saving.
!        Specifically, the call
!
!        CALL LJUST ( ABCORR, SVCORR )
!
!        replaced with
!
!        CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.2.0, 29-OCT-2020 (NJB)
!
!        Bug fix: in the case of a circular or elliptical FOV, where
!        the target shape is an ellipsoid, this routine creates an
!        ellipsoid such that its limb coincides with the FOV. Previously
!        this ellipsoid was allowed to be very large when the FOV itself
!        was large. Now the sum of the maximum ellipsoid radius plus
!        the distance between the ellipsoid center and the observer is
!        limited to FVEMAX km.
!
!        Renamed local LOGICAL variable SVXTRG (save extended target)
!        to SVETRG (save ellipsoidal target), since it does not apply
!        to DSK targets. Note that DSK targets currently are not
!        supported.
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFFVIN' )

!
!     To avoid portability problems, initialize all
!     saved variables that aren't initialized via DATA
!     statements and aren't guaranteed to be initialized
!     for all cases.
!
      CALL CLEARD ( 3*MAXVRT, SVBNDS )
      CALL CLEARD ( 3,        SVEDCT )
      CALL CLEARD ( 3,        SVFAXI )
      CALL CLEARD ( 2*MAXVRT, SVFPOL )
      CALL CLEARD ( 9,        SVFSMX )
      CALL CLEARD ( UBPL,     SVPLAN )
      CALL CLEARD ( 3,        SVRDIR )
      SVTARG = 0
      SVTFRM = ' '
      SVTNAM = ' '
      CALL CLEARD ( 3,        SVTRAD )
      SVUSTL = .FALSE.
      SVXMIT = .FALSE.

!
!     Find the NAIF ID for OBSRVR.
!
      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFFVIN'                                 )
         RETURN

      END IF

!
!     Process the target shape specifier here.
!
!     Save a left-justified, upper case version of the target shape
!     specifier.
!
      CALL LJUST ( TSHAPE, SVTSHP )
      CALL UCASE ( SVTSHP, SVTSHP )

!
!     Note for maintenance programmer: these checks will
!     require modification to handle DSK-based shapes.
!
      IF (      ( SVTSHP .NE. PTSHAP ) &
          .AND. ( SVTSHP .NE. EDSHAP ) &
          .AND. ( SVTSHP .NE. RYSHAP )  ) THEN

         CALL SETMSG ( 'The target shape specification, ' &
         //            '''#'', is not recognized.'       )
            CALL ERRCH  ( '#', TSHAPE                    )
         CALL SIGERR ( 'SPICE(INVALIDSHAPE)'             )
         CALL CHKOUT ( 'ZZGFFVIN'                        )
         RETURN

      END IF

!
!     We'll use the logical variable USERAY to indicate that the
!     target is modeled as ray.
!
      SVURAY = SVTSHP .EQ. RYSHAP

!
!     Indicate whether we have an ellipsoidal target. SVETRG is .TRUE.
!     if and only we have one.
!
      SVETRG = SVTSHP .EQ. EDSHAP

!
!     If the target is an ephemeris object, obtain its ID code.
!     Save the target object's name, if applicable.
!
      IF ( .NOT. SVURAY ) THEN

         CALL BODS2C ( TARGET, SVTARG, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'The target object, ' &
            //            '''#'', is not a recognized name for an ' &
            //            'ephemeris object. The cause of this ' &
            //            'problem may be that you need an updated ' &
            //            'version of the SPICE Toolkit. '           )
            CALL ERRCH  ( '#', TARGET                                )
            CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
            CALL CHKOUT ( 'ZZGFFVIN'                                 )
            RETURN

         END IF

!
!        Save the target's name.
!
         SVTNAM = TARGET

!
!        Make sure the observer and target are distinct.
!
         IF ( SVTARG .EQ. SVOBS ) THEN

            CALL SETMSG ( 'The observer and target must be ' &
            //            'distinct objects, but are not: ' &
            //            'OBSRVR = #; TARGET = #;'         )
            CALL ERRCH  ( '#', OBSRVR                       )
            CALL ERRCH  ( '#', TARGET                       )
            CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'        )
            CALL CHKOUT ( 'ZZGFFVIN'                        )
            RETURN

         END IF

      END IF

!
!     Process the target frame. The target frame is defined except
!     when the target is an ephemeris object modeled as a point.
!
      IF ( SVURAY .OR. SVETRG ) THEN
!
!        We'll use the target frame argument. Look up the target
!        frame's ID code. But first, check for a blank frame name,
!        since this may be a common problem for the GF FOV system.
!
         IF ( TFRAME .EQ. ' ' ) THEN

            CALL SETMSG ( 'The target is not modeled as a ' &
            //            'point, but the associated ' &
            //            'frame name is blank.'           )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZGFFVIN'                       )
            RETURN

         END IF

         CALL NAMFRM ( TFRAME, FRAMID )

         IF ( FRAMID .EQ. 0 ) THEN

            CALL SETMSG ( 'The target frame name # is not recognized.' )
            CALL ERRCH  ( '#',  TFRAME                                 )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'                        )
            CALL CHKOUT ( 'ZZGFFVIN'                                   )
            RETURN

         END IF

!
!        Save the target frame name.
!
         CALL LJUST ( TFRAME, SVTFRM )
         CALL UCASE ( SVTFRM, SVTFRM )

!
!        Obtain the center of the frame. If the target is an ephemeris
!        object, we must verify the frame center is the target.
!
         CALL FRINFO ( FRAMID, FRCENT, FRCLSS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN
!
!           Since we mapped the frame name to an ID code, we expect to
!           find the frame info. Getting here may be a sign of an
!           invalid frame kernel.
!
            CALL SETMSG ( 'Frame ID found for # body-fixed ' &
            //            'frame # but FRINFO couldn''t find frame ' &
            //            'info. This may be due to a frame kernel ' &
            //            'error.'                                  )
            CALL ERRCH  ( '#',  TARGET                              )
            CALL SIGERR ( 'SPICE(FRAMEINFONOTFOUND)'                )
            CALL CHKOUT ( 'ZZGFFVIN'                                )
            RETURN

         END IF

         IF ( SVETRG ) THEN
!
!           We have an ellipsoidal target. Check the target frame's
!           center.
!
            IF ( FRCENT .NE. SVTARG ) THEN
!
!              The supposed body-fixed frame for the target isn't
!              actually centered on the target.
!
               CALL SETMSG ( 'Supposed body-fixed frame # for ' &
               //            'target # is actually centered ' &
               //            'on body #.'                       )
               CALL ERRCH  ( '#',  TFRAME                       )
               CALL ERRCH  ( '#',  TARGET                       )
               CALL ERRINT ( '#',  FRCENT                       )
               CALL SIGERR ( 'SPICE(INVALIDFRAME)'              )
               CALL CHKOUT ( 'ZZGFFVIN'                         )
               RETURN

            END IF

         END IF

      END IF

!
!     Process the aberration correction specifier.
!
      IF ( SVURAY ) THEN
!
!        The target is represented by a ray. Check and save the
!        aberration correction.
!
         CALL ZZPRSCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVIN' )
            RETURN
         END IF

!
!        Reject aberration correction flags calling for any type of
!        light time correction. However, stellar aberration corrections
!        are allowed: note this is the reverse of the situation for
!        ephemeris objects. The allowed aberration correction flags are
!
!           'NONE', 'S', 'XS'
!
         IF ( ATTBLK(LTIDX) ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for ' &
            //            'light time corrections; these are not ' &
            //            'supported for targets represented by ' &
            //            'rays.'                                  )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                   )
            CALL CHKOUT ( 'ZZGFFVIN'                               )
            RETURN

         END IF

!
!        Save flags indicating whether to use stellar aberration
!        corrections and indicating the sense of radiation travel.
!
         SVUSTL = ATTBLK(STLIDX)
         SVXMIT = ATTBLK(XMTIDX)


      ELSE
!
!        The target is an ephemeris object.
!
!        Check the aberration correction. If SPKEZR can't handle it,
!        neither can we.
!
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVIN' )
            RETURN
         END IF

      END IF

!
!     Remove all spaces from ABCORR then convert to uppercase. Save
!     this version of the aberration correction specifier.
!
      CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
      CALL UCASE ( SVCORR, SVCORR )

!
!     Process the target body's radii, if applicable.
!
      IF ( SVETRG ) THEN

!
!        Fetch and check the radii.
!
         CALL ZZGFTREB ( SVTARG, SVTRAD )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVIN' )
            RETURN
         END IF

!
!        Checks of radii have been completed.
!
      ELSE
!
!        We don't have an ellipsoidal target body: zero out radius
!        values for this target.
!
         CALL CLEARD ( 3, SVTRAD )

      END IF

!
!     Check the direction vector, if applicable.
!
      IF ( SVURAY ) THEN
!
!        Make sure the direction vector is non-zero. Save a unit-length
!        copy of the vector.
!
         IF ( VZERO( RAYDIR ) ) THEN

            CALL SETMSG ( 'Input ray direction was the zero vector; ' &
            //            'this vector must be non-zero.'            )
            CALL SIGERR ( 'SPICE(ZEROVECTOR)'                        )
            CALL CHKOUT ( 'ZZGFFVIN'                                 )
            RETURN

         END IF

         CALL VHAT ( RAYDIR, SVRDIR )

      END IF

!
!     Look up the instrument's ID code.
!
      CALL BODS2C ( INST, SVINST, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( '''#'' is not a recognized name for an ' &
         //            'instrument. The cause of this ' &
         //            'problem may be that you have not loaded ' &
         //            'a required frame kernel or instrument ' &
         //            'kernel.'                                  )
         CALL ERRCH  ( '#', INST                                  )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFFVIN'                                 )
         RETURN

      END IF

!
!     Save the instrument's name.
!
      CALL LJUST ( INST,   SVINAM )
      CALL UCASE ( SVINAM, SVINAM )

!
!     Look up the instrument parameters.
!
      CALL GETFOV ( SVINST, MAXVRT, SVISHP, &
                    SVIFRM, BSITE,  SVNVRT, SVBNDS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFFVIN' )
         RETURN
      END IF

!
!     Scale boresight vector to unit length.
!
      CALL VHATIP( BSITE )

!
!     Make sure the instrument shape specifier is left-justified
!     and in upper case.
!
      CALL LJUST ( SVISHP, SVISHP )
      CALL UCASE ( SVISHP, SVISHP )

!
!     If the instrument's shape is 'RECTANGLE', map it to
!     'POLYGON'
!
      IF (  SVISHP .EQ. RECFOV ) THEN

         SVISHP = POLFOV

      END IF

!
!     Save an axis vector for the FOV. For circular and ellipsoidal
!     FOVs, the boresight serves as this axis. For polygonal FOVs
!     (rectangular FOVs are included), we'll generate an axis vector.
!
      IF ( SVISHP .EQ. POLFOV ) THEN

         CALL ZZFOVAXI ( INST,  SVNVRT, SVBNDS, SVFAXI )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVIN' )
            RETURN
         END IF

      ELSE

         CALL VEQU ( BSITE, SVFAXI )

      END IF

!
!     Check the angular radius of the FOV.
!
!     Compute the angular radius of the FOV. We'll use this to define a
!     "bounding cone" centered on the FOV axis and having its apex at
!     the observer. This cone will be used for a preliminary FOV
!     exclusion test.
!
      SVARAD = 0.D0

      DO I = 1, SVNVRT
         SVARAD = MAX (  SVARAD,  VSEP( SVBNDS(1,I), SVFAXI )  )
      END DO

!
!     Our algorithms can't handle FOVs with angular radius of 90
!     degrees.
!
      IF (  SVARAD   .GT. ( HALFPI()-ATOL )  ) THEN

         CALL SETMSG ( 'FOV angular radius of # degrees ' &
         //            'exceeds limit of # degrees.'     )
         CALL ERRDP  ( '#',  SVARAD*DPR()                )
         CALL ERRDP  ( '#',  (HALFPI()-ATOL)*DPR()       )
         CALL SIGERR ( 'SPICE(FOVTOOWIDE)'               )
         CALL CHKOUT ( 'ZZGFFVIN'                        )
         RETURN

      END IF

!
!     Convert the FOV shape specifier to a left-justified, upper
!     case form.
!
      CALL LJUST ( SVISHP, SVISHP )
      CALL UCASE ( SVISHP, SVISHP )

!
!     We can make the search more efficient by computing any
!     required, time-invariant quantities here in the initialization
!     routine.
!
!     Compute the FOV plane SVPLAN, which is represented in the
!     instrument frame. The origin will be considered to be located at
!     the observer. The plane is normal to the FOV axis, at distance 1
!     unit from the observer.
!
      CALL NVC2PL ( SVFAXI, 1.D0, SVPLAN )

!
!     Find the point on the plane closest to the origin. This is
!     the center of the FOV.
!
      CALL VHAT ( SVFAXI, SVFVCT )

!
!     If applicable, perform the computations required for an
!     elliptical FOV, where the target representation is arbitrary, or
!     a circular FOV when the target is an ellipsoid. Note that
!     these computations are not needed for the combination of a
!     circular FOV and a point or ray target.
!
      IF (        ( SVISHP .EQ. ELLFOV ) &
           .OR. ( ( SVISHP .EQ. CIRFOV ) .AND. SVETRG )  ) THEN
!
!        Also compute the center, semi-axis vectors, and semi-axis
!        lengths of the FOV. If the FOV is circular, we create an
!        artificial, second semi-axis vector.
!
         IF ( SVISHP .EQ. CIRFOV ) THEN
!
!           We have a circular FOV. We'll create an artificial, second
!           boundary vector, which will give rise to a second
!           semi-axis.
!
            CALL VROTV ( SVBNDS(1,1), SVFAXI, HALFPI(), SVBNDS(1,2) )

         END IF

!
!        Now find the endpoints of the semi-axes in this plane.
!
         DO I = 1, 2

            CALL INRYPL ( SVORIG, SVBNDS(1,I), &
                          SVPLAN, NXPTS,       SEMIPT(1,I) )

            IF ( NXPTS .NE. 1 ) THEN

               CALL SETMSG ( 'Error creating FOV semi-axis ' &
               //            'vectors, NXPTS = #. This ' &
               //            'may indicate an error in the ' &
               //            'IK parameters for #.'         )
               CALL ERRINT ( '#', NXPTS                     )
               CALL ERRCH  ( '#', INST                      )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'        )
               CALL CHKOUT ( 'ZZGFFVIN'                     )
               RETURN

            END IF

!
!           Compute and find the length of each semi-axis vector.
!
            CALL VSUB ( SEMIPT(1,I), SVFVCT, SVSEMI(1,I) )

            SVXMAG(I) = VNORM ( SVSEMI(1,I) )

            IF ( SVXMAG(I) .EQ. 0.D0 ) THEN

               CALL SETMSG ( 'FOV semi-axis #* for @ ' &
               //            'has zero length.'        )
               CALL ERRINT ( '*', I                    )
               CALL ERRCH  ( '@', INST                 )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'   )
               CALL CHKOUT ( 'ZZGFFVIN'                )
               RETURN

            END IF

         END DO

      END IF

!
!     If we have an ellipsoidal target, and the FOV is circular or
!     elliptical, we'll create an ellipsoid whose limb coincides with
!     the FOV. This allows use to later use ZZOCCED to determine the
!     target's visibility.
!
      IF (   (      ( SVISHP .EQ. CIRFOV ) &
               .OR. ( SVISHP .EQ. ELLFOV ) )  .AND.  SVETRG  ) THEN
!
!
!        Create an ellipsoid whose semi-axes are consistent with the
!        ellipse in SVPLAN defined by SEMIPT. Caution: after we
!        create this ellipsoid, we'll scale and shift it so it doesn't
!        extend too far from the observer.
!
!        Recall the origin is that of the instrument frame. The plane
!        SVPLAN is normal to the FOV axis and has distance 1 km from
!        the origin.
!
!        To start out, select the center of the ellipsoid. We place the
!        center along the direction defined by the FOV axis, at a
!        distance beyond SVPLAN (that is, on the side of the plane
!        opposite the observer), such that a sphere centered at this
!        point would have a limb consisting of a circle of radius
!        SVXMAG(1). If CTREXT is the distance of the ellipsoid center
!        from SVFVCT, then the limb geometry requires
!
!           CTREXT / SVXMAG(1) = SVXMAG(1) / 1
!
!
         CTREXT = SVXMAG(1) ** 2

!
!        The ellipsoid's center is SVEDCT.
!
         CALL VSCL ( 1.D0 + CTREXT, SVFVCT, SVEDCT )

!
!        NOTE: in the code and discussion that follow, there are
!        references to both the FOV center SVFVCT and the ellipsoid
!        center SVEDCT. Note that the directions of the ellipsoid's
!        semi-axes point from the FOV center, NOT the ellipsoid center,
!        toward the intercepts of the FOV boundary vectors on the
!        FOV plane.
!
!        Compute the radius of the sphere centered at SVEDCT. The
!        ellipsoid's semi-axes pointing in the FOV axis direction and
!        in the direction from SVFVCT toward SEMIPT(*,1) will have this
!        length.
!
         ELLRAD(3) = SVXMAG(1) * SQRT ( 1.D0 + SVXMAG(1)**2.D0 )
         ELLRAD(1) = ELLRAD(3)

!
!        Compute the corresponding columns of the FOV semi-axis matrix.
!
!        The ellipsoid's third axis points along the FOV axis. Note
!        that SVFVCT is a unit vector pointing in the desired
!        direction.
!
         CALL VSCL ( ELLRAD(3), SVFVCT, SVFSMX(1,3) )

!
!        The first ellipsoid semi-axis is associated with SEMIPT(*,1)
!        and also has length ELLRAD(3):
!
         CALL VHAT ( SVSEMI(1,1), VTEMP )
         CALL VSCL ( ELLRAD(1),   VTEMP,  SVFSMX(1,1) )

!
!        The ellipsoid's second semi-axis points from SVFVCT toward
!        SEMIPT(*,2). The ratio of its length to that of the other
!        semi-axis is the ratio of the length of the FOV's second
!        semi-axis to that of its first. Note that we've already ruled
!        out divide-by-zero errors here.
!
         ELLRAD(2) = ( SVXMAG(2)/SVXMAG(1) ) * ELLRAD(3)

!
!        We define the third axis using a cross product to
!        ensure we produce a matrix with positive determinant.
!
         CALL UCRSS  ( SVFSMX(1,3), SVFSMX(1,1), VTEMP )
         CALL VSCL   ( ELLRAD(2),   VTEMP,       SVFSMX(1,2) )

!
!        Scale the ellipsoid and the distance of its center from
!        the observer so that the ellipsoid is of reasonable size
!        and doesn't extend too far from the observer. Caution: this
!        modification means the ellipsoid no longer intersects the FOV
!        plane at the FOV boundary. The ellipsoid is still usable with
!        ZZOCCED, which is the ellipsoid's raison d'etre.
!
         ESCALE = FVEMAX/ ( VNORM(SVEDCT) + MAX(ELLRAD(1), ELLRAD(2)) )

         DO I = 1, 3
            CALL VSCLIP ( ESCALE, SVFSMX(1,I) )
         END DO

         CALL VSCLIP ( ESCALE, SVEDCT )

      END IF


      IF ( ( SVISHP .EQ. CIRFOV ) .AND. ( .NOT. SVETRG ) ) THEN
!
!        We have a circular FOV and a point or ray target model.
!        In this case, our FOV inclusion test is simple as can
!        be: we just compare the angular separation of the
!        target and FOV axis against the angular radius of the
!        FOV. Compute and save this angular radius.
!
         SVARAD = VSEP ( SVFAXI, SVBNDS(1,1) )


      ELSE IF (       (      ( SVISHP .EQ. RECFOV ) &
                        .OR. ( SVISHP .EQ. POLFOV ) ) &
                .AND. ( .NOT.  SVETRG               )   ) THEN
!
!        We have a rectangular or polygonal FOV and a ray or point
!        target.
!
!        We're going to represent the FOV boundary by a polygon
!        in the FOV plane SVPLAN. We want to be able to use a
!        2-dimensional winding number computation to decide whether
!        the target is within the FOV. We'll need a reference
!        frame with the Z-axis parallel to the FOV axis vector;
!        we'll represent the intersections of the boundary vectors
!        with the FOV plane in this frame. Then our 2D polygon
!        will have vertices given by the (X,Y) components of each
!        intersection.
!
         CALL VEQU ( SVFAXI, Z )

         CALL FRAME ( Z, X, Y )

         DO I = 1, 3
            SVFOVM(1,I) = X(I)
            SVFOVM(2,I) = Y(I)
            SVFOVM(3,I) = Z(I)
         END DO

!
!        Compute the intersections of the FOV boundary vectors with the
!        FOV plane. For each intercept, find the vector pointing from
!        the FOV center to that intercept. Transform each such
!        difference vector into the FOV frame. Save the projection onto
!        the FOV frame's X-Y plane.
!
         DO I = 1, SVNVRT

            CALL INRYPL ( SVORIG, SVBNDS(1,I), &
                          SVPLAN, NXPTS,       XPT)

            IF ( NXPTS .NE. 1 ) THEN

               CALL SETMSG ( 'Error finding FOV plane intercept ' &
               //            'of FOV boundary vector #, NXPTS = #. ' &
               //            'This may indicate an error ' &
               //            'in the IK parameters for #.'          )
               CALL ERRINT ( '#', I                                 )
               CALL ERRINT ( '#', NXPTS                             )
               CALL ERRCH  ( '#', INST                              )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'                )
               CALL CHKOUT ( 'ZZGFFVIN'                             )
               RETURN

            END IF

            CALL VSUB ( XPT,   SVFVCT, VTEMP  )
            CALL MXV ( SVFOVM, VTEMP,  VTEMP2 )

            SVFPOL(1,I) = VTEMP2(1)
            SVFPOL(2,I) = VTEMP2(2)

         END DO

      END IF

      CALL CHKOUT ( 'ZZGFFVIN' )
      RETURN



!$Procedure ZZGFFVST ( GF, "is target in FOV?"  )

      ENTRY ZZGFFVST ( TIME, VISTAT )

! $ Abstract
!
!     Indicate whether the target is currently in the instrument FOV.
!
! $ Required_Reading
!
!     SPK
!     TIME
!
! $ Keywords
!
!     SEARCH
!     GEOMETRY
!
! $ Declarations
!
!     DOUBLE PRECISION      TIME
!     LOGICAL               VISTAT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TIME       I   TDB epoch (in seconds past J2000)
!     VISTAT     O   .TRUE. if the object is visible, .FALSE.
!                    otherwise.
!
! $ Detailed_Input
!
!     TIME       is the epoch of interest in TDB seconds past the
!                J2000 epoch.
!
! $ Detailed_Output
!
!     VISTAT     is a logical flag indicating the state of visibility.
!                If the target is in the instrument FOV at epoch TIME,
!                where target and instrument are those specified by the
!                last call to ZZGFFVIN, VISTAT is returned with the
!                value .TRUE.; otherwise VISTAT is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If any SPK lookup fails, the error will be diagnosed by
!        routines in the call tree of this routine.
!
!     2) If any frame transformation lookup fails, the error will be
!        diagnosed by routines in the call tree of this routine.
!
!     3) If the FOV is polygonal, the target is an ellipsoid,
!        and while testing whether the target is visible, an error
!        occurs due to FOV errors not detected in the initialization
!        step, the error will be diagnosed by routines in the call tree
!        of this routine.
!
!     4) If the FOV is circular or elliptical, the target is an
!        ellipsoid, and while testing whether the target is visible, an
!        error occurs due to degenerate geometry of the limb, FOV, or
!        both, the error will be diagnosed by routines in the call tree
!        of this routine.
!
!     5) If the target shape is not recognized, the error will be
!        diagnosed by routines in the call tree of this routine.
!
!     6)  If a body RADII vector has other than exactly thee elements,
!         the error SPICE(INVALIDCOUNT) is signaled by a routine in
!         the call tree of this routine.
!
!     7)  If a body RADII vector has any element less-than or
!         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     See the Files header section of the umbrella routine ZZGFFVU.
!
! $ Particulars
!
!     This routine determines the visibility state of the
!     configuration specified by the last call to ZZGFFVIN and the
!     input time value.
!
! $ Examples
!
!     See the umbrella routine ZZGFFVU.
!
! $ Restrictions
!
!     This is a SPICELIB private routine; it should not be called by
!     user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (EDW)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFFVST' )

!
!     Initialize the state output.
!
      VISTAT = .FALSE.

!
!     The algorithm for the state determination depends on the
!     target model and the FOV shape.
!
      IF ( SVETRG ) THEN
!
!        The target is an ephemeris object modeled as an ellipsoid.
!        There are two branches here: one for a rectangular/
!        polygonal FOV and one for a circular/elliptical FOV.
!
!        Start by finding the observer-target position vector in the
!        target body-fixed frame.
!
         CALL SPKEZP ( SVTARG, TIME, SVTFRM, SVCORR, SVOBS, POS, LT )

!
!        Compute the target epoch.
!
         CALL ZZCOREPC ( SVCORR, TIME, LT, ETTARG )

!
!        Find the transformation from the target frame at ETTARG to the
!        instrument frame at TIME. We'll need to use J2000 as an
!        intermediate frame.
!
         CALL PXFORM ( SVTFRM,  'J2000', ETTARG, M1 )
         CALL PXFORM ( 'J2000', SVIFRM,  TIME,   M2 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVST' )
            RETURN
         END IF

         CALL MXM ( M2, M1, INSMAT )


         IF ( ( SVISHP .EQ. RECFOV ) .OR. ( SVISHP .EQ. POLFOV ) ) THEN
!
!           The FOV is a rectangle or other polygon; we treat both
!           cases the same way.
!
!           Negate POS to obtain the position of the observer with
!           respect to the target.
!
            CALL VMINUS ( POS, OBSPOS )

!
!           Find the limb in the target body-fixed frame.
!
            CALL EDLIMB ( SVTRAD(1), SVTRAD(2), SVTRAD(3), &
                          OBSPOS,    LIMB                  )

!
!           Transform the limb from the target frame at ETTARG
!           to the instrument frame at TIME. The matrix INSMAT
!           effects just this transformation. We unpack the center
!           and semi-axis vectors of LIMB, transform them, and
!           pack the results into FVLIMB. Below, M1 and M2 are
!           simply temporary 3x3 matrices.
!
            CALL EL2CGV ( LIMB, M1(1,1), M1(1,2), M1(1,3) )

!
!           Before performing the frame transformation on the
!           limb's center, translate the center so that the
!           observer is at the origin. Since POS is expressed
!           in the target body-fixed frame, this is a convenient
!           place for the translation.
!
            CALL VADD ( POS,   M1(1,1), VTEMP )
            CALL VEQU ( VTEMP, M1(1,1)        )

            DO I = 1, 3
               CALL MXV ( INSMAT, M1(1,I), M2(1,I) )
            END DO

            CALL CGV2EL (  M2(1,1), M2(1,2), M2(1,3), FVLIMB )

!
!           All geometric objects in the following call are expressed
!           in the instrument reference frame.
!
!           The target is in the FOV if and only if ZZELVUPY finds an
!           intersection, so we use VISTAT as the "found" flag.
!
            CALL ZZELVUPY ( FVLIMB, SVORIG, SVFAXI, &
                            SVNVRT, SVBNDS, VISTAT )


         ELSE IF (      ( SVISHP .EQ. CIRFOV ) &
                   .OR. ( SVISHP .EQ. ELLFOV )  ) THEN
!
!           The FOV is a circle or ellipse. For both FOV shapes,
!           we represent the FOV by an ellipsoid in the FOV
!           frame. We can then use ZZOCCED to determine whether
!           there's any overlap of this ellipsoid and the target.
!
!           We'll perform the occultation test in the instrument frame,
!           so we'll need to represent the observer-target position
!           and target semi-axes in that frame.
!
!           Transform the target position to the instrument frame.
!
            CALL MXV ( INSMAT, POS, TRGCTR )

!
!           The columns of INSMAT are the target body's semi-axis
!           direction vectors; we scale these by the target radii
!           to obtain the semi-axis matrix for the target.
!
            DO I = 1, 3
               CALL VSCL ( SVTRAD(I), INSMAT(1,I), TRGSMX(1,I) )
            END DO

            OCSTAT = ZZOCCED ( SVORIG, SVEDCT, SVFSMX, TRGCTR, TRGSMX )

!
!           A return code of zero indicates no occultation. Any other
!           return code indicates a non-empty intersection of the
!           target and FOV.
!
            VISTAT = OCSTAT .NE. 0

         ELSE
!
!           This is an unexpected FOV shape. We should have prevented
!           this problem in the initialization step, but we repeat the
!           check here for safety.
!
            CALL SETMSG ( 'The target body # has shape #; the only ' &
            //            'supported shapes are ELLIPSOID, POINT, ' &
            //            'and RAY.'                                )
            CALL ERRCH  ( '#',  SVTNAM                              )
            CALL ERRCH  ( '#',  SVISHP                              )
            CALL SIGERR ( 'SPICE(INVALIDSHAPE)'                     )
            CALL CHKOUT ( 'ZZGFFVST'                                )
            RETURN

         END IF

!
!        This is the end of the ellipsoidal target case. At this
!        point, VISTAT is set.
!

      ELSE
!
!        The target is a ray or an ephemeris object modeled as a point.
!        In either case, we want to obtain the aberration-corrected
!        observer-target vector.
!
         IF ( SVURAY ) THEN
!
!           The target is represented by a ray expressed in the
!           frame SVTFRM.
!
!           Normally we'd need to correct the orientation of SVTFRM
!           for light time between the center of that frame and the
!           observer. But since light time corrections are not allowed
!           for targets represented by rays, we evaluate SVTFRM
!           at the current epoch TIME.
!
            CALL PXFORM ( SVTFRM, SVIFRM, TIME, INSMAT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFFVST' )
               RETURN
            END IF

!
!           Transform the ray's direction vector to the instrument
!           frame.
!
            CALL MXV ( INSMAT, SVRDIR, DIR )

!
!           If we need to correct the ray's direction for stellar
!           aberration, do it now.
!
            IF ( SVUSTL ) THEN
!
!              Find the state of the observer relative to the
!              solar system barycenter in the J2000 frame.
!
               CALL SPKSSB ( SVOBS, TIME, 'J2000', STOBS )

!
!              Convert the direction vector to the J2000 frame.
!
               CALL PXFORM ( SVIFRM, 'J2000', TIME, M1 )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZGFFVST' )
                  RETURN
               END IF

               CALL MXV ( M1, DIR, VTEMP )

!
!              Apply the stellar aberration correction.
!
               IF ( SVXMIT ) THEN
!
!                 Use the transmission correction.
!
                  CALL STLABX ( VTEMP, STOBS(4), VTEMP2 )
               ELSE
                  CALL STELAB ( VTEMP, STOBS(4), VTEMP2 )
               END IF

!
!              Map the direction vector back to the instrument
!              frame.
!
               CALL MTXV ( M1, VTEMP2, DIR )

            END IF
!
!           The target direction in the instrument frame DIR has
!           been computed.
!

         ELSE
!
!           The target is an ephemeris object. Look up the
!           target's position relative to the observer.
!
!           Note for the maintenance programmer: don't think of
!           changing this call to look up the position in the
!           instrument frame. :) Since we don't have a guarantee that
!           the instrument frame is centered on the observer (the frame
!           could be J2000, for example), and since we don't want to
!           correct the orientation of the instrument frame for light
!           time, we look up the direction vector in the J2000 frame
!           and then map it to the instrument frame.
!
            CALL SPKEZP ( SVTARG,  TIME,   'J2000', &
                          SVCORR,  SVOBS,  VTEMP, LT )

            CALL PXFORM ( 'J2000', SVIFRM, TIME,  M1 )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFFVST' )
               RETURN
            END IF

            CALL MXV ( M1, VTEMP, DIR )

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFFVST' )
            RETURN
         END IF

!
!        The observer-target direction vector DIR is set.
!
!        The determination of whether the ray is in the FOV depends
!        on the FOV shape.
!
         SEP = VSEP( DIR, SVFAXI )

         IF ( SVISHP .EQ. CIRFOV ) THEN
!
!           Just compare the angular separation of POS with the
!           FOV axis direction against the FOV angular radius SVARAD.
!
            VISTAT =  SEP .LE. SVARAD


         ELSE IF ( SEP .GT. SVARAD ) THEN
!
!           The FOV is an ellipse or polygon.
!
!           The angular separation of target and FOV axis is
!           greater than the angular radius of the exclusion
!           cone. The target can't be seen.
!
            VISTAT = .FALSE.

         ELSE
!
!           The FOV is an ellipse or polygon.
!
!           The angular separation of target and FOV axis is
!           less than or equal to than the angular radius of the
!           exclusion code, so the target may be visible.
!
!           Find the intersection of the ray emanating from the
!           observer, and having direction vector POS, with the FOV
!           plane.
!
            CALL INRYPL ( SVORIG, DIR, SVPLAN, NXPTS, XPT )

!
!           If there's no intersection, the target isn't visible.
!
            IF ( NXPTS .EQ. 0 ) THEN

               VISTAT = .FALSE.

            ELSE IF ( NXPTS .NE. 1 ) THEN
!
!              "This can't happen." :)
!
               CALL SETMSG ( 'By construction, the vertex of ' &
               //            'the observer-target ray can''t ' &
               //            'lie in the FOV plane. If ' &
               //            'somehow it does, we have a serious ' &
               //            'problem.'                           )
               CALL SIGERR ( 'SPICE(BUG)'                         )
               CALL CHKOUT ( 'ZZGFFVST'                           )
               RETURN

            ELSE
!
!              NXPTS is 1.
!
!              Find the vector from the center of the FOV to XPT.
!              Call this vector FOVPT.
!
               CALL VSUB ( XPT, SVFVCT, FOVPT )

               IF ( SVISHP .EQ. ELLFOV ) THEN
!
!                 The FOV shape is elliptical. To decide whether FOVPT
!                 is within the FOV, compute the level surface
!                 parameter
!
!                                   2              2
!                    L  =  ( x / a )   +  ( y / b )
!
!                 and compare L to 1. We'll use the variable COORD
!                 to represent the coordinates (x,y).
!
!                 We've already eliminated zero divisors in the
!                 initialization routine.
!
                  DO I = 1, 2
                     COORD(I) = VDOT( FOVPT, SVSEMI(1,I) )  /  SVXMAG(I)
                  END DO

                  L =   ( COORD(1) / SVXMAG(1) )**2.D0 &
                      + ( COORD(2) / SVXMAG(2) )**2.D0

!
!                 The target is visible if FOVPT is inside the FOV
!                 ellipse; this condition is indicated by L <= 1.
!
                  VISTAT = L .LE. 1.D0


               ELSE IF ( SVISHP .EQ. POLFOV ) THEN
!
!                The FOV is a polygon. Convert FOVPT to the FOV frame,
!                then find the winding number of the FOV about the X-Y
!                projection of FOVPT.
!
                  CALL MXV ( SVFOVM, FOVPT, VTEMP )

                  PNT2D(1) = VTEMP(1)
                  PNT2D(2) = VTEMP(2)

                  W        = ZZWIND2D ( SVNVRT, SVFPOL, PNT2D )

!
!                 Any non-zero winding number indicates that the
!                 FOV polygon wraps around the point representing
!                 the intercept of the target direction with the
!                 FOV plane.
!
                  VISTAT   = W .NE. 0

               ELSE
!
!                 This is an unexpected FOV shape. We should have
!                 prevented this problem in the initialization step,
!                 but we repeat the check here for safety.
!
                  CALL SETMSG ( 'Instrument #''s FOV has shape #; ' &
                  //            'the only supported shapes are ' &
                  //            'ELLIPSE, CIRCLE, and POLYGON.'    )
                  CALL ERRCH  ( '#',  SVINAM                       )
                  CALL ERRCH  ( '#',  SVISHP                       )
                  CALL SIGERR ( 'SPICE(INVALIDSHAPE)'              )
                  CALL CHKOUT ( 'ZZGFFVST'                         )
                  RETURN

               END IF
!
!              We've performed visibility tests for elliptical or
!              polygonal FOVs. VISTAT is set.
!
            END IF
!
!           We've processed the intercept found by the INRYPL call,
!           or, if the intercept count was not 1, indicated that the
!           target is not visible. VISTAT is set.
!
         END IF
!
!        We've processed both the ray and point ephemeris object
!        cases. VISTAT is set.
!
      END IF
!
!     We've processed all target representation/FOV shape cases.
!

      CALL CHKOUT ( 'ZZGFFVST' )
      RETURN
      END
