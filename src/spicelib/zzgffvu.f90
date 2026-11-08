!$Procedure ZZGFFVU ( GF, instrument FOV utilities )

      subroutine zzgffvu ( inst,   tshape, raydir, target, &
                           tframe, abcorr, obsrvr, time,   vistat )
      use pepper_kinds, only: ip, wp

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

      implicit none

      include 'zzabcorr.inc'
      include 'zzocced.inc'
      include 'gf.inc'

      integer(ip)           ubel
      parameter           ( ubel = 9 )

      integer(ip)           ubpl
      parameter           ( ubpl = 4 )

      character*(*)         inst
      character*(*)         tshape
      real(wp)              raydir ( 3 )
      character*(*)         target
      character*(*)         tframe
      character*(*)         abcorr
      character*(*)         obsrvr
      real(wp)              time
      logical               vistat

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
      real(wp)              dpr
      real(wp)              halfpi
      real(wp)              vdot
      real(wp)              vnorm
      real(wp)              vsep

      integer(ip)           zzocced
      integer(ip)           zzwind2d

      logical               failed
      logical               return
      logical               vzero


!
!     Local parameters
!

!
!     ATOL is a tolerance value for computing FOV angular radius.
!     The angular radius must not exceed pi/2 - ATOL radians.
!
      real(wp)              atol
      parameter           ( atol   = 1.0e-6_wp )

      real(wp)              fvemax
      parameter           ( fvemax = 1.0e-5_wp )

      integer(ip)           bdnmln
      parameter           ( bdnmln = 36 )

      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

!
!     Local variables
!
      character*(frnmln)    svifrm
      character*(bdnmln)    svinam
      character*(shplen)    svishp
      character*(frnmln)    svtfrm
      character*(bdnmln)    svtnam
      character*(shplen)    svtshp
      character*(corlen)    svcorr

      real(wp)              bsite  ( 3 )
      real(wp)              coord  ( 2 )
      real(wp)              ctrext
      real(wp)              dir    ( 3 )
      real(wp)              ellrad ( 3 )
      real(wp)              escale
      real(wp)              ettarg
      real(wp)              fovpt  ( 3 )
      real(wp)              fvlimb ( ubel )
      real(wp)              insmat ( 3, 3 )
      real(wp)              l
      real(wp)              limb   ( ubel )
      real(wp)              lt
      real(wp)              m1     ( 3, 3 )
      real(wp)              m2     ( 3, 3 )
      real(wp)              obspos ( 3 )
      real(wp)              pnt2d  ( 3 )
      real(wp)              pos    ( 3 )
      real(wp)              semipt ( 3, 2 )
      real(wp)              sep
      real(wp)              stobs  ( 6 )
      real(wp)              svarad
      real(wp)              svbnds ( 3, maxvrt )
      real(wp)              svedct ( 3 )
      real(wp)              svfaxi ( 3 )
      real(wp)              svfovm ( 3, 3 )
      real(wp)              svfpol ( 2, maxvrt )
      real(wp)              svfsmx ( 3, 3 )
      real(wp)              svfvct ( 3 )
      real(wp)              svorig ( 3 )
      real(wp)              svplan ( ubpl )
      real(wp)              svrdir ( 3 )
      real(wp)              svsemi ( 3, 2 )
      real(wp)              svtrad ( 3 )
      real(wp)              svxmag ( 2 )
      real(wp)              trgctr ( 3 )
      real(wp)              trgsmx ( 3, 3 )
      real(wp)              vtemp  ( 3 )
      real(wp)              vtemp2 ( 3 )
      real(wp)              x      ( 3 )
      real(wp)              xpt    ( 3 )
      real(wp)              y      ( 3 )
      real(wp)              z      ( 3 )

      integer(ip)           clssid
      integer(ip)           framid
      integer(ip)           frcent
      integer(ip)           frclss
      integer(ip)           i
      integer(ip)           nxpts
      integer(ip)           ocstat
      integer(ip)           svinst
      integer(ip)           svnvrt
      integer(ip)           svobs
      integer(ip)           svtarg
      integer(ip)           w

      logical               attblk ( nabcor )
      logical               found
      logical               svetrg
      logical               svuray
      logical               svustl
      logical               svxmit

!
!     Saved variables
!
      save                  svarad
      save                  svbnds
      save                  svcorr
      save                  svedct
      save                  svetrg
      save                  svfaxi
      save                  svfovm
      save                  svfpol
      save                  svfsmx
      save                  svfvct
      save                  svifrm
      save                  svinam
      save                  svinst
      save                  svishp
      save                  svnvrt
      save                  svobs
      save                  svorig
      save                  svplan
      save                  svrdir
      save                  svsemi
      save                  svtarg
      save                  svtfrm
      save                  svtnam
      save                  svtrad
      save                  svtshp
      save                  svuray
      save                  svustl
      save                  svxmag
      save                  svxmit

!
!     Initial values
!
      data                  svorig / 3 * 0.0_wp /

!
!     Below we initialize the list of visibility types.
!

!
!     This routine should never be called directly.
!
      call chkin  ( 'ZZGFFVU' )

      call sigerr ( 'SPICE(BOGUSENTRY)' )

      call chkout ( 'ZZGFFVU' )
      return




!$Procedure  ZZGFFVIN ( GF, visibility initialization )

      entry zzgffvin ( inst,   tshape, raydir, &
                       target, tframe, abcorr, obsrvr )

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
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZGFFVIN' )

!
!     To avoid portability problems, initialize all
!     saved variables that aren't initialized via DATA
!     statements and aren't guaranteed to be initialized
!     for all cases.
!
      call cleard ( 3*maxvrt, svbnds )
      call cleard ( 3,        svedct )
      call cleard ( 3,        svfaxi )
      call cleard ( 2*maxvrt, svfpol )
      call cleard ( 9,        svfsmx )
      call cleard ( ubpl,     svplan )
      call cleard ( 3,        svrdir )
      svtarg = 0
      svtfrm = ' '
      svtnam = ' '
      call cleard ( 3,        svtrad )
      svustl = .false.
      svxmit = .false.

!
!     Find the NAIF ID for OBSRVR.
!
      call bods2c ( obsrvr, svobs, found )

      if ( .not. found ) then

         call setmsg ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         call errch  ( '#', obsrvr                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFFVIN'                                 )
         return

      end if

!
!     Process the target shape specifier here.
!
!     Save a left-justified, upper case version of the target shape
!     specifier.
!
      call ljust ( tshape, svtshp )
      call ucase ( svtshp, svtshp )

!
!     Note for maintenance programmer: these checks will
!     require modification to handle DSK-based shapes.
!
      if (      ( svtshp .ne. ptshap ) &
          .and. ( svtshp .ne. edshap ) &
          .and. ( svtshp .ne. ryshap )  ) then

         call setmsg ( 'The target shape specification, ' &
         //            '''#'', is not recognized.'       )
            call errch  ( '#', tshape                    )
         call sigerr ( 'SPICE(INVALIDSHAPE)'             )
         call chkout ( 'ZZGFFVIN'                        )
         return

      end if

!
!     We'll use the logical variable USERAY to indicate that the
!     target is modeled as ray.
!
      svuray = svtshp .eq. ryshap

!
!     Indicate whether we have an ellipsoidal target. SVETRG is .TRUE.
!     if and only we have one.
!
      svetrg = svtshp .eq. edshap

!
!     If the target is an ephemeris object, obtain its ID code.
!     Save the target object's name, if applicable.
!
      if ( .not. svuray ) then

         call bods2c ( target, svtarg, found )

         if ( .not. found ) then

            call setmsg ( 'The target object, ' &
            //            '''#'', is not a recognized name for an ' &
            //            'ephemeris object. The cause of this ' &
            //            'problem may be that you need an updated ' &
            //            'version of the SPICE Toolkit. '           )
            call errch  ( '#', target                                )
            call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
            call chkout ( 'ZZGFFVIN'                                 )
            return

         end if

!
!        Save the target's name.
!
         svtnam = target

!
!        Make sure the observer and target are distinct.
!
         if ( svtarg .eq. svobs ) then

            call setmsg ( 'The observer and target must be ' &
            //            'distinct objects, but are not: ' &
            //            'OBSRVR = #; TARGET = #;'         )
            call errch  ( '#', obsrvr                       )
            call errch  ( '#', target                       )
            call sigerr ( 'SPICE(BODIESNOTDISTINCT)'        )
            call chkout ( 'ZZGFFVIN'                        )
            return

         end if

      end if

!
!     Process the target frame. The target frame is defined except
!     when the target is an ephemeris object modeled as a point.
!
      if ( svuray .or. svetrg ) then
!
!        We'll use the target frame argument. Look up the target
!        frame's ID code. But first, check for a blank frame name,
!        since this may be a common problem for the GF FOV system.
!
         if ( tframe .eq. ' ' ) then

            call setmsg ( 'The target is not modeled as a ' &
            //            'point, but the associated ' &
            //            'frame name is blank.'           )
            call sigerr ( 'SPICE(INVALIDFRAME)'            )
            call chkout ( 'ZZGFFVIN'                       )
            return

         end if

         call namfrm ( tframe, framid )

         if ( framid .eq. 0 ) then

            call setmsg ( 'The target frame name # is not recognized.' )
            call errch  ( '#',  tframe                                 )
            call sigerr ( 'SPICE(INVALIDFRAME)'                        )
            call chkout ( 'ZZGFFVIN'                                   )
            return

         end if

!
!        Save the target frame name.
!
         call ljust ( tframe, svtfrm )
         call ucase ( svtfrm, svtfrm )

!
!        Obtain the center of the frame. If the target is an ephemeris
!        object, we must verify the frame center is the target.
!
         call frinfo ( framid, frcent, frclss, clssid, found )

         if ( .not. found ) then
!
!           Since we mapped the frame name to an ID code, we expect to
!           find the frame info. Getting here may be a sign of an
!           invalid frame kernel.
!
            call setmsg ( 'Frame ID found for # body-fixed ' &
            //            'frame # but FRINFO couldn''t find frame ' &
            //            'info. This may be due to a frame kernel ' &
            //            'error.'                                  )
            call errch  ( '#',  target                              )
            call sigerr ( 'SPICE(FRAMEINFONOTFOUND)'                )
            call chkout ( 'ZZGFFVIN'                                )
            return

         end if

         if ( svetrg ) then
!
!           We have an ellipsoidal target. Check the target frame's
!           center.
!
            if ( frcent .ne. svtarg ) then
!
!              The supposed body-fixed frame for the target isn't
!              actually centered on the target.
!
               call setmsg ( 'Supposed body-fixed frame # for ' &
               //            'target # is actually centered ' &
               //            'on body #.'                       )
               call errch  ( '#',  tframe                       )
               call errch  ( '#',  target                       )
               call errint ( '#',  frcent                       )
               call sigerr ( 'SPICE(INVALIDFRAME)'              )
               call chkout ( 'ZZGFFVIN'                         )
               return

            end if

         end if

      end if

!
!     Process the aberration correction specifier.
!
      if ( svuray ) then
!
!        The target is represented by a ray. Check and save the
!        aberration correction.
!
         call zzprscor ( abcorr, attblk )

         if ( failed() ) then
            call chkout ( 'ZZGFFVIN' )
            return
         end if

!
!        Reject aberration correction flags calling for any type of
!        light time correction. However, stellar aberration corrections
!        are allowed: note this is the reverse of the situation for
!        ephemeris objects. The allowed aberration correction flags are
!
!           'NONE', 'S', 'XS'
!
         if ( attblk(ltidx) ) then

            call setmsg ( 'Aberration correction flag # calls for ' &
            //            'light time corrections; these are not ' &
            //            'supported for targets represented by ' &
            //            'rays.'                                  )
            call errch  ( '#', abcorr                              )
            call sigerr ( 'SPICE(INVALIDOPTION)'                   )
            call chkout ( 'ZZGFFVIN'                               )
            return

         end if

!
!        Save flags indicating whether to use stellar aberration
!        corrections and indicating the sense of radiation travel.
!
         svustl = attblk(stlidx)
         svxmit = attblk(xmtidx)


      else
!
!        The target is an ephemeris object.
!
!        Check the aberration correction. If SPKEZR can't handle it,
!        neither can we.
!
         call zzvalcor ( abcorr, attblk )

         if ( failed() ) then
            call chkout ( 'ZZGFFVIN' )
            return
         end if

      end if

!
!     Remove all spaces from ABCORR then convert to uppercase. Save
!     this version of the aberration correction specifier.
!
      call cmprss ( ' ', 0, abcorr, svcorr )
      call ucase ( svcorr, svcorr )

!
!     Process the target body's radii, if applicable.
!
      if ( svetrg ) then

!
!        Fetch and check the radii.
!
         call zzgftreb ( svtarg, svtrad )

         if ( failed() ) then
            call chkout ( 'ZZGFFVIN' )
            return
         end if

!
!        Checks of radii have been completed.
!
      else
!
!        We don't have an ellipsoidal target body: zero out radius
!        values for this target.
!
         call cleard ( 3, svtrad )

      end if

!
!     Check the direction vector, if applicable.
!
      if ( svuray ) then
!
!        Make sure the direction vector is non-zero. Save a unit-length
!        copy of the vector.
!
         if ( vzero( raydir ) ) then

            call setmsg ( 'Input ray direction was the zero vector; ' &
            //            'this vector must be non-zero.'            )
            call sigerr ( 'SPICE(ZEROVECTOR)'                        )
            call chkout ( 'ZZGFFVIN'                                 )
            return

         end if

         call vhat ( raydir, svrdir )

      end if

!
!     Look up the instrument's ID code.
!
      call bods2c ( inst, svinst, found )

      if ( .not. found ) then

         call setmsg ( '''#'' is not a recognized name for an ' &
         //            'instrument. The cause of this ' &
         //            'problem may be that you have not loaded ' &
         //            'a required frame kernel or instrument ' &
         //            'kernel.'                                  )
         call errch  ( '#', inst                                  )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFFVIN'                                 )
         return

      end if

!
!     Save the instrument's name.
!
      call ljust ( inst,   svinam )
      call ucase ( svinam, svinam )

!
!     Look up the instrument parameters.
!
      call getfov ( svinst, maxvrt, svishp, &
                    svifrm, bsite,  svnvrt, svbnds )

      if ( failed() ) then
         call chkout ( 'ZZGFFVIN' )
         return
      end if

!
!     Scale boresight vector to unit length.
!
      call vhatip( bsite )

!
!     Make sure the instrument shape specifier is left-justified
!     and in upper case.
!
      call ljust ( svishp, svishp )
      call ucase ( svishp, svishp )

!
!     If the instrument's shape is 'RECTANGLE', map it to
!     'POLYGON'
!
      if (  svishp .eq. recfov ) then

         svishp = polfov

      end if

!
!     Save an axis vector for the FOV. For circular and ellipsoidal
!     FOVs, the boresight serves as this axis. For polygonal FOVs
!     (rectangular FOVs are included), we'll generate an axis vector.
!
      if ( svishp .eq. polfov ) then

         call zzfovaxi ( inst,  svnvrt, svbnds, svfaxi )

         if ( failed() ) then
            call chkout ( 'ZZGFFVIN' )
            return
         end if

      else

         call vequ ( bsite, svfaxi )

      end if

!
!     Check the angular radius of the FOV.
!
!     Compute the angular radius of the FOV. We'll use this to define a
!     "bounding cone" centered on the FOV axis and having its apex at
!     the observer. This cone will be used for a preliminary FOV
!     exclusion test.
!
      svarad = 0.0_wp

      do i = 1, svnvrt
         svarad = max (  svarad,  vsep( svbnds(1,i), svfaxi )  )
      end do

!
!     Our algorithms can't handle FOVs with angular radius of 90
!     degrees.
!
      if (  svarad   .gt. ( halfpi()-atol )  ) then

         call setmsg ( 'FOV angular radius of # degrees ' &
         //            'exceeds limit of # degrees.'     )
         call errdp  ( '#',  svarad*dpr()                )
         call errdp  ( '#',  (halfpi()-atol)*dpr()       )
         call sigerr ( 'SPICE(FOVTOOWIDE)'               )
         call chkout ( 'ZZGFFVIN'                        )
         return

      end if

!
!     Convert the FOV shape specifier to a left-justified, upper
!     case form.
!
      call ljust ( svishp, svishp )
      call ucase ( svishp, svishp )

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
      call nvc2pl ( svfaxi, 1.0_wp, svplan )

!
!     Find the point on the plane closest to the origin. This is
!     the center of the FOV.
!
      call vhat ( svfaxi, svfvct )

!
!     If applicable, perform the computations required for an
!     elliptical FOV, where the target representation is arbitrary, or
!     a circular FOV when the target is an ellipsoid. Note that
!     these computations are not needed for the combination of a
!     circular FOV and a point or ray target.
!
      if (        ( svishp .eq. ellfov ) &
           .or. ( ( svishp .eq. cirfov ) .and. svetrg )  ) then
!
!        Also compute the center, semi-axis vectors, and semi-axis
!        lengths of the FOV. If the FOV is circular, we create an
!        artificial, second semi-axis vector.
!
         if ( svishp .eq. cirfov ) then
!
!           We have a circular FOV. We'll create an artificial, second
!           boundary vector, which will give rise to a second
!           semi-axis.
!
            call vrotv ( svbnds(1,1), svfaxi, halfpi(), svbnds(1,2) )

         end if

!
!        Now find the endpoints of the semi-axes in this plane.
!
         do i = 1, 2

            call inrypl ( svorig, svbnds(1,i), &
                          svplan, nxpts,       semipt(1,i) )

            if ( nxpts .ne. 1 ) then

               call setmsg ( 'Error creating FOV semi-axis ' &
               //            'vectors, NXPTS = #. This ' &
               //            'may indicate an error in the ' &
               //            'IK parameters for #.'         )
               call errint ( '#', nxpts                     )
               call errch  ( '#', inst                      )
               call sigerr ( 'SPICE(DEGENERATECASE)'        )
               call chkout ( 'ZZGFFVIN'                     )
               return

            end if

!
!           Compute and find the length of each semi-axis vector.
!
            call vsub ( semipt(1,i), svfvct, svsemi(1,i) )

            svxmag(i) = vnorm ( svsemi(1,i) )

            if ( svxmag(i) .eq. 0.0_wp ) then

               call setmsg ( 'FOV semi-axis #* for @ ' &
               //            'has zero length.'        )
               call errint ( '*', i                    )
               call errch  ( '@', inst                 )
               call sigerr ( 'SPICE(DEGENERATECASE)'   )
               call chkout ( 'ZZGFFVIN'                )
               return

            end if

         end do

      end if

!
!     If we have an ellipsoidal target, and the FOV is circular or
!     elliptical, we'll create an ellipsoid whose limb coincides with
!     the FOV. This allows use to later use ZZOCCED to determine the
!     target's visibility.
!
      if (   (      ( svishp .eq. cirfov ) &
               .or. ( svishp .eq. ellfov ) )  .and.  svetrg  ) then
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
         ctrext = svxmag(1) ** 2

!
!        The ellipsoid's center is SVEDCT.
!
         call vscl ( 1.0_wp + ctrext, svfvct, svedct )

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
         ellrad(3) = svxmag(1) * sqrt ( 1.0_wp + svxmag(1)**2.0_wp )
         ellrad(1) = ellrad(3)

!
!        Compute the corresponding columns of the FOV semi-axis matrix.
!
!        The ellipsoid's third axis points along the FOV axis. Note
!        that SVFVCT is a unit vector pointing in the desired
!        direction.
!
         call vscl ( ellrad(3), svfvct, svfsmx(1,3) )

!
!        The first ellipsoid semi-axis is associated with SEMIPT(*,1)
!        and also has length ELLRAD(3):
!
         call vhat ( svsemi(1,1), vtemp )
         call vscl ( ellrad(1),   vtemp,  svfsmx(1,1) )

!
!        The ellipsoid's second semi-axis points from SVFVCT toward
!        SEMIPT(*,2). The ratio of its length to that of the other
!        semi-axis is the ratio of the length of the FOV's second
!        semi-axis to that of its first. Note that we've already ruled
!        out divide-by-zero errors here.
!
         ellrad(2) = ( svxmag(2)/svxmag(1) ) * ellrad(3)

!
!        We define the third axis using a cross product to
!        ensure we produce a matrix with positive determinant.
!
         call ucrss  ( svfsmx(1,3), svfsmx(1,1), vtemp )
         call vscl   ( ellrad(2),   vtemp,       svfsmx(1,2) )

!
!        Scale the ellipsoid and the distance of its center from
!        the observer so that the ellipsoid is of reasonable size
!        and doesn't extend too far from the observer. Caution: this
!        modification means the ellipsoid no longer intersects the FOV
!        plane at the FOV boundary. The ellipsoid is still usable with
!        ZZOCCED, which is the ellipsoid's raison d'etre.
!
         escale = fvemax/ ( vnorm(svedct) + max(ellrad(1), ellrad(2)) )

         do i = 1, 3
            call vsclip ( escale, svfsmx(1,i) )
         end do

         call vsclip ( escale, svedct )

      end if


      if ( ( svishp .eq. cirfov ) .and. ( .not. svetrg ) ) then
!
!        We have a circular FOV and a point or ray target model.
!        In this case, our FOV inclusion test is simple as can
!        be: we just compare the angular separation of the
!        target and FOV axis against the angular radius of the
!        FOV. Compute and save this angular radius.
!
         svarad = vsep ( svfaxi, svbnds(1,1) )


      else if (       (      ( svishp .eq. recfov ) &
                        .or. ( svishp .eq. polfov ) ) &
                .and. ( .not.  svetrg               )   ) then
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
         call vequ ( svfaxi, z )

         call frame ( z, x, y )

         do i = 1, 3
            svfovm(1,i) = x(i)
            svfovm(2,i) = y(i)
            svfovm(3,i) = z(i)
         end do

!
!        Compute the intersections of the FOV boundary vectors with the
!        FOV plane. For each intercept, find the vector pointing from
!        the FOV center to that intercept. Transform each such
!        difference vector into the FOV frame. Save the projection onto
!        the FOV frame's X-Y plane.
!
         do i = 1, svnvrt

            call inrypl ( svorig, svbnds(1,i), &
                          svplan, nxpts,       xpt)

            if ( nxpts .ne. 1 ) then

               call setmsg ( 'Error finding FOV plane intercept ' &
               //            'of FOV boundary vector #, NXPTS = #. ' &
               //            'This may indicate an error ' &
               //            'in the IK parameters for #.'          )
               call errint ( '#', i                                 )
               call errint ( '#', nxpts                             )
               call errch  ( '#', inst                              )
               call sigerr ( 'SPICE(DEGENERATECASE)'                )
               call chkout ( 'ZZGFFVIN'                             )
               return

            end if

            call vsub ( xpt,   svfvct, vtemp  )
            call mxv ( svfovm, vtemp,  vtemp2 )

            svfpol(1,i) = vtemp2(1)
            svfpol(2,i) = vtemp2(2)

         end do

      end if

      call chkout ( 'ZZGFFVIN' )
      return



!$Procedure ZZGFFVST ( GF, "is target in FOV?"  )

      entry zzgffvst ( time, vistat )

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
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFFVST' )

!
!     Initialize the state output.
!
      vistat = .false.

!
!     The algorithm for the state determination depends on the
!     target model and the FOV shape.
!
      if ( svetrg ) then
!
!        The target is an ephemeris object modeled as an ellipsoid.
!        There are two branches here: one for a rectangular/
!        polygonal FOV and one for a circular/elliptical FOV.
!
!        Start by finding the observer-target position vector in the
!        target body-fixed frame.
!
         call spkezp ( svtarg, time, svtfrm, svcorr, svobs, pos, lt )

!
!        Compute the target epoch.
!
         call zzcorepc ( svcorr, time, lt, ettarg )

!
!        Find the transformation from the target frame at ETTARG to the
!        instrument frame at TIME. We'll need to use J2000 as an
!        intermediate frame.
!
         call pxform ( svtfrm,  'J2000', ettarg, m1 )
         call pxform ( 'J2000', svifrm,  time,   m2 )

         if ( failed() ) then
            call chkout ( 'ZZGFFVST' )
            return
         end if

         call mxm ( m2, m1, insmat )


         if ( ( svishp .eq. recfov ) .or. ( svishp .eq. polfov ) ) then
!
!           The FOV is a rectangle or other polygon; we treat both
!           cases the same way.
!
!           Negate POS to obtain the position of the observer with
!           respect to the target.
!
            call vminus ( pos, obspos )

!
!           Find the limb in the target body-fixed frame.
!
            call edlimb ( svtrad(1), svtrad(2), svtrad(3), &
                          obspos,    limb                  )

!
!           Transform the limb from the target frame at ETTARG
!           to the instrument frame at TIME. The matrix INSMAT
!           effects just this transformation. We unpack the center
!           and semi-axis vectors of LIMB, transform them, and
!           pack the results into FVLIMB. Below, M1 and M2 are
!           simply temporary 3x3 matrices.
!
            call el2cgv ( limb, m1(1,1), m1(1,2), m1(1,3) )

!
!           Before performing the frame transformation on the
!           limb's center, translate the center so that the
!           observer is at the origin. Since POS is expressed
!           in the target body-fixed frame, this is a convenient
!           place for the translation.
!
            call vadd ( pos,   m1(1,1), vtemp )
            call vequ ( vtemp, m1(1,1)        )

            do i = 1, 3
               call mxv ( insmat, m1(1,i), m2(1,i) )
            end do

            call cgv2el (  m2(1,1), m2(1,2), m2(1,3), fvlimb )

!
!           All geometric objects in the following call are expressed
!           in the instrument reference frame.
!
!           The target is in the FOV if and only if ZZELVUPY finds an
!           intersection, so we use VISTAT as the "found" flag.
!
            call zzelvupy ( fvlimb, svorig, svfaxi, &
                            svnvrt, svbnds, vistat )


         else if (      ( svishp .eq. cirfov ) &
                   .or. ( svishp .eq. ellfov )  ) then
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
            call mxv ( insmat, pos, trgctr )

!
!           The columns of INSMAT are the target body's semi-axis
!           direction vectors; we scale these by the target radii
!           to obtain the semi-axis matrix for the target.
!
            do i = 1, 3
               call vscl ( svtrad(i), insmat(1,i), trgsmx(1,i) )
            end do

            ocstat = zzocced ( svorig, svedct, svfsmx, trgctr, trgsmx )

!
!           A return code of zero indicates no occultation. Any other
!           return code indicates a non-empty intersection of the
!           target and FOV.
!
            vistat = ocstat .ne. 0

         else
!
!           This is an unexpected FOV shape. We should have prevented
!           this problem in the initialization step, but we repeat the
!           check here for safety.
!
            call setmsg ( 'The target body # has shape #; the only ' &
            //            'supported shapes are ELLIPSOID, POINT, ' &
            //            'and RAY.'                                )
            call errch  ( '#',  svtnam                              )
            call errch  ( '#',  svishp                              )
            call sigerr ( 'SPICE(INVALIDSHAPE)'                     )
            call chkout ( 'ZZGFFVST'                                )
            return

         end if

!
!        This is the end of the ellipsoidal target case. At this
!        point, VISTAT is set.
!

      else
!
!        The target is a ray or an ephemeris object modeled as a point.
!        In either case, we want to obtain the aberration-corrected
!        observer-target vector.
!
         if ( svuray ) then
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
            call pxform ( svtfrm, svifrm, time, insmat )

            if ( failed() ) then
               call chkout ( 'ZZGFFVST' )
               return
            end if

!
!           Transform the ray's direction vector to the instrument
!           frame.
!
            call mxv ( insmat, svrdir, dir )

!
!           If we need to correct the ray's direction for stellar
!           aberration, do it now.
!
            if ( svustl ) then
!
!              Find the state of the observer relative to the
!              solar system barycenter in the J2000 frame.
!
               call spkssb ( svobs, time, 'J2000', stobs )

!
!              Convert the direction vector to the J2000 frame.
!
               call pxform ( svifrm, 'J2000', time, m1 )

               if ( failed() ) then
                  call chkout ( 'ZZGFFVST' )
                  return
               end if

               call mxv ( m1, dir, vtemp )

!
!              Apply the stellar aberration correction.
!
               if ( svxmit ) then
!
!                 Use the transmission correction.
!
                  call stlabx ( vtemp, stobs(4), vtemp2 )
               else
                  call stelab ( vtemp, stobs(4), vtemp2 )
               end if

!
!              Map the direction vector back to the instrument
!              frame.
!
               call mtxv ( m1, vtemp2, dir )

            end if
!
!           The target direction in the instrument frame DIR has
!           been computed.
!

         else
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
            call spkezp ( svtarg,  time,   'J2000', &
                          svcorr,  svobs,  vtemp, lt )

            call pxform ( 'J2000', svifrm, time,  m1 )

            if ( failed() ) then
               call chkout ( 'ZZGFFVST' )
               return
            end if

            call mxv ( m1, vtemp, dir )

         end if

         if ( failed() ) then
            call chkout ( 'ZZGFFVST' )
            return
         end if

!
!        The observer-target direction vector DIR is set.
!
!        The determination of whether the ray is in the FOV depends
!        on the FOV shape.
!
         sep = vsep( dir, svfaxi )

         if ( svishp .eq. cirfov ) then
!
!           Just compare the angular separation of POS with the
!           FOV axis direction against the FOV angular radius SVARAD.
!
            vistat =  sep .le. svarad


         else if ( sep .gt. svarad ) then
!
!           The FOV is an ellipse or polygon.
!
!           The angular separation of target and FOV axis is
!           greater than the angular radius of the exclusion
!           cone. The target can't be seen.
!
            vistat = .false.

         else
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
            call inrypl ( svorig, dir, svplan, nxpts, xpt )

!
!           If there's no intersection, the target isn't visible.
!
            if ( nxpts .eq. 0 ) then

               vistat = .false.

            else if ( nxpts .ne. 1 ) then
!
!              "This can't happen." :)
!
               call setmsg ( 'By construction, the vertex of ' &
               //            'the observer-target ray can''t ' &
               //            'lie in the FOV plane. If ' &
               //            'somehow it does, we have a serious ' &
               //            'problem.'                           )
               call sigerr ( 'SPICE(BUG)'                         )
               call chkout ( 'ZZGFFVST'                           )
               return

            else
!
!              NXPTS is 1.
!
!              Find the vector from the center of the FOV to XPT.
!              Call this vector FOVPT.
!
               call vsub ( xpt, svfvct, fovpt )

               if ( svishp .eq. ellfov ) then
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
                  do i = 1, 2
                     coord(i) = vdot( fovpt, svsemi(1,i) )  /  svxmag(i)
                  end do

                  l =   ( coord(1) / svxmag(1) )**2.0_wp &
                      + ( coord(2) / svxmag(2) )**2.0_wp

!
!                 The target is visible if FOVPT is inside the FOV
!                 ellipse; this condition is indicated by L <= 1.
!
                  vistat = l .le. 1.0_wp


               else if ( svishp .eq. polfov ) then
!
!                The FOV is a polygon. Convert FOVPT to the FOV frame,
!                then find the winding number of the FOV about the X-Y
!                projection of FOVPT.
!
                  call mxv ( svfovm, fovpt, vtemp )

                  pnt2d(1) = vtemp(1)
                  pnt2d(2) = vtemp(2)

                  w        = zzwind2d ( svnvrt, svfpol, pnt2d )

!
!                 Any non-zero winding number indicates that the
!                 FOV polygon wraps around the point representing
!                 the intercept of the target direction with the
!                 FOV plane.
!
                  vistat   = w .ne. 0

               else
!
!                 This is an unexpected FOV shape. We should have
!                 prevented this problem in the initialization step,
!                 but we repeat the check here for safety.
!
                  call setmsg ( 'Instrument #''s FOV has shape #; ' &
                  //            'the only supported shapes are ' &
                  //            'ELLIPSE, CIRCLE, and POLYGON.'    )
                  call errch  ( '#',  svinam                       )
                  call errch  ( '#',  svishp                       )
                  call sigerr ( 'SPICE(INVALIDSHAPE)'              )
                  call chkout ( 'ZZGFFVST'                         )
                  return

               end if
!
!              We've performed visibility tests for elliptical or
!              polygonal FOVs. VISTAT is set.
!
            end if
!
!           We've processed the intercept found by the INRYPL call,
!           or, if the intercept count was not 1, indicated that the
!           target is not visible. VISTAT is set.
!
         end if
!
!        We've processed both the ray and point ephemeris object
!        cases. VISTAT is set.
!
      end if
!
!     We've processed all target representation/FOV shape cases.
!

      call chkout ( 'ZZGFFVST' )
      return
      end
