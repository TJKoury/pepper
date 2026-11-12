!$Procedure T_ILUMIN ( Illumination angle test utility )

      SUBROUTINE T_ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                            ABCORR, OBSRVR, SPOINT, TRGEPC, &
                            SRFVEC, PHASE,  SOLAR,  EMISSN  )
      use pepper_legacy
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Find the illumination angles (phase, solar incidence, and
!     emission) at a specified surface point of a target body.
!
!     This routine provides an illumination angle computation
!     independent of those performed in ILUMIN, GFILUM, and
!     ZZILUMSTA.
!
! $ Required_Reading
!
!     FRAMES
!     NAIF_IDS
!     PCK
!     SPK
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     MOSPICE
!
! $ Declarations

      IMPLICIT NONE


      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      SOLAR
      DOUBLE PRECISION      EMISSN

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TARGET     I   Name of target body.
!     ET         I   Epoch in ephemeris seconds past J2000 TDB.
!     FIXREF     I   Body-fixed, body-centered target body frame.
!     ABCORR     I   Desired aberration correction.
!     OBSRVR     I   Name of observing body.
!     SPOINT     I   Body-fixed coordinates of a target surface point.
!     TRGEPC     O   Target surface point epoch.
!     SRFVEC     O   Vector from observer to target surface point.
!     PHASE      O   Phase angle at the surface point.
!     SOLAR      O   Solar incidence angle at the surface point.
!     EMISSN     O   Emission angle at the surface point.
!
! $ Detailed_Input
!
!
!     METHOD      is a short string providing parameters defining
!                 the computation method to be used. Parameters
!                 include, but are not limited to, the shape model
!                 used to represent the surface of the target body.
!
!                 The only choice currently supported is
!
!                    'Ellipsoid'        The illumination angle
!                                       computation uses a triaxial
!                                       ellipsoid to model the surface
!                                       of the target body. The
!                                       ellipsoid's radii must be
!                                       available in the kernel pool.
!
!                 Neither case nor white space are significant in
!                 METHOD. For example, the string ' eLLipsoid ' is
!                 valid.
!
!
!     TARGET      is the name of the target body. TARGET is
!                 case-insensitive, and leading and trailing blanks in
!                 TARGET are not significant. Optionally, you may
!                 supply a string containing the integer ID code for
!                 the object. For example both 'MOON' and '301' are
!                 legitimate strings that indicate the Moon is the
!                 target body.
!
!     ET          is the epoch, expressed as seconds past J2000 TDB,
!                 for which the apparent illumination angles at the
!                 specified surface point on the target body, as seen
!                 from the observing body, are to be computed.
!
!
!     FIXREF      is the name of the body-fixed, body-centered
!                 reference frame associated with the target body. The
!                 input surface point SPOINT and the output vector
!                 SRFVEC are expressed relative to this reference
!                 frame. The string FIXREF is case-insensitive, and
!                 leading and trailing blanks in FIXREF are not
!                 significant.
!
!
!     ABCORR      is the aberration correction to be used in computing
!                 the position and orientation of the target body and
!                 the location of the Sun.
!
!                 For remote sensing applications, where the apparent
!                 illumination angles seen by the observer are desired,
!                 normally either of the corrections
!
!                    'LT+S'
!                    'CN+S'
!
!                 should be used. These and the other supported options
!                 are described below. ABCORR may be any of the
!                 following:
!
!                    'NONE'     No aberration correction.
!
!                 Let LT represent the one-way light time between the
!                 observer and SPOINT (note: NOT between the observer
!                 and the target body's center). The following values
!                 of ABCORR apply to the "reception" case in which
!                 photons depart from SPOINT at the light-time
!                 corrected epoch ET-LT and *arrive* at the observer's
!                 location at ET:
!
!                    'LT'       Correct both the position of SPOINT as
!                               seen by the observer, and the position
!                               of the Sun as seen by the target, for
!                               light time.
!
!                    'LT+S'     Correct both the position of SPOINT as
!                               seen by the observer, and the position
!                               of the Sun as seen by the target, for
!                               light time and stellar aberration.
!
!                    'CN'       Converged Newtonian light time
!                               correction. In solving the light time
!                               equations for target and the Sun, the
!                               "CN" correction iterates until the
!                               solution converges.
!
!                    'CN+S'     Converged Newtonian light time and
!                               stellar aberration corrections. This
!                               option produces a solution that is at
!                               least as accurate at that obtainable
!                               with the 'LT+S' option. Whether the
!                               'CN+S' solution is substantially more
!                               accurate depends on the geometry of the
!                               participating objects and on the
!                               accuracy of the input data. In all
!                               cases this routine will execute more
!                               slowly when a converged solution is
!                               computed.
!
!                 Neither case nor white space are significant in
!                 ABCORR. For example, the string
!
!                   'Lt + s'
!
!                 is valid.
!
!
!     OBSRVR      is the name of the observing body. The observing body
!                 is an ephemeris object: it typically is a spacecraft,
!                 the earth, or a surface point on the earth. OBSRVR is
!                 case-insensitive, and leading and trailing blanks in
!                 OBSRVR are not significant. Optionally, you may
!                 supply a string containing the integer ID code for
!                 the object. For example both 'MOON' and '301' are
!                 legitimate strings that indicate the Moon is the
!                 observer.
!
!                 OBSRVR may be not be identical to TARGET.
!
!
!     SPOINT      is a surface point on the target body, expressed in
!                 Cartesian coordinates, relative to the body-fixed
!                 target frame designated by FIXREF.
!
!                 SPOINT need not be visible from the observer's
!                 location at the epoch ET.
!
!                 The components of SPOINT have units of km.
!
!
! $ Detailed_Output
!
!
!     TRGEPC      is the "surface point epoch." TRGEPC is defined as
!                 follows: letting LT be the one-way light time between
!                 the observer and the input surface point SPOINT,
!                 TRGEPC is either the epoch ET-LT or ET depending on
!                 whether the requested aberration correction is,
!                 respectively, for received radiation or omitted. LT
!                 is computed using the method indicated by ABCORR.
!
!                 TRGEPC is expressed as seconds past J2000 TDB.
!
!
!     SRFVEC      is the vector from the observer's position at ET to
!                 the aberration-corrected (or optionally, geometric)
!                 position of SPOINT, where the aberration corrections
!                 are specified by ABCORR. SRFVEC is expressed in the
!                 target body-fixed reference frame designated by
!                 FIXREF, evaluated at TRGEPC.
!
!                 The components of SRFVEC are given in units of km.
!
!                 One can use the SPICELIB function VNORM to obtain the
!                 distance between the observer and SPOINT:
!
!                    DIST = VNORM ( SRFVEC )
!
!                 The observer's position OBSPOS, relative to the
!                 target body's center, where the center's position is
!                 corrected for aberration effects as indicated by
!                 ABCORR, can be computed via the call:
!
!                    CALL VSUB ( SPOINT, SRFVEC, OBSPOS )
!
!                 To transform the vector SRFVEC to a time-dependent
!                 reference frame REF at ET, a sequence of two frame
!                 transformations is required. For example, let MFIX
!                 and MREF be 3x3 matrices respectively describing the
!                 target body-fixed to J2000 frame transformation at
!                 TRGEPC and the J2000 to (time-dependent frame) REF
!                 transformation at ET, and let XFORM be the 3x3 matrix
!                 representing the composition of MREF with MFIX. Then
!                 SRFVEC can be transformed to the result REFVEC as
!                 follows:
!
!                     CALL PXFORM ( FIXREF,  'J2000', TRGEPC, MFIX   )
!                     CALL PXFORM ( 'J2000', REF,     ET,     MREF   )
!                     CALL MXM    ( MREF,    MFIX,            XFORM  )
!                     CALL MXV    ( XFORM,   SRFVEC,          REFVEC )
!
!
!     PHASE       is the phase angle at SPOINT, as seen from OBSRVR at
!                 time ET. This is the angle between the negative of
!                 the vector SRFVEC and the SPOINT-Sun vector at
!                 TRGEPC. Units are radians. The range of PHASE is
!                 [0, pi].
!
!     SOLAR       is the solar incidence angle at SPOINT, as seen from
!                 OBSRVR at time ET. This is the angle between the
!                 surface normal vector at SPOINT and the SPOINT-Sun
!                 vector at TRGEPC. Units are radians. The range of
!                 SOLAR is [0, pi].
!
!     EMISSN      is the emission angle at SPOINT, as seen from OBSRVR
!                 at time ET. This is the angle between the surface
!                 normal vector at SPOINT and the negative of the
!                 vector SRFVEC. Units are radians. The range of EMISSN
!                 is [0, pi].
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!
!     1)  If the specified aberration correction is relativistic or
!         calls for stellar aberration but not light time correction,
!         the error SPICE(NOTSUPPORTED) is signaled. If the specified
!         aberration correction is any other unrecognized value, the
!         error will be diagnosed and signaled by a routine in the call
!         tree of this routine.
!
!     2)  If either the target or observer input strings cannot be
!         converted to an integer ID code, the error
!         SPICE(IDCODENOTFOUND) is signaled.
!
!     3)  If OBSRVR and TARGET map to the same NAIF integer ID code,
!         the error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     4)  If the input target body-fixed frame FIXREF is not
!         recognized, the error SPICE(NOFRAME) is signaled. A frame
!         name may fail to be recognized because a required frame
!         specification kernel has not been loaded; another cause is a
!         misspelling of the frame name.
!
!     5)  If the input frame FIXREF is not centered at the target body,
!         the error SPICE(INVALIDFRAME) is signaled.
!
!     6)  If the input argument METHOD is not recognized, the error
!         SPICE(INVALIDMETHOD) is signaled.
!
!     7)  If the target and observer have distinct identities but are
!         at the same location (for example, the target is Mars and the
!         observer is the Mars barycenter), the error
!         SPICE(NOSEPARATION) is signaled.
!
!     8)  If insufficient ephemeris data have been loaded prior to
!         calling T_ILUMIN, the error will be diagnosed and signaled by
!         a routine in the call tree of this routine. Note that when
!         light time correction is used, sufficient ephemeris data must
!         be available to propagate the states of observer, target, and
!         the Sun to the solar system barycenter.
!
!     9)  If the computation method specifies an ellipsoidal target
!         shape and triaxial radii of the target body have not been
!         loaded into the kernel pool prior to calling T_ILUMIN, the
!         error will be diagnosed and signaled by a routine in the call
!         tree of this routine.
!
!     10) The target must be an extended body: if any of the radii of
!         the target body are non-positive, the error will be
!         diagnosed and signaled by routines in the call tree of this
!         routine.
!
!     11) If PCK data specifying the target body-fixed frame
!         orientation have not been loaded prior to calling T_ILUMIN,
!         the error will be diagnosed and signaled by a routine in the
!         call tree of this routine.
!
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - SPK data: ephemeris data for target, observer, and the
!          Sun must be loaded. If aberration corrections are used, the
!          states of target, observer, and the Sun relative to the
!          solar system barycenter must be calculable from the
!          available ephemeris data. Typically ephemeris data are made
!          available by loading one or more SPK files via FURNSH.
!
!        - PCK data: if the target body shape is modeled as an
!          ellipsoid, triaxial radii for the target body must be loaded
!          into the kernel pool. Typically this is done by loading a
!          text PCK file via FURNSH.
!
!        - Further PCK data: rotation data for the target body must be
!          loaded. These may be provided in a text or binary PCK file.
!
!        - Frame data: if a frame definition is required to convert the
!          observer and target states to the body-fixed frame of the
!          target, that definition must be available in the kernel
!          pool. Typically the definition is supplied by loading a
!          frame kernel via FURNSH.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
!
! $ Particulars
!
!
!     The term "illumination angles" refers to following set of
!     angles:
!
!
!        phase angle              Angle between the vectors from the
!                                 surface point to the observer and
!                                 from the surface point to the Sun.
!
!        solar incidence angle    Angle between the surface normal at
!                                 the specified surface point and the
!                                 vector from the surface point to the
!                                 Sun.
!
!        emission angle           Angle between the surface normal at
!                                 the specified surface point and the
!                                 vector from the surface point to the
!                                 observer.
!
!     The diagram below illustrates the geometric relationships
!     defining these angles. The labels for the solar incidence,
!     emission, and phase angles are "s.i.", "e.", and "phase".
!
!
!                                                      *
!                                                     Sun
!
!                    surface normal vector
!                              ._                 _.
!                              |\                 /|  Sun vector
!                                \    phase      /
!                                 \   .    .    /
!                                 .            .
!                                   \   ___   /
!                              .     \/     \/
!                                    _\ s.i./
!                             .    /   \   /
!                             .   |  e. \ /
!         *             <--------------- *  surface point on
!      viewing            vector            target body
!      location           to viewing
!      (observer)         location
!
!
!     Note that if the target-observer vector, the target normal vector
!     at the surface point, and the target-sun vector are coplanar,
!     then phase is the sum of incidence and emission. This is rarely
!     true; usually
!
!        phase angle  <  solar incidence angle + emission angle
!
!     All of the above angles can be computed using light time
!     corrections, light time and stellar aberration corrections, or
!     no aberration corrections. In order to describe apparent
!     geometry as observed by a remote sensing instrument, both
!     light time and stellar aberration corrections should be used.
!
!     The way aberration corrections are applied by this routine
!     is described below.
!
!        Light time corrections
!        ======================
!
!           Observer-target surface point vector
!           ------------------------------------
!
!           Let ET be the epoch at which an observation or remote
!           sensing measurement is made, and let ET - LT ("LT" stands
!           for "light time") be the epoch at which the photons
!           received at ET were emitted from the surface point SPOINT.
!           Note that the light time between the surface point and
!           observer will generally differ from the light time between
!           the target body's center and the observer.
!
!
!           Target body's orientation
!           -------------------------
!
!           Using the definitions of ET and LT above, the target body's
!           orientation at ET - LT is used. The surface normal is
!           dependent on the target body's orientation, so the body's
!           orientation model must be evaluated for the correct epoch.
!
!
!           Target body -- Sun vector
!           -------------------------
!
!           The surface features on the target body near SPOINT will
!           appear in a measurement made at ET as they were at ET-LT.
!           In particular, lighting on the target body is dependent on
!           the apparent location of the Sun as seen from the target
!           body at ET-LT. So, a second light time correction is used
!           to compute the position of the Sun relative to the surface
!           point.
!
!
!        Stellar aberration corrections
!        ==============================
!
!        Stellar aberration corrections are applied only if
!        light time corrections are applied as well.
!
!           Observer-target surface point body vector
!           -----------------------------------------
!
!           When stellar aberration correction is performed, the
!           direction vector SRFVEC is adjusted so as to point to the
!           apparent position of SPOINT: considering SPOINT to be an
!           ephemeris object, SRFVEC points from the observer's
!           position at ET to the light time and stellar aberration
!           corrected position of SPOINT.
!
!           Target body-Sun vector
!           ----------------------
!
!           The target body-Sun vector is the apparent position of the
!           Sun, corrected for light time and stellar aberration, as
!           seen from the target body at time ET-LT.
!
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Find the phase, solar incidence, and emission angles at the
!        sub-solar and sub-spacecraft points on Mars as seen from the
!        Mars Global Surveyor spacecraft at a specified UTC time. Use
!        light time and stellar aberration corrections.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!           KPL/MK
!
!           File: mgs_example.tm
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
!              File name                     Contents
!              ---------                     --------
!              de418.bsp                     Planetary ephemeris
!              pck00008.tpc                  Planet orientation and
!                                            radii
!              naif0008.tls                  Leapseconds
!              mgs_ext13_ipng_mgs95j.bsp     MGS ephemeris
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de418.bsp',
!                                  'pck00008.tpc',
!                                  'naif0008.tls',
!                                  'mgs_ext13_ipng_mgs95j.bsp'  )
!           \begintext
!
!
!        Example code begins here.
!
!           PROGRAM ANGLES
!           IMPLICIT NONE
!     C
!     C     SPICELIB functions
!     C
!           DOUBLE PRECISION      DPR
!     C
!     C     Local parameters
!     C
!           CHARACTER*(*)         META
!           PARAMETER           ( META   = 'mgs_example.tm' )
!
!           INTEGER               NAMLEN
!           PARAMETER           ( NAMLEN = 32 )
!
!           INTEGER               TIMLEN
!           PARAMETER           ( TIMLEN = 25 )
!
!           INTEGER               CORLEN
!           PARAMETER           ( CORLEN = 5 )
!     C
!     C     Local variables
!     C
!           CHARACTER*(CORLEN)    ABCORR
!           CHARACTER*(NAMLEN)    OBSRVR
!           CHARACTER*(NAMLEN)    TARGET
!           CHARACTER*(TIMLEN)    UTC
!
!           DOUBLE PRECISION      ET
!           DOUBLE PRECISION      SRFVEC ( 3 )
!           DOUBLE PRECISION      SSCEMI
!           DOUBLE PRECISION      SSCPHS
!           DOUBLE PRECISION      SSCPT  ( 3 )
!           DOUBLE PRECISION      SSCSOL
!           DOUBLE PRECISION      SSLEMI
!           DOUBLE PRECISION      SSLPHS
!           DOUBLE PRECISION      SSLSOL
!           DOUBLE PRECISION      SSOLPT ( 3 )
!           DOUBLE PRECISION      TRGEPC
!
!     C
!     C     Load kernel files.
!     C
!           CALL FURNSH ( META )
!     C
!     C     Convert the UTC request time string to seconds past
!     C     J2000 TDB.
!     C
!           UTC = '2004 JAN 1 12:00:00'
!
!           CALL UTC2ET ( UTC, ET )
!
!     C
!     C     Assign observer and target names. The acronym MGS
!     C     indicates Mars Global Surveyor. See NAIF_IDS for a
!     C     list of names recognized by SPICE. Also set the
!     C     aberration correction flag.
!     C
!           TARGET = 'Mars'
!           OBSRVR = 'MGS'
!           ABCORR = 'CN+S'
!     C
!     C     Find the sub-solar point on the Earth as seen from
!     C     the MGS spacecraft at ET. Use the "near point: ellipsoid"
!     C     style of sub-point definition. This makes it easy
!     C     to verify the solar incidence angle.
!     C
!           CALL SUBSLR ( 'Near point: ellipsoid',
!          .              TARGET,  ET,      'IAU_MARS',
!          .              ABCORR,  OBSRVR,  SSOLPT, TRGEPC, SRFVEC )
!     C
!     C     Now find the sub-spacecraft point.
!     C
!           CALL SUBPNT ( 'Near point: ellipsoid',
!          .              TARGET,  ET,     'IAU_MARS',
!          .              ABCORR,  OBSRVR, SSCPT,   TRGEPC, SRFVEC )
!     C
!     C     Find the phase, solar incidence, and emission
!     C     angles at the sub-solar point on the Earth as seen
!     C     from MGS at time ET.
!     C
!           CALL T_ILUMIN ( 'Ellipsoid', TARGET, ET,     'IAU_MARS',
!          .              ABCORR,      OBSRVR, SSOLPT, TRGEPC,
!          .              SRFVEC,      SSLPHS, SSLSOL, SSLEMI    )
!     C
!     C     Do the same for the sub-spacecraft point.
!     C
!           CALL T_ILUMIN ( 'Ellipsoid', TARGET, ET,     'IAU_MARS',
!          .              ABCORR,      OBSRVR, SSCPT,  TRGEPC,
!          .              SRFVEC,      SSCPHS, SSCSOL, SSCEMI    )
!     C
!     C     Convert the angles to degrees and write them out.
!     C
!           SSLPHS = DPR() * SSLPHS
!           SSLSOL = DPR() * SSLSOL
!           SSLEMI = DPR() * SSLEMI
!
!           SSCPHS = DPR() * SSCPHS
!           SSCSOL = DPR() * SSCSOL
!           SSCEMI = DPR() * SSCEMI
!
!           WRITE (*,*) ' '
!           WRITE (*,*) 'UTC epoch is ', UTC
!           WRITE (*,*) ' '
!           WRITE (*,*) 'Illumination angles at the sub-solar point:'
!           WRITE (*,*) ' '
!           WRITE (*,*) 'Phase angle           (deg.): ', SSLPHS
!           WRITE (*,*) 'Solar incidence angle (deg.): ', SSLSOL
!           WRITE (*,*) 'Emission angle        (deg.): ', SSLEMI
!           WRITE (*,*) ' '
!           WRITE (*,*) 'The solar incidence angle should be 0.'
!           WRITE (*,*) 'The emission and phase angles should be equal.'
!
!           WRITE (*,*) ' '
!           WRITE (*,*) 'Illumination angles at the sub-s/c point:'
!           WRITE (*,*) ' '
!           WRITE (*,*) 'Phase angle           (deg.): ', SSCPHS
!           WRITE (*,*) 'Solar incidence angle (deg.): ', SSCSOL
!           WRITE (*,*) 'Emission angle        (deg.): ', SSCEMI
!           WRITE (*,*) ' '
!           WRITE (*,*) 'The emission angle should be 0.'
!           WRITE (*,*) 'The solar incidence and phase angles should '
!          .//          'be equal.'
!           END
!
!
!     When this program was executed on a PC/Linux/g77 platform,
!     the output was:
!
!        UTC epoch is 2004 JAN 1 12:00:00
!
!        Illumination angles at the sub-solar point:
!
!        Phase angle           (deg.):   115.542001
!        Solar incidence angle (deg.):   3.20530645E-15
!        Emission angle        (deg.):   115.542001
!
!        The solar incidence angle should be 0.
!        The emission and phase angles should be equal.
!
!        Illumination angles at the sub-s/c point:
!
!        Phase angle           (deg.):   62.0840031
!        Solar incidence angle (deg.):   62.0840031
!        Emission angle        (deg.):   6.46461886E-11
!
!        The emission angle should be 0.
!        The solar incidence and phase angles should be equal.
!
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    Test Utility Version 1.0.0, 20-FEB-2012 (NJB)
!
!-       Based on BSPICELIB Version 1.1.0, 17-MAY-2010 (NJB)
!
!-&

! $ Index_Entries
!
!     illumination angles
!     lighting angles
!     phase angle
!     solar incidence angle
!     emission angle
!
!-&

! $ Revisions
!
!     None.
!
!-&

!
!     SPICELIB functions
!


!
!     Local parameters
!
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  =  'T_ILUMIN' )

!
!     This value will become system-dependent when systems
!     using 128-bit d.p. numbers are supported by SPICELIB.
!     CNVLIM, when added to 1.0D0, should yield 1.0D0.
!
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-17 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXITR
      PARAMETER           ( MAXITR =  5 )

      INTEGER               SUN
      PARAMETER           ( SUN    = 10 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    LOCMTH
      CHARACTER*(LNSIZE)    PRVMTH
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      CORPOS ( 3 )
      DOUBLE PRECISION      CORVJ2 ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ETDIFF
      DOUBLE PRECISION      J2POS  ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTDIFF
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      OFFOBS ( 3 )
      DOUBLE PRECISION      OFFSUN ( 3 )
      DOUBLE PRECISION      PREVET
      DOUBLE PRECISION      PREVLT
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SLT
      DOUBLE PRECISION      SSBOST ( 6 )
      DOUBLE PRECISION      SSBTST ( 6 )
      DOUBLE PRECISION      STLOFF ( 3 )
      DOUBLE PRECISION      SUBVEC ( 3 )
      DOUBLE PRECISION      SUBVJ2 ( 3 )
      DOUBLE PRECISION      SUNPOS ( 3 )
      DOUBLE PRECISION      TPOS   ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               CENTER
      INTEGER               I
      INTEGER               N
      INTEGER               NITR
      INTEGER               OBSCDE
      INTEGER               REFCDE
      INTEGER               TRGCDE
      INTEGER               TYPE
      INTEGER               TYPEID

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               ELIPSD
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

!
!     Saved variables
!
      SAVE                  ELIPSD
      SAVE                  FIRST
      SAVE                  PRVCOR
      SAVE                  PRVMTH
      SAVE                  USECN
      SAVE                  USELT
      SAVE                  USESTL

!
!     Note: XMIT need not be saved, since it's used only
!     for error checking when an aberration correction flag
!     is parsed.
!

!
!     Initial values
!
      DATA                  ELIPSD  / .TRUE. /
      DATA                  FIRST   / .TRUE. /
      DATA                  PRVCOR  / ' '    /
      DATA                  PRVMTH  / 'Ellipsoid' /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )


      IF (  FIRST  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any. Analyze the new flag.
!
         CALL ZZPRSCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

!
!        The aberration correction flag is recognized; save it.
!
         PRVCOR = ABCORR

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           XMIT is .TRUE. when the correction is for transmitted
!           radiation.
!
!           USELT is .TRUE. when any type of light time correction
!           (normal or converged Newtonian) is specified.
!
!           USECN indicates converged Newtonian light time correction.
!
!           USESTL indicates stellar aberration corrections.
!
!
!        The above definitions are consistent with those used by
!        ZZPRSCOR.
!
         XMIT    =  ATTBLK ( XMTIDX )
         USELT   =  ATTBLK ( LTIDX  )
         USECN   =  ATTBLK ( CNVIDX )
         USESTL  =  ATTBLK ( STLIDX )
!
!        Reject an aberration correction flag calling for transmission
!        corrections.
!
         IF ( XMIT ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for ' &
            //            'transmission-style corrections.'        )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         END IF

!
!        Reject an aberration correction flag calling for stellar
!        aberration but not light time correction.
!
         IF (  USESTL  .AND.  ( .NOT. USELT )  ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for ' &
            //            'stellar aberration but not light time ' &
            //            'corrections. This combination is not ' &
            //            'expected.'                              )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         ELSE IF ( ATTBLK(RELIDX) ) THEN
!
!           Also reject flags calling for relativistic corrections.
!
            CALL SETMSG ( 'Aberration correction flag # calls for ' &
            //            'relativistic light time correction.'    )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         END IF

      END IF

!
!     Obtain integer codes for the target and observer.
!
      CALL BODS2C ( TARGET, TRGCDE, FND )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The target, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, OBSCDE, FND )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF

!
!     Check the input body codes. If they are equal, signal
!     an error.
!
      IF ( OBSCDE .EQ. TRGCDE ) THEN

         CALL SETMSG ( 'In computing the sub-solar point, ' &
         //            'the observing body and target body are the ' &
         //            'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( RNAME                                        )
         RETURN

      END IF

!
!     Determine the attributes of the frame designated by FIXREF.
!
      CALL NAMFRM ( FIXREF, REFCDE )

      CALL FRINFO ( REFCDE, CENTER, TYPE, TYPEID, FND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' // &
                       'the SPICE frame subsystem. Possibly '    // &
                       'a required frame definition kernel has ' // &
                       'not been loaded.'                        )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL SIGERR ( 'SPICE(NOFRAME)'                          )
         CALL CHKOUT ( RNAME                                     )
         RETURN

      END IF

!
!     Make sure that FIXREF is centered at the target body's center.
!
      IF ( CENTER .NE. TRGCDE ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the ' &
         //            'target body #. The ID code of the frame ' &
         //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  CENTER                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF

!
!     If necessary, parse the method specification. PRVMTH
!     and the derived flags NEAR and ELIPSD start out with
!     valid values. PRVMTH records the last valid value of
!     METHOD; ELIPSD is the corresponding shape flag.
!
      IF ( METHOD .NE. PRVMTH ) THEN
!
!        Parse the computation method specification. Work with a local
!        copy of the method specification that contains no leading or
!        embedded blanks.
!
         CALL CMPRSS ( ' ', 0, METHOD, LOCMTH )
         CALL UCASE  ( LOCMTH,         LOCMTH )

!
!        Check the shape specification.
!

         IF ( LOCMTH .NE. 'ELLIPSOID'  ) THEN

            CALL SETMSG ( 'Computation method argument was <#>; this ' &
            //            'string must specify a supported shape ' &
            //            'model and computation type. See the ' &
            //            'header of SUBSLR for details.'            )
            CALL ERRCH  ( '#',  METHOD                               )
            CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                     )
            CALL CHKOUT ( RNAME                                      )
            RETURN

         END IF

!
!        At this point the method specification has passed our tests.
!        Use the flag ELIPSD to indicate that the shape is modeled as
!        an ellipsoid (which is true, for now).
!
         ELIPSD = .TRUE.
!
!        Save the current value of METHOD.
!
         PRVMTH = METHOD

      END IF

!
!     Get the sign S prefixing LT in the expression for TRGEPC.
!     When light time correction is not used, setting S = 0
!     allows us to seamlessly set TRGEPC equal to ET.
!
      IF ( USELT ) THEN
         S = -1.D0
      ELSE
         S =  0.D0
      END IF

!
!     Determine the position of the observer in target body-fixed
!     coordinates. This is a first estimate.
!
!         -  Call SPKEZP to compute the position of the target body as
!            seen from the observing body and the light time (LT)
!            between them. We request that the coordinates of POS be
!            returned relative to the body fixed reference frame
!            associated with the target body, using aberration
!            corrections specified by the input argument ABCORR.
!
!         -  Call VMINUS to negate the direction of the vector (OBSPOS)
!            so it will be the position of the observer as seen from
!            the target body in target body fixed coordinates.
!
!            Note that this result is not the same as the result of
!            calling SPKEZP with the target and observer switched. We
!            computed the vector FROM the observer TO the target in
!            order to get the proper light time and stellar aberration
!            corrections (if requested). Now we need the inverse of
!            that corrected vector in order to compute the sub-solar
!            point.
!
      CALL SPKEZP ( TRGCDE, ET, FIXREF, ABCORR, OBSCDE, TPOS, LT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

!
!     Negate the target's position to obtain the position of the
!     observer relative to the target.
!
      CALL VMINUS ( TPOS, OBSPOS )

      RANGE  = VNORM ( OBSPOS )

      IF ( RANGE .EQ. 0.D0 ) THEN
!
!        We've already ensured that observer and target are
!        distinct, so this should be a very unusual occurrence.
!
         CALL SETMSG ( 'Observer-target distance is zero. ' &
         //            'Observer is #; target is #.'       )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL SIGERR ( 'SPICE(NOSEPARATION)'               )
         CALL CHKOUT ( RNAME                               )
         RETURN

      END IF

!
!     Make a first estimate of the light time and target epoch. Note
!     that TRGEPC will equal ET if we're performing an uncorrected
!     computation, since in that case, S will be zero.
!
      CALL VSUB ( SPOINT, OBSPOS, SRFVEC )

      DIST   = VNORM ( SRFVEC )
      LT     = DIST /  CLIGHT()

      TRGEPC = ET + S*LT

!
!     If we're using light time corrections, refine our light time,
!     target epoch, and observer position estimates.
!
      IF ( USELT ) THEN
!
!        We'll now make improved light time, target epoch, and observer
!        position estimates using the previous estimates. The number of
!        iterations depends on the light time correction type.
!
         IF ( USECN ) THEN
            NITR = MAXITR
         ELSE
            NITR = 1
         END IF

!
!        Get the J2000-relative state of the observer relative to
!        the solar system barycenter at ET.
!
         CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOST )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

!
!        Initialize the variables required to evaluate the
!        loop termination condition.
!
         I      = 0
         LTDIFF = 1.D0
         ETDIFF = 1.D0
         PREVLT = LT
         PREVET = TRGEPC

         DO WHILE (       ( I      .LT.   NITR                ) &
                    .AND. ( LTDIFF .GT. ( CNVLIM * ABS(LT) )  ) &
                    .AND. ( ETDIFF .GT.   0.D0                )  )
!
!           Get the J2000-relative state of the target relative to
!           the solar system barycenter at the target epoch.
!
            CALL SPKSSB ( TRGCDE, TRGEPC, 'J2000', SSBTST )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

!
!           Find the position of the observer relative to the target.
!           Convert this vector from the J2000 frame to the target
!           frame at TRGEPC.
!
            CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
            CALL PXFORM ( 'J2000', FIXREF, TRGEPC, XFORM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXV ( XFORM, J2POS, OBSPOS )

!
!           If we're using stellar aberration corrections, adjust the
!           observer position to account for the stellar aberration
!           correction applicable to SPOINT.
!
            IF ( USESTL ) THEN
!
!              We want to apply the stellar aberration correction that
!              applies to our current estimate of the sub-solar point
!              location, NOT the correction for the target body's
!              center. In most cases the two corrections will be
!              similar, but they might not be---consider the case of a
!              highly prolate target body where the observer is close
!              to one "end" of the body.
!
!              Find the vector from the observer to the estimated
!              sub-solar point. Find the stellar aberration offset
!              STLOFF for this vector. Note that all vectors are
!              expressed relative to the target body-fixed frame at
!              TRGEPC. We must perform our corrections in an inertial
!              frame.
!
               CALL VSUB ( SPOINT, OBSPOS, SUBVEC )

               CALL MTXV ( XFORM,  SUBVEC, SUBVJ2 )

!
!              Note that we don't handle the transmission
!              case here.
!
               CALL STELAB ( SUBVJ2, SSBOST(4), CORVJ2 )

               CALL MXV  ( XFORM,  CORVJ2, CORPOS )
               CALL VSUB ( CORPOS, SUBVEC, STLOFF )

!
!              In principle, we want to shift the target body position
!              relative to the solar system barycenter by STLOFF, but
!              we can skip this step and just re-compute the observer's
!              location relative to the target body's center by
!              subtracting off STLOFF.
!
               CALL VSUB ( OBSPOS, STLOFF, VTEMP  )
               CALL VEQU ( VTEMP,          OBSPOS )

            END IF

            DIST = VDIST ( OBSPOS, SPOINT )

!
!           Compute a new light time estimate and new target epoch.
!
            LT      =  DIST / CLIGHT()
            TRGEPC  =  ET   + S*LT

!
!           At this point, we have new estimates of the sub-solar point
!           SPOINT, the observer altitude DIST, the target epoch TRGEPC,
!           and the position of the observer relative to the target
!           OBSPOS.
!
!           We use the d.p. identity function TOUCHD to force the
!           compiler to create double precision arguments from the
!           differences LT-PREVLT and TRGEPC-PREVET. Some compilers
!           will perform extended-precision register arithmetic, which
!           can prevent a difference from rounding to zero. Simply
!           storing the result of the subtraction in a double precision
!           variable doesn't solve the problem, because that variable
!           can be optimized out of existence.
!
            LTDIFF  =   ABS( TOUCHD(LT     - PREVLT) )
            ETDIFF  =   ABS( TOUCHD(TRGEPC - PREVET) )
            PREVLT  =   LT
            PREVET  =   TRGEPC
            I       =   I + 1

         END DO

      END IF

!
!     Find the body-fixed position of the Sun as seen from the target
!     at TRGEPC.
!
      CALL SPKEZP ( SUN, TRGEPC, FIXREF, ABCORR, TRGCDE, SUNPOS, SLT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

!
!     Now we'll modify the target-Sun vector to take into account the
!     offset between the target center and the surface point of
!     interest; we want the vector to point from the surface point to
!     Sun.
!
      CALL VSUB ( SUNPOS, SPOINT, OFFSUN )

!
!     Let OFFOBS be the offset observer position: this vector
!     points from SPOINT to the observer.
!
      CALL VSUB   ( SPOINT, OBSPOS, SRFVEC )
      CALL VMINUS ( SRFVEC, OFFOBS )

!
!     Find the surface normal at SPOINT. This computation depends
!     on target body shape model.
!
      IF ( ELIPSD ) THEN
!
!        We'll need the radii of the target body.
!
         CALL BODVCD ( TRGCDE, 'RADII', 3, N, RADII )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )

      ELSE
!
!        We've already checked the computation method input argument,
!        so we don't expect to arrive here. This code is present for
!        safety.
!
         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( RNAME                                           )
         RETURN

      END IF

!
!     Find the illumination angles. VSEP will give us angular
!     separation in radians.
!
      PHASE   =  VSEP ( OFFSUN, OFFOBS )
      SOLAR   =  VSEP ( NORMAL, OFFSUN )
      EMISSN  =  VSEP ( NORMAL, OFFOBS )

!
!     TRGEPC and SRFVEC have already been set.
!
      CALL CHKOUT ( RNAME )
      RETURN
      END
