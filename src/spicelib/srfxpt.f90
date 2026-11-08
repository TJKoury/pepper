!$Procedure SRFXPT ( Surface intercept point )

      SUBROUTINE SRFXPT ( METHOD,  TARGET,  ET,      ABCORR, &
                          OBSRVR,  DREF,    DVEC,    SPOINT, &
                          DIST,    TRGEPC,  OBSPOS,  FOUND )

! $ Abstract
!
!     Deprecated: This routine has been superseded by the SPICELIB
!     routine SINCPT. This routine is supported for purposes of
!     backward compatibility only.
!
!     Given an observer and a direction vector defining a ray, compute
!     the surface intercept point of the ray on a target body at a
!     specified epoch, optionally corrected for light time and stellar
!     aberration.
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
!     FRAMES
!     NAIF_IDS
!     PCK
!     SPK
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'frmtyp.inc'
      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      OBSPOS ( 3 )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TARGET     I   Name of target body.
!     ET         I   Epoch in ephemeris seconds past J2000 TDB.
!     ABCORR     I   Aberration correction.
!     OBSRVR     I   Name of observing body.
!     DREF       I   Reference frame of input direction vector.
!     DVEC       I   Ray's direction vector.
!     SPOINT     O   Surface intercept point on the target body.
!     DIST       O   Distance from the observer to the intercept point.
!     TRGEPC     O   Intercept epoch.
!     OBSPOS     O   Observer position relative to target center.
!     FOUND      O   Flag indicating whether intercept was found.
!
! $ Detailed_Input
!
!     METHOD   is a short string providing parameters defining
!              the computation method to be used. Parameters
!              include, but are not limited to, the shape model
!              used to represent the surface of the target body.
!
!              The only choice currently supported is
!
!                 'Ellipsoid'        The intercept computation uses
!                                    a triaxial ellipsoid to model
!                                    the surface of the target body.
!                                    The ellipsoid's radii must be
!                                    available in the kernel pool.
!
!              Neither case nor white space are significant in
!              METHOD. For example, the string ' eLLipsoid ' is
!              valid.
!
!              In a later Toolkit release, this argument will be
!              used to invoke a wider range of surface
!              representations. For example, it will be possible to
!              represent the target body's surface using a digital
!              model.
!
!     TARGET   is the name of the target body. TARGET is
!              case-insensitive, and leading and trailing blanks in
!              TARGET are not significant. Optionally, you may
!              supply a string containing the integer ID code
!              for the object. For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!              When the target body's surface is represented by a
!              tri-axial ellipsoid, this routine assumes that a
!              kernel variable representing the ellipsoid's radii is
!              present in the kernel pool. Normally the kernel
!              variable would be defined by loading a PCK file.
!
!     ET       is the epoch of participation of the observer,
!              expressed as ephemeris seconds past J2000 TDB: ET is
!              the epoch at which the observer's state is computed.
!
!              When aberration corrections are not used, ET is also
!              the epoch at which the state and orientation of the
!              target body are computed.
!
!              When aberration corrections are used, ET is the epoch
!              at which the observer's state relative to the solar
!              system barycenter is computed; in this case the
!              position and orientation of the target body are
!              computed at ET-LT or ET+LT, where LT is the one-way
!              light time between the intercept point and the
!              observer, and the sign applied to LT depends on the
!              selected correction. See the description of ABCORR
!              below for details.
!
!     ABCORR   indicates the aberration correction to be applied
!              when computing the observer-target state and the
!              orientation of the target body. ABCORR may be any of
!              the following.
!
!                 'NONE'     Apply no correction. Return the
!                            geometric surface intercept point on the
!                            target body.
!
!              Let LT represent the one-way light time between the
!              observer and the surface intercept point (note: NOT
!              between the observer and the target body's center).
!              The following values of ABCORR apply to the
!              "reception" case in which photons depart from the
!              intercept point's location at the light-time
!              corrected epoch ET-LT and *arrive* at the observer's
!              location at ET:
!
!                 'LT'       Correct for one-way light time (also
!                            called "planetary aberration") using a
!                            Newtonian formulation. This correction
!                            yields the location of the surface
!                            intercept point at the moment it
!                            emitted photons arriving at the
!                            observer at ET.
!
!                            The light time correction uses an
!                            iterative solution of the light time
!                            equation. The solution invoked by the
!                            'LT' option uses one iteration.
!
!                            Both the target state as seen by the
!                            observer, and rotation of the target
!                            body, are corrected for light time.
!
!                 'LT+S'     Correct for one-way light time and
!                            stellar aberration using a Newtonian
!                            formulation. This option modifies the
!                            state obtained with the 'LT' option to
!                            account for the observer's velocity
!                            relative to the solar system
!                            barycenter. The result is the apparent
!                            surface intercept point as seen by the
!                            observer.
!
!                 'CN'       Converged Newtonian light time
!                            correction. In solving the light time
!                            equation, the 'CN' correction iterates
!                            until the solution converges. Both the
!                            state and rotation of the target body
!                            are corrected for light time.
!
!                 'CN+S'     Converged Newtonian light time
!                            and stellar aberration corrections.
!
!              The following values of ABCORR apply to the
!              "transmission" case in which photons *depart* from
!              the observer's location at ET and arrive at the
!              intercept point at the light-time corrected epoch
!              ET+LT:
!
!                 'XLT'      "Transmission" case: correct for
!                            one-way light time using a Newtonian
!                            formulation. This correction yields the
!                            intercept location at the moment it
!                            receives photons emitted from the
!                            observer's location at ET.
!
!                            The light time correction uses an
!                            iterative solution of the light time
!                            equation. The solution invoked by the
!                            'LT' option uses one iteration.
!
!                            Both the target state as seen by the
!                            observer, and rotation of the target
!                            body, are corrected for light time.
!
!                 'XLT+S'    "Transmission" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation  This option modifies the
!                            intercept obtained with the 'XLT'
!                            option to account for the observer's
!                            velocity relative to the solar system
!                            barycenter.
!
!                 'XCN'      Converged Newtonian light time
!                            correction. This is the same as XLT
!                            correction but with further iterations
!                            to a converged Newtonian light time
!                            solution.
!
!                 'XCN+S'    "Transmission" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!     OBSRVR   is the name of the observing body. This is typically
!              a spacecraft, the earth, or a surface point on the
!              earth. OBSRVR is case-insensitive, and leading and
!              trailing blanks in OBSRVR are not significant.
!              Optionally, you may supply a string containing the
!              integer ID code for the object. For example both
!              'MOON' and '301' are legitimate strings that indicate
!              the moon is the observer.
!
!     DREF     is the name of the reference frame relative to which
!              the input direction vector is expressed. This may be
!              any frame supported by the SPICE system, including
!              built-in frames (documented in the Frames Required
!              Reading) and frames defined by a loaded frame kernel
!              (FK).
!
!              When DREF designates a non-inertial frame, the
!              orientation of the frame is evaluated at an epoch
!              dependent on the frame's center and, if the center is
!              not the observer, on the selected aberration
!              correction. See the description of the direction
!              vector DVEC for details.
!
!     DVEC     is a pointing vector emanating from the observer. The
!              intercept with the target body's surface of the ray
!              defined by the observer and DVEC is sought.
!
!              DVEC is specified relative to the reference frame
!              designated by DREF.
!
!              Non-inertial reference frames are treated as follows:
!              if the center of the frame is at the observer's
!              location, the frame is evaluated at ET. If the
!              frame's center is located elsewhere, then letting
!              LTCENT be the one-way light time between the observer
!              and the central body associated with the frame, the
!              orientation of the frame is evaluated at ET-LTCENT,
!              ET+LTCENT, or ET depending on whether the requested
!              aberration correction is, respectively, for received
!              radiation, transmitted radiation, or is omitted.
!              LTCENT is computed using the method indicated by
!              ABCORR.
!
! $ Detailed_Output
!
!     SPOINT   is the surface intercept point on the target body of
!              the ray defined by the observer and the direction
!              vector. If the ray intersects the target body in
!              multiple points, the selected intersection point is
!              the one closest to the observer. The output
!              argument FOUND (see below) indicates whether an
!              intercept was found.
!
!              SPOINT is expressed in Cartesian coordinates,
!              relative to the body-fixed frame associated with the
!              target body. The body-fixed target frame is
!              evaluated at the intercept epoch TRGEPC (see
!              description below).
!
!              When light time correction is used, the duration of
!              light travel between SPOINT to the observer is
!              considered to be the one way light time. When both
!              light time and stellar aberration corrections are
!              used, SPOINT is selected such that, when SPOINT is
!              corrected for light time and the vector from the
!              observer to the light-time corrected location of
!              SPOINT is corrected for stellar aberration, the
!              resulting vector is parallel to the ray defined by
!              the observer's location and DVEC.
!
!              The components of SPOINT are given in units of km.
!
!     DIST     is the distance between the observer and the surface
!              intercept on the target body. DIST is given in
!              units of km.
!
!     TRGEPC   is the "intercept epoch." This is the epoch at which
!              the ray defined by OBSRVR and DVEC intercepts the
!              target surface at SPOINT. TRGEPC is defined as
!              follows: letting LT be the one-way light time between
!              the observer and the intercept point, TRGEPC is the
!              epoch ET-LT, ET+LT, or ET depending on whether the
!              requested aberration correction is, respectively, for
!              received radiation, transmitted radiation, or
!              omitted. LT is computed using the method indicated by
!              ABCORR.
!
!              TRGEPC is expressed as seconds past J2000 TDB.
!
!     OBSPOS   is the vector from the center of the target body at
!              epoch TRGEPC to the observer at epoch ET. OBSPOS is
!              expressed in the target body-fixed reference frame
!              evaluated at TRGEPC. (This is the frame relative to
!              which SPOINT is given.)
!
!              OBSPOS is returned to simplify various related
!              computations that would otherwise be cumbersome. For
!              example, the vector XVEC from the observer to SPOINT
!              can be calculated via the call
!
!                 CALL VSUB ( SPOINT, OBSPOS, XVEC )
!
!              The components of OBSPOS are given in units of km.
!
!     FOUND    is a logical flag indicating whether or not the ray
!              intersects the target. If an intersection exists
!              FOUND will be returned as .TRUE. If the ray misses
!              the target, FOUND will be returned as .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     If any of the listed errors occur, the output arguments are
!     left unchanged.
!
!     1)  If the input argument METHOD is not recognized, an error
!         is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If TARGET cannot be mapped to an ID code, the error
!         SPICE(IDCODENOTFOUND) is signaled.
!
!     3)  If OBSRVR cannot be mapped to an ID code, an error is signaled
!         by a routine in the call tree of this routine.
!
!     4)  If the input argument ABCORR is invalid, an error
!         is signaled by a routine in the call tree of this
!         routine.
!
!     5)  If a body-fixed reference frame associated with the
!         target cannot be found, the error SPICE(NOFRAME) is signaled.
!
!     6)  If OBSRVR and TARGET map to the same NAIF integer ID codes, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     7)  If frame definition data enabling the evaluation of the state
!         of the target relative to the observer in target body-fixed
!         coordinates have not been loaded prior to calling SRFXPT, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     8)  If the specified aberration correction is not recognized, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     9)  If insufficient ephemeris data have been loaded prior to
!         calling SRFXPT, an error is signaled by a
!         routine in the call tree of this routine. Note that when
!         light time correction is used, sufficient ephemeris data
!         must be available to propagate the states of both observer
!         and target to the solar system barycenter.
!
!     10) If the computation method has been specified as "Ellipsoid"
!         and triaxial radii of the target body have not been loaded
!         into the kernel pool prior to calling SRFXPT, an error is
!         signaled by a routine in the call tree of this routine.
!
!     11) If PCK data needed to define the target body-fixed frame have
!         not been loaded prior to calling SRFXPT, an error is signaled
!         by a routine in the call tree of this routine.
!
!     12) If the reference frame designated by DREF is not recognized
!         by the SPICE frame subsystem, an error is signaled
!         by a routine in the call tree of this routine.
!
!     13) If the direction vector DVEC is the zero vector, an error
!         is signaled  by a routine in the call tree of this routine.
!
!     14) If radii for TARGET are not found in the kernel pool, an error
!         is signaled by a routine in the call tree of this routine.
!
!     15) If the size of the TARGET body radii kernel variable is not
!         three, an error is signaled by a routine in the call tree of
!         this routine.
!
!     16) If any of the three TARGET body radii is less-than or equal to
!         zero, an error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     Appropriate SPK, PCK, and frame kernels must be loaded by the
!     calling program before this routine is called.  CK, SCLK, and
!     IK kernels may be required as well.
!
!     The following data are required:
!
!     -  SPK data: ephemeris data for target and observer must be
!        loaded. If aberration corrections are used, the states of
!        target and observer relative to the solar system barycenter
!        must be calculable from the available ephemeris data.
!        Typically ephemeris data are made available by loading one
!        or more SPK files via FURNSH.
!
!     -  PCK data: if the computation method is specified as
!        "Ellipsoid," triaxial radii for the target body must be
!        loaded into the kernel pool. Typically this is done by
!        loading a text PCK file via FURNSH.
!
!     -  Further PCK data: rotation data for the target body must
!        be loaded. These may be provided in a text or binary PCK
!        file.
!
!     -  Frame data: if a frame definition is required to convert
!        the observer and target states to the body-fixed frame of
!        the target, that definition must be available in the kernel
!        pool. Similarly, the frame definition required to map
!        between the frame designated by DREF and the target
!        body-fixed frame must be available. Typically the
!        definitions of frames not already built-in to SPICE are
!        supplied by loading a frame kernel.
!
!     The following data may be required:
!
!     -  CK data: if the frame to which DREF refers is fixed to a
!        spacecraft instrument or structure, at least one CK file
!        will be needed to permit transformation of vectors between
!        that frame and both J2000 and the target body-fixed frame.
!
!     -  SCLK data: if a CK file is needed, an associated SCLK
!        kernel is required to enable conversion between encoded SCLK
!        (used to time-tag CK data) and barycentric dynamical time
!        (TDB).
!
!     -  IK data: one or more I-kernels may be required to enable
!        transformation of vectors from an instrument-fixed frame to
!        a spacecraft-fixed frame whose attitude is given by a
!        C-kernel.
!
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     Given a ray defined by a direction vector and the location of an
!     observer, SRFXPT computes the surface intercept point of the ray
!     on a specified target body. SRFXPT also determines the distance
!     between the observer and the surface intercept point.
!
!     When aberration corrections are used, this routine finds the
!     value of SPOINT such that, if SPOINT is regarded as an ephemeris
!     object, after the selected aberration corrections are applied to
!     the vector from the observer to SPOINT, the resulting vector is
!     parallel to the direction vector DVEC.
!
!     This routine computes light time corrections using light time
!     between the observer and the surface intercept point, as opposed
!     to the center of the target. Similarly, stellar aberration
!     corrections done by this routine are based on the direction of
!     the vector from the observer to the light-time corrected
!     intercept point, not to the target center. This technique avoids
!     errors due to the differential between aberration corrections
!     across the target body. Therefore it's valid to use aberration
!     corrections with this routine even when the observer is very
!     close to the intercept point, in particular when the
!     observer-intercept point distance is much less than the
!     observer-target center distance. It's also valid to use stellar
!     aberration corrections even when the intercept point is near or
!     on the limb (as may occur in occultation computations using a
!     point target).
!
!     When comparing surface intercept point computations with results
!     from sources other than SPICE, it's essential to make sure the
!     same geometric definitions are used.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following program computes surface intercept points on
!        Mars for the boresight and FOV boundary vectors of the MGS MOC
!        narrow angle camera. The intercepts are computed for a single
!        observation epoch. Light time and stellar aberration
!        corrections are used. For simplicity, camera distortion is
!        ignored.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: srfxpt_ex1.tm
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
!              de405s.bsp                       Planetary ephemeris
!              mars_iau2000_v0.tpc              Planet orientation and
!                                               radii
!              naif0011.tls                     Leapseconds
!              mgs_moc_v20.ti                   MGS MOC instrument
!                                               parameters
!              mgs_sclkscet_00061.tsc           MGS SCLK coefficients
!              mgs_sc_ext12.bc                  MGS s/c bus attitude
!              mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de405s.bsp',
!                                  'mars_iau2000_v0.tpc',
!                                  'naif0011.tls',
!                                  'mgs_moc_v20.ti',
!                                  'mgs_sclkscet_00061.tsc',
!                                  'mgs_sc_ext12.bc',
!                                  'mgs_ext12_ipng_mgs95j.bsp' )
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM SRFXPT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters
!        C
!              INTEGER               ABCLEN
!              PARAMETER           ( ABCLEN = 20 )
!
!              INTEGER               LNSIZE
!              PARAMETER           ( LNSIZE = 78 )
!
!              INTEGER               METLEN
!              PARAMETER           ( METLEN = 40 )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 32 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN = 50 )
!
!              INTEGER               SHPLEN
!              PARAMETER           ( SHPLEN = 80 )
!
!              INTEGER               NCORNR
!              PARAMETER           ( NCORNR = 4 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(ABCLEN)    ABCORR
!              CHARACTER*(NAMLEN)    CAMERA
!              CHARACTER*(NAMLEN)    DREF
!              CHARACTER*(METLEN)    METHOD
!              CHARACTER*(NAMLEN)    OBSRVR
!              CHARACTER*(NAMLEN)    SHAPE
!              CHARACTER*(NAMLEN)    TARGET
!              CHARACTER*(LNSIZE)    TITLE
!              CHARACTER*(TIMLEN)    UTC
!
!              DOUBLE PRECISION      BOUNDS ( 3, NCORNR )
!              DOUBLE PRECISION      BSIGHT ( 3 )
!              DOUBLE PRECISION      DIST
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      DVEC   ( 3 )
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      OBSPOS ( 3 )
!              DOUBLE PRECISION      RADIUS
!              DOUBLE PRECISION      SPOINT ( 3 )
!              DOUBLE PRECISION      TRGEPC
!
!              INTEGER               CAMID
!              INTEGER               I
!              INTEGER               J
!              INTEGER               N
!
!              LOGICAL               FOUND
!
!              DATA                  ABCORR / 'LT+S'      /
!              DATA                  CAMERA / 'MGS_MOC_NA'/
!              DATA                  METHOD / 'Ellipsoid' /
!              DATA                  OBSRVR / 'MGS'       /
!              DATA                  TARGET / 'Mars'      /
!              DATA                  UTC    /
!             .                '2003 OCT 13 06:00:00 UTC' /
!
!        C
!        C     Load kernel files.
!        C
!              CALL FURNSH ( 'srfxpt_ex1.tm' )
!
!        C
!        C     Convert the UTC request time to ET (seconds past
!        C     J2000, TDB).
!        C
!              CALL STR2ET ( UTC, ET )
!
!        C
!        C     Get the MGS MOC Narrow angle camera (MGS_MOC_NA)
!        C     ID code.  Then look up the field of view (FOV)
!        C     parameters by calling GETFOV.
!        C
!              CALL BODN2C ( CAMERA, CAMID, FOUND )
!
!              IF ( .NOT. FOUND ) THEN
!                 CALL SETMSG ( 'Could not find ID code for ' //
!             .                 'instrument #.'               )
!                 CALL ERRCH  ( '#', CAMERA                   )
!                 CALL SIGERR ( 'SPICE(NOTRANSLATION)'        )
!              END IF
!
!        C
!        C     GETFOV will return the name of the camera-fixed frame
!        C     in the string DREF, the camera boresight vector in
!        C     the array BSIGHT, and the FOV corner vectors in the
!        C     array BOUNDS.
!        C
!              CALL GETFOV ( CAMID,  NCORNR, SHAPE,  DREF,
!             .              BSIGHT, N,      BOUNDS       )
!
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Surface Intercept Locations for Camera'
!              WRITE (*,*) 'FOV Boundary and Boresight Vectors'
!              WRITE (*,*) ' '
!              WRITE (*,*) '   Instrument:            ', CAMERA
!              WRITE (*,*) '   Epoch:                 ', UTC
!              WRITE (*,*) '   Aberration correction: ', ABCORR
!              WRITE (*,*) ' '
!
!        C
!        C     Now compute and display the surface intercepts for the
!        C     boresight and all of the FOV boundary vectors.
!        C
!              DO I = 1, NCORNR+1
!
!                 IF ( I .LE. NCORNR ) THEN
!
!                    TITLE = 'Corner vector #'
!                    CALL REPMI ( TITLE, '#', I, TITLE )
!
!                    CALL VEQU ( BOUNDS(1,I), DVEC )
!
!                 ELSE
!
!                    TITLE = 'Boresight vector'
!                    CALL VEQU ( BSIGHT, DVEC )
!
!                 END IF
!
!        C
!        C        Compute the surface intercept point using
!        C        the specified aberration corrections.
!        C
!        C        SRFXPT will signal an error if required kernel
!        C        data are unavailable.  See example (2) below for
!        C        a suggestion on detecting absence of C-kernel
!        C        data prior to calling SRFXPT.
!        C
!                 CALL SRFXPT ( METHOD, TARGET, ET,     ABCORR,
!             .                 OBSRVR, DREF,   DVEC,   SPOINT,
!             .                 DIST,   TRGEPC, OBSPOS, FOUND  )
!
!                 IF ( FOUND ) THEN
!        C
!        C           Convert rectangular coordinates to planetocentric
!        C           latitude and longitude. Convert radians to degrees.
!        C
!                    CALL RECLAT ( SPOINT, RADIUS, LON, LAT )
!
!                    LON = LON * DPR ()
!                    LAT = LAT * DPR ()
!        C
!        C           Display the results.
!        C
!                    WRITE (*,*) ' '
!                    WRITE (*,*) TITLE
!
!                    TITLE = '  Vector in # frame = '
!                    CALL REPMC ( TITLE, '#', DREF, TITLE )
!
!                    WRITE (*,*) ' '
!                    WRITE (*,*) TITLE
!
!                    IF ( I .LE. NCORNR ) THEN
!                       WRITE (*,'(A,3F20.14)') '  ',
!             .                               ( BOUNDS(J,I), J=1,3 )
!                    ELSE
!                       WRITE (*,'(A,3F20.14)') '  ', BSIGHT
!                    END IF
!
!                    WRITE (*,*) ' '
!                    WRITE (*,*) '  Intercept:'
!                    WRITE (*,*)
!             .      '     Radius                   (km)  = ', RADIUS
!                    WRITE (*,*)
!             .      '     Planetocentric Latitude  (deg) = ', LAT
!                    WRITE (*,*)
!             .      '     Planetocentric Longitude (deg) = ', LON
!                    WRITE (*,*)
!             .      '     Range                    (km)  = ', DIST
!                    WRITE (*,*) ' '
!
!                 ELSE
!
!                    WRITE (*,*) ' '
!                    WRITE (*,*) 'Intercept not found.'
!                    WRITE (*,*) ' '
!
!                 END IF
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
!         Surface Intercept Locations for Camera
!         FOV Boundary and Boresight Vectors
!
!            Instrument:            MGS_MOC_NA
!            Epoch:                 2003 OCT 13 06:00:00 UTC
!            Aberration correction: LT+S
!
!
!         Corner vector 1
!
!           Vector in MGS_MOC_NA frame =
!              0.00000185713838   -0.00380156226592    0.99999277403434
!
!           Intercept:
!              Radius                   (km)  =    3384.9411359391133
!              Planetocentric Latitude  (deg) =   -48.477481851561002
!              Planetocentric Longitude (deg) =   -123.47407882634886
!              Range                    (km)  =    388.98310724844424
!
!
!         Corner vector 2
!
!           Vector in MGS_MOC_NA frame =
!              0.00000185713838    0.00380156226592    0.99999277403434
!
!           Intercept:
!              Radius                   (km)  =    3384.9396987514451
!              Planetocentric Latitude  (deg) =   -48.481636266908055
!              Planetocentric Longitude (deg) =   -123.39882275183645
!              Range                    (km)  =    388.97512489721356
!
!
!         Corner vector 3
!
!           Vector in MGS_MOC_NA frame =
!             -0.00000185713838    0.00380156226592    0.99999277403434
!
!           Intercept:
!              Radius                   (km)  =    3384.9396899058052
!              Planetocentric Latitude  (deg) =   -48.481661836856034
!              Planetocentric Longitude (deg) =   -123.39882595816586
!              Range                    (km)  =    388.97466598000682
!
!
!         Corner vector 4
!
!           Vector in MGS_MOC_NA frame =
!             -0.00000185713838   -0.00380156226592    0.99999277403434
!
!           Intercept:
!              Radius                   (km)  =    3384.9411270910964
!              Planetocentric Latitude  (deg) =   -48.477507427894842
!              Planetocentric Longitude (deg) =   -123.47408199055646
!              Range                    (km)  =    388.98264816551176
!
!
!         Boresight vector
!
!           Vector in MGS_MOC_NA frame =
!              0.00000000000000    0.00000000000000    1.00000000000000
!
!           Intercept:
!              Radius                   (km)  =    3384.9404101835457
!              Planetocentric Latitude  (deg) =   -48.479579751487201
!              Planetocentric Longitude (deg) =   -123.43645374920047
!              Range                    (km)  =    388.97573917648396
!
!
!     2) SRFXPT will signal an error if required kernel data are
!        unavailable: for example, in the program of Example 1, if the
!        C-kernel containing data for the MGS bus had a gap at epoch ET,
!        SRFXPT would be unable to transform the direction vector DVEC
!        from the reference frame fixed to the camera to the reference
!        frame fixed to the target body.
!
!        We could modify the code of Example 1 as shown below to test
!        for the availability of C-kernel data. We would add the
!        declarations shown, and we'd call the C-kernel reader CKGP to
!        find whether the desired pointing was available. Depending on
!        the value of the FOUND flag returned by CKGP, we'd go on to
!        compute the surface intercept point or respond to the error
!        condition.
!
!
!                        .
!                        .
!                        .
!
!        C
!        C     Local parameters
!        C
!              INTEGER               BUSID
!              PARAMETER           ( BUSID = -94000 )
!
!              INTEGER               MGS
!              PARAMETER           ( MGS    = -94 )
!                        .
!                        .
!                        .
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      CLKOUT
!              DOUBLE PRECISION      CMAT   ( 3, 3 )
!              DOUBLE PRECISION      SCLKDP
!
!                        .
!                        .
!                        .
!
!        C
!        C     Look up the transformation from the J2000 frame to the
!        C     MGS spacecraft frame. To do this, we'll need to
!        C     represent our observation epoch in terms of MGS encoded
!        C     SCLK.
!        C
!              CALL SCE2C ( MGS, ET, SCLKDP )
!
!        C
!        C     Look up the spacecraft attitude from the C-kernel.
!        C
!              CALL CKGP ( BUSID, SCLKDP, 0.D0, 'J2000',
!             .            CMAT,  CLKOUT, FOUND         )
!
!              IF ( FOUND ) THEN
!
!                 [Proceed to compute intercept point]
!
!              ELSE
!
!                 [Handle case where pointing is unavailable
!                  for the epoch of interest]
!
!              END IF
!                        .
!                        .
!                        .
!
! $ Restrictions
!
!     1)  A cautionary note: if aberration corrections are used, and if
!         DREF is the target body-fixed frame, the epoch at which that
!         frame is evaluated is offset from ET by the light time between
!         the observer and the *center* of the target body. This light
!         time normally will differ from the light time between the
!         observer and intercept point. Consequently the orientation of
!         the target body-fixed frame at TRGEPC will not match that of
!         the target body-fixed frame at the epoch associated with DREF.
!         As a result, various derived quantities may not be as
!         expected: for example, OBSPOS would not be the inverse of the
!         aberration-corrected position of the target as seen by the
!         observer.
!
!         In many applications the errors arising from this frame
!         discrepancy may be insignificant; however a safe approach is
!         to always use as DREF a frame other than the target body-fixed
!         frame.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.6.0, 01-NOV-2021 (EDW) (JDR)
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Updated example #1 to use a meta-kernel to load the required
!        kernels and modified its output format to comply with the
!        maximum line length for header comments.
!
!-    SPICELIB Version 1.5.0, 31-MAR-2014 (BVS)
!
!        Updated to save the input body names and ZZBODTRN state
!        counters and to do name-ID conversions only if the counters
!        have changed.
!
!        Updated to save the input frame name and POOL state counter
!        and to do frame name-ID conversion only if the counter has
!        changed.
!
!-    SPICELIB Version 1.4.1, 18-MAY-2010 (BVS)
!
!        Index line now states that this routine is deprecated.
!
!-    SPICELIB Version 1.4.0, 23-MAR-2009 (NJB)
!
!        Bug fix: quick test for non-intersection is
!        no longer performed when observer-target distance
!        is less than target's maximum radius.
!
!        Typo correction in $Required_Reading: changed FRAME
!        to FRAMES.
!
!-    SPICELIB Version 1.3.0, 15-FEB-2008 (NJB)
!
!        Bug fix: near-miss case light time improvement
!        logic is no longer applied when a geometric
!        solution is requested via ABCORR.
!
!        References to unneeded variables FJ2000 and FIRST
!        were deleted.
!
!        Header typo was corrected; reference to VMINUS was replaced
!        with reference to VSUB.
!
!        $Abstract now states that this routine is deprecated.
!
!-    SPICELIB Version 1.2.1, 25-APR-2007 (NJB)
!
!        Header typo was corrected; reference to VMINUS was replaced
!        with reference to VSUB.
!
!-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
!
!        Call to BODVAR was replaced with call to BODVCD.
!
!-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB)
!
!        Updated to use BODS2C.
!
!-    SPICELIB Version 1.0.0, 27-FEB-2004 (NJB)
!
!-&


! $ Index_Entries
!
!     DEPRECATED surface intercept point
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               EQCHR
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
!
!     This value will become system-dependent when systems
!     using 128-bit d.p. numbers are supported by SPICELIB.
!     CNVLIM, when added to 1.0D0, should yield 1.0D0.
!
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-17 )

!
!     Round-off error limit for arc sine input:
!
      DOUBLE PRECISION      RNDTOL
      PARAMETER           ( RNDTOL = 1.D-14 )

!
!     Fraction of planetary angular radius used to define
!     region outside of which rays are immediately rejected
!     as non-intersecting.
!
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.001D0 )


      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN =  15 )

      INTEGER               MAXITR
      PARAMETER           ( MAXITR =  10 )

!
!     Saved body name length.
!
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )

!
!     Saved frame name length.
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )


!
!     Local variables
!
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(ABCLEN)    LOCCOR

      DOUBLE PRECISION      ETDIFF
      DOUBLE PRECISION      J2DIR  ( 3 )
      DOUBLE PRECISION      J2EST  ( 3 )
      DOUBLE PRECISION      J2POS  ( 3 )
      DOUBLE PRECISION      J2TMAT ( 3, 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTCENT
      DOUBLE PRECISION      LTDIFF
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      NEGPOS ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      PREVET
      DOUBLE PRECISION      PREVLT
      DOUBLE PRECISION      R2JMAT ( 3, 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RAYALT
      DOUBLE PRECISION      REFEPC
      DOUBLE PRECISION      REJECT
      DOUBLE PRECISION      RPOS   ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SSBOST ( 6 )
      DOUBLE PRECISION      SSBTST ( 6 )
      DOUBLE PRECISION      STLDIR ( 3 )
      DOUBLE PRECISION      STLERR ( 3 )
      DOUBLE PRECISION      STLTMP ( 3 )
      DOUBLE PRECISION      TPOS   ( 3 )
      DOUBLE PRECISION      TRGDIR ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               CENTER
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               NITR
      INTEGER               OBSCDE
      INTEGER               REFCDE
      INTEGER               TRGCDE
      INTEGER               TYPE
      INTEGER               TYPEID

      LOGICAL               FND
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

!
!     Saved name/ID item declarations.
!
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTCDE
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSR
      INTEGER               SVOBSC
      LOGICAL               SVFND2

      LOGICAL               FIRST

!
!     Saved frame name/ID item declarations.
!
      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVDREF
      INTEGER               SVREFC


!
!     Saved name/ID items.
!
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTCDE
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVOBSR
      SAVE                  SVOBSC
      SAVE                  SVFND2

      SAVE                  FIRST

!
!     Saved frame name/ID items.
!
      SAVE                  SVCTR3
      SAVE                  SVDREF
      SAVE                  SVREFC


!
!     Initial values.
!
      DATA                  FIRST   / .TRUE. /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SRFXPT' )

!
!     Nothing has been found yet.
!
      FOUND = .FALSE.

!
!     Initialization.
!
      IF ( FIRST ) THEN

!
!        Initialize counters.
!
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )
         CALL ZZCTRUIN( SVCTR3 )

         FIRST = .FALSE.

      END IF

!
!     Obtain integer codes for the target and observer.
!
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTCDE, SVFND1, &
                      TARGET, TRGCDE, FND    )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The target, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SRFXPT'                                   )
         RETURN

      END IF


      CALL ZZBODS2C ( SVCTR2, SVOBSR, SVOBSC, SVFND2, &
                      OBSRVR, OBSCDE, FND    )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SRFXPT'                                   )
         RETURN

      END IF


!
!     Check the input body codes.  If they are equal, signal
!     an error.
!
      IF ( OBSCDE .EQ. TRGCDE ) THEN

         CALL SETMSG ( 'In computing the surface intercept point, ' // &
                       'the observing body and target body are the '// &
                       'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( 'SRFXPT'                                     )
         RETURN

      END IF

!
!     Get a left-justified, upper case copy of the aberration
!     correction flag.
!
      CALL LJUST ( ABCORR, LOCCOR )
      CALL UCASE ( LOCCOR, LOCCOR )

!
!     Check for stellar aberration in the aberration correction flag.
!
      USESTL  =  INDEX( LOCCOR, '+S') .GT. 0

!
!     Now remove the stellar aberration component from the aberration
!     correction flag; we'll do our state lookups without stellar
!     aberration correction.
!
      CALL REPMC ( LOCCOR, '+S', ' ', LOCCOR )

!
!     Decide whether the aberration correction is for received or
!     transmitted radiation.
!
      XMIT = EQCHR ( LOCCOR(1:1),  'X' )

!
!     Decide what sort of light time correction has been requested.
!
      USECN = ( LOCCOR(1:2) .EQ. 'CN' ) .OR. ( LOCCOR(1:3) .EQ. 'XCN' )

      USELT =        USECN &
              .OR. ( LOCCOR(1:2) .EQ. 'LT'  ) &
              .OR. ( LOCCOR(1:3) .EQ. 'XLT' )

!
!     Get the sign S prefixing LT in the expression for TRGEPC.
!     When light time correction is not used, setting S = 0
!     allows us to seamlessly set TRGEPC equal to ET.
!
      IF ( USELT ) THEN

         IF ( XMIT ) THEN
            S   =  1.D0
         ELSE
            S   = -1.D0
         END IF

      ELSE
         S = 0.D0
      END IF

!
!     Find the name of the body-fixed frame associated with the
!     target body.  We'll want the state of the target relative to
!     the observer in this body-fixed frame.
!
      CALL CIDFRM ( TRGCDE, FRCODE, FRNAME, FND )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'No body-fixed frame is associated with '   // &
                       'target body #; a frame kernel must be '    // &
                       'loaded to make this association.  Consult '// &
                       'the FRAMES Required Reading for details.'   )
         CALL ERRCH  ( '#', TARGET                                  )
         CALL SIGERR ( 'SPICE(NOFRAME)'                             )
         CALL CHKOUT ( 'SRFXPT'                                     )
         RETURN

      END IF

!
!     Determine the position of the observer in target
!     body-fixed coordinates.
!
!         -  Call SPKEZP to compute the position of the target body as
!            seen from the observing body and the light time (LT)
!            between them.  We request that the coordinates of POS be
!            returned relative to the body fixed reference frame
!            associated with the target body, using aberration
!            corrections specified by the input argument ABCORR.
!
!         -  Call VMINUS to negate the direction of the vector (OBSPOS)
!            so it will be the position of the observer as seen from
!            the target body in target body fixed coordinates.
!
!            Note that this result is not the same as the result of
!            calling SPKEZP with the target and observer switched.  We
!            computed the vector FROM the observer TO the target in
!            order to get the proper light time and stellar aberration
!            corrections (if requested).  Now we need the inverse of
!            that corrected vector in order to compute the intercept
!            point.
!

      CALL SPKEZP ( TRGCDE, ET, FRNAME, LOCCOR, OBSCDE, TPOS, LT )

!
!     Negate the target's position to obtain the position of the
!     observer relative to the target.
!
      CALL VMINUS ( TPOS, OBSPOS )

!
!     We now need to convert the direction vector into the
!     body fixed frame associated with the target.  The target
!     epoch is dependent on the aberration correction.  The
!     coefficient S has been set to give us the correct answer
!     for each case.
!
      TRGEPC = ET  +  S*LT

!
!     Determine the attributes of the frame designated by DREF.
!
      CALL ZZNAMFRM ( SVCTR3, SVDREF, SVREFC, DREF, REFCDE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SRFXPT' )
         RETURN
      END IF

      CALL FRINFO ( REFCDE, CENTER, TYPE, TYPEID, FND )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' // &
                       'the SPICE frame subsystem.  Possibly '   // &
                       'a required frame definition kernel has ' // &
                       'not been loaded.'                        )
         CALL ERRCH  ( '#',  DREF                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
         CALL CHKOUT ( 'SRFXPT'                                  )
         RETURN

      END IF

!
!     Transform the direction vector from frame DREF to the body-fixed
!     frame associated with the target.  The epoch TRGEPC associated
!     with the body-fixed frame has been set already.
!
!     We'll compute the transformation in two parts:  first
!     from frame DREF to J2000, then from J2000 to the target
!     frame.
!
      IF ( TYPE .EQ. INERTL ) THEN
!
!        Inertial frames can be evaluated at any epoch.
!
         REFEPC = ET


      ELSE IF ( .NOT. USELT ) THEN
!
!        We're not using light time corrections (converged or
!        otherwise), so there's no time offset.
!
         REFEPC = ET


      ELSE IF ( CENTER .EQ. OBSCDE ) THEN
!
!        If the center of frame DREF is the observer (which is
!        usually the case if the observer is a spacecraft), then
!        the epoch of frame DREF is simply ET.
!
!        There's no offset between the center for frame DREF
!        and the observer.
!
         REFEPC = ET

      ELSE
!
!        Find the light time from the observer to the center of
!        frame DREF.
!
         CALL SPKEZP ( CENTER, ET, 'J2000', LOCCOR, OBSCDE, &
                       RPOS,   LTCENT                       )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SRFXPT' )
            RETURN
         END IF

         REFEPC  =  ET  +  S * LTCENT

      END IF

!
!     The epoch REFEPC associated with frame DREF has been set.
!
!     Compute the transformation from frame DREF to J2000 and the
!     transformation from J2000 to the target body-fixed frame.
!
!     Map DVEC to both the J2000 and target body-fixed frames. We'll
!     store DVEC, expressed relative to the J2000 frame, in the
!     variable J2DIR.  DVEC in the target body-fixed frame will be
!     stored in TRGDIR.
!
!     We may need both versions of DVEC:  if we use light time
!     correction, we'll update "intercept epoch", and hence the
!     transformation between J2000 and the target body-fixed frame.
!     The transformation between DREF and J2000 doesn't change, on the
!     other hand, so we don't have to recompute J2DIR.  We need TRGDIR
!     in all cases.
!
      CALL PXFORM ( DREF, 'J2000', REFEPC,  R2JMAT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SRFXPT' )
         RETURN
      END IF

      CALL MXV ( R2JMAT, DVEC, J2DIR )

!
!     Map J2DIR (in the J2000 frame) to the target body-fixed
!     frame.
!
      CALL PXFORM ( 'J2000', FRNAME,  TRGEPC,  J2TMAT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SRFXPT' )
         RETURN
      END IF

      CALL MXV ( J2TMAT, J2DIR, TRGDIR )

!
!     At this point,
!
!        TRGEPC is set.
!        TRGDIR is set.
!        J2DIR is set.
!
!
!     Get the J2000-relative state of the observer relative to
!     the solar system barycenter at ET.  We'll use this in
!     several places later.
!
      CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOST )

!
!     If we're using stellar aberration correction, at this point we'll
!     account for it.  We're going to find a surface point such that
!     the radiation path from that point to the observer, after
!     correction for stellar aberration, is parallel to the ray. So
!     by applying the inverse of the correction to the ray, we obtain
!     the ray with which we must perform our intercept computation.
!
      IF ( USESTL ) THEN
!
!        We approximate the inverse stellar aberration correction by
!        using the correction for the reverse transmission direction.
!        If we're in the reception case, we apply the transmission
!        stellar aberration correction to J2DIR and vice versa.
!
         IF ( XMIT ) THEN
!
!           Use reception stellar aberration correction
!           routine STELAB to generate a first estimate of
!           the direction vector after stellar aberration
!           has been "removed"---that is, apply the inverse
!           of the transmission stellar aberration correction
!           mapping to J2DIR.
!
            CALL STELAB ( J2DIR, SSBOST(4), STLDIR )

!
!           Estimate the error in our first approximation
!           by applying the transmission stellar aberration
!           to STLDIR and finding the difference with J2DIR.
!
            CALL STLABX ( STLDIR, SSBOST(4), J2EST  )
            CALL VSUB   ( J2DIR,  J2EST,     STLERR )

!
!           Adding the error in the transmission mapping to STLDIR
!           will give us a second-order estimate of the inverse.
!
            CALL VADD ( STLERR, STLDIR, STLTMP )
            CALL VEQU ( STLTMP,         STLDIR )
!
!           At this point we've found a good estimate of the
!           direction vector under the inverse of the transmission
!           stellar aberration correction mapping.
!
         ELSE
!
!           Use transmission stellar aberration correction
!           routine STLABX to generate a first estimate of
!           the direction vector after stellar aberration
!           has been "removed."
!
            CALL STLABX ( J2DIR, SSBOST(4), STLDIR )

!
!           Estimate the error in our first approximation
!           by applying the reception stellar aberration
!           to STLDIR and finding the difference with J2DIR.
!
            CALL STELAB ( STLDIR, SSBOST(4), J2EST  )
            CALL VSUB   ( J2DIR,  J2EST,     STLERR )

!
!           Adding the error in the reception mapping to STLDIR
!           will give us a second-order estimate of the inverse.
!
            CALL VADD ( STLERR, STLDIR, STLTMP )
            CALL VEQU ( STLTMP,         STLDIR )

!
!           At this point we've found a good estimate of the
!           direction vector under the inverse of the reception
!           stellar aberration correction mapping.
!
         END IF

!
!        Replace the J2000-relative ray direction with the corrected
!        direction.
!
         CALL VEQU ( STLDIR,  J2DIR )
         CALL MXV  ( J2TMAT,  J2DIR,   TRGDIR )

      END IF

!
!     Find the surface intercept point and distance from observer to
!     intercept point using the specified geometric definition.
!
      IF (  EQSTR( METHOD, 'Ellipsoid' )  ) THEN
!
!        Find the surface intercept given the target epoch,
!        observer-target position, and target body orientation
!        we've already computed.  If we're not using light
!        time correction, this is all we must do.  Otherwise,
!        our result will give us an initial estimate of the
!        target epoch, which we'll then improve.
!

!
!        Get the radii of the target body from the kernel pool.
!
         CALL ZZGFTREB ( TRGCDE, RADII )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SRFXPT' )
            RETURN
         END IF

!
!        Make an easy test to see whether we can quit now because
!        an intercept cannot exist.  If the ray is separated from
!        the observer-target center vector by more than (MARGIN *
!        the maximum triaxial radius), we're done.  Let REJECT be
!        the angular separation limit.
!
         MAXRAD = MAX  ( RADII(1), RADII(2), RADII(3) )

         RANGE  = VNORM(OBSPOS)

         IF ( RANGE .EQ. 0.D0 ) THEN
!
!           We've already ensured that observer and target are
!           distinct, so this should be a very unusual occurrence.
!
            CALL SETMSG ( 'Observer-target distance is zero.' )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'               )
            CALL CHKOUT ( 'SRFXPT'                            )
            RETURN

         END IF


         IF ( RANGE .GT. MARGIN*MAXRAD ) THEN
!
!           Compute the arc sine with SPICE error checking.
!
            REJECT = DASINE ( MARGIN * MAXRAD / RANGE,  RNDTOL )

            CALL VMINUS ( OBSPOS, NEGPOS )

            IF (  VSEP( NEGPOS, TRGDIR )  .GT.  REJECT  ) THEN
!
!              The angular separation of ray and target is too great
!              for a solution to exist, even with a better light time
!              estimate.
!
               CALL CHKOUT ( 'SRFXPT' )
               RETURN

            END IF

         END IF

!
!        Locate the nearest point to the observer on the target.
!
         CALL SURFPT ( OBSPOS, TRGDIR, RADII(1), RADII(2), RADII(3), &
                       SPOINT, FOUND                                 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SRFXPT' )
            RETURN
         END IF

!
!        If we're not using light time corrections, we're almost
!        done now. TRGEPC, OBSPOS, and FOUND have been set.
!        If an intercept was found, SPOINT has been set as well.
!        We haven't yet computed DIST.
!
         IF ( .NOT. USELT ) THEN

            IF ( FOUND ) THEN
               DIST  =  VDIST ( OBSPOS, SPOINT )
            END IF

            CALL CHKOUT ( 'SRFXPT' )
            RETURN

         END IF


         IF ( .NOT. FOUND ) THEN
!
!           If there's no intercept, we're probably done.  However,
!           we need to guard against the possibility that the ray does
!           intersect the ellipsoid but we haven't discovered it
!           because our first light time estimate was too poor.
!
!           We'll make an improved light time estimate as follows:
!           Find the nearest point on the ellipsoid to the ray.  Find
!           the light time between the observer and this point.
!
!           If we're using converged Newtonian corrections, we
!           iterate this procedure up to two times.
!
            IF ( USECN ) THEN
               NITR = 2
            ELSE
               NITR = 1
            END IF

            I = 1

            DO WHILE (  ( I .LE. NITR ) .AND. ( .NOT. FOUND )  )

               CALL NPEDLN ( RADII(1), RADII(2), RADII(3), OBSPOS, &
                             TRGDIR,   PNEAR,    RAYALT           )

               LT      =  VDIST ( OBSPOS, PNEAR ) / CLIGHT()
!
!              Use the new light time estimate to repeat the intercept
!              computation.
!
               TRGEPC  =  ET  +  S * LT

!
!              Get the J2000-relative state of the target relative to
!              the solar system barycenter at the target epoch.
!
               CALL SPKSSB ( TRGCDE, TRGEPC, 'J2000', SSBTST )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SRFXPT' )
                  RETURN
               END IF

!
!              Find the position of the observer relative to the target.
!              Convert this vector from the J2000 frame to the target
!              frame at TRGEPC.
!
               CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
               CALL PXFORM ( 'J2000', FRNAME, TRGEPC, XFORM )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SRFXPT' )
                  RETURN
               END IF

!
!              Convert the observer's position relative to the target
!              from the J2000 frame to the target frame at the target
!              epoch.
!
               CALL MXV ( XFORM, J2POS, OBSPOS )

!
!              Convert the ray's direction vector from the J2000 frame
!              to the target frame at the target epoch.
!
               CALL MXV ( XFORM, J2DIR, TRGDIR )

!
!              Repeat the intercept computation.
!
               CALL SURFPT ( OBSPOS,   TRGDIR, RADII(1), RADII(2), &
                             RADII(3), SPOINT, FOUND               )

               I = I + 1

            END DO
!
!           If there's still no intercept, we're done.
!
            IF ( .NOT. FOUND ) THEN
               CALL CHKOUT ( 'SRFXPT' )
               RETURN
            END IF

         END IF

!
!        We've got an intersection.  SURFPT doesn't compute range, so do
!        it here.
!
         DIST  =  VDIST ( OBSPOS, SPOINT )

!
!        Since we're using light time corrections, we're going to make
!        an estimate of light time to the intercept point, then re-do
!        our computation of the target position and orientation using
!        the new light time value.
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

!
!        Compute new light time estimate and new target epoch.
!
         LT     =  DIST / CLIGHT()
         TRGEPC =  ET  +  S * LT

         PREVLT = 0.D0
         PREVET = TRGEPC

         I      = 0
         LTDIFF = 1.D0
         ETDIFF = 1.D0

         DO WHILE (       ( I      .LT.   NITR                ) &
                    .AND. ( LTDIFF .GT. ( CNVLIM * ABS(LT) )  ) &
                    .AND. ( ETDIFF .GT.   0.D0                )  )

!
!           Get the J2000-relative state of the target relative to
!           the solar system barycenter at the target epoch.
!
            CALL SPKSSB ( TRGCDE, TRGEPC, 'J2000', SSBTST )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SRFXPT' )
               RETURN
            END IF
!
!           Find the position of the observer relative to the target.
!           Convert this vector from the J2000 frame to the target
!           frame at TRGEPC.
!
            CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
            CALL PXFORM ( 'J2000', FRNAME, TRGEPC, XFORM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SRFXPT' )
               RETURN
            END IF

!
!           Convert the observer's position relative to the target from
!           the J2000 frame to the target frame at the target epoch.
!
            CALL MXV    ( XFORM,  J2POS, OBSPOS )
            CALL VMINUS ( OBSPOS, NEGPOS )

!
!           Convert the ray's direction vector from the J2000 frame
!           to the target frame at the target epoch.
!
            CALL MXV ( XFORM, J2DIR, TRGDIR )

!
!           Repeat the intercept computation.
!
            CALL SURFPT ( OBSPOS, TRGDIR, RADII(1), RADII(2), RADII(3), &
                          SPOINT, FOUND                                )

!
!           If there's no intercept, we're done.
!
            IF ( .NOT. FOUND ) THEN
                CALL CHKOUT ( 'SRFXPT' )
               RETURN
            END IF

!
!           Compute the distance between intercept and observer.
!
            DIST    =   VDIST ( OBSPOS, SPOINT )

!
!           Compute new light time estimate and new target epoch.
!
            LT      =  DIST / CLIGHT()

            TRGEPC  =  ET  +  S * LT

!
!           We use the d.p. identity function TOUCHD to force the
!           compiler to create double precision arguments from the
!           differences LT-PREVLT and TRGEPC-PREVET. Some compilers
!           will perform extended-precision register arithmetic, which
!           can prevent a difference from rounding to zero.  Simply
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

      ELSE

         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( 'SRFXPT'                                        )
         RETURN

      END IF

!
!     FOUND, SPOINT, TRGEPC, and DIST have been set at this point.
!
      CALL CHKOUT ( 'SRFXPT' )
      RETURN
      END
