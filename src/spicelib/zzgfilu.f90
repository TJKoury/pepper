!$Procedure ZZGFILU ( GF, illumination angle utilities )

      SUBROUTINE ZZGFILU ( METHOD, ANGTYP, TARGET, ILLUM, FIXREF, &
                           ABCORR, OBSRVR, SPOINT, ET,    UDFUNC, &
                           DECRES, ANGLE                          )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the entry points used by
!     GFEVNT in order to find illumination angle events.
!
! $ Required_Reading
!
!     GF
!     NAIF_IDS
!     SPK
!     TIME
!
! $ Keywords
!
!     ANGLE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         ANGTYP
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLUM
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      ET
      EXTERNAL              UDFUNC
      LOGICAL               DECRES
      DOUBLE PRECISION      ANGLE

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     METHOD     I   ZZGFILIN
!     ANGTYP     I   ZZGFILIN
!     TARGET     I   ZZGFILIN
!     ILLUM      I   ZZGFILIN
!     FIXREF     I   ZZGFILIN
!     ABCORR     I   ZZGFILIN
!     OBSRVR     I   ZZGFILIN
!     SPOINT     I   ZZGFILIN
!     ET         I   ZZGFILLT, ZZGFILGQ
!     UDFUNC     I   ZZGFILDC
!     DECRES     O   ZZGFILDC
!     ANGLE      O   ZZGFILGQ
!
! $ Detailed_Input
!
!     See individual entry points.
!
! $ Detailed_Output
!
!     See individual entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  See individual entry points.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!     -  SPK data: ephemeris data for target, observer, and
!        illumination source, for the times at which state or
!        positions are computed, must be loaded. If aberration
!        corrections are used, the states of target and observer
!        relative to the solar system barycenter must be calculable
!        from the available ephemeris data. Typically ephemeris data
!        are made available by loading one or more SPK files via
!        FURNSH.
!
!     -  If non-inertial reference frames are used, then PCK
!        files, frame kernels, C-kernels, and SCLK kernels may be
!        needed.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This is an umbrella for routines required by the GF scalar
!     quantity search algorithm to support searches involving
!     illumination angle constraints.
!
!     The entry points of this routine are:
!
!        ZZGFILIN   Saves the user-supplied inputs defining the
!                   illumination angle computation to be performed.
!                   Initializes the illumination angle search.
!
!        ZZGFILDC   Determines whether or not a specified illumination
!                   angle is decreasing at a specified epoch.
!
!        ZZGFILGQ   Returns the specified illumination angle
!                   at a specified epoch.
!
! $ Examples
!
!     See GFEVNT.
!
! $ Restrictions
!
!     1)  This is a SPICELIB private routine; it should not be called by
!         user applications.
!
!     2)  ZZGFILIN must be called prior to use of any of the other
!         entry points.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 30-MAY-2021 (NJB) (JDR)
!
!        Edited the umbrella routine and all its entry points' headers
!        to comply with NAIF standard.
!
!        Updated header of entry point ZZGFILIN: added missing
!        argument documentation.
!
!-    SPICELIB Version 1.1.0, 08-MAR-2017 (NJB)
!
!        Bug fixes: added FAILED checks in entry points ZZGFILIN,
!        ZZGFILDC, and ZZGFILGQ.
!
!        Removed unused declaration of parameter SUN.
!
!
!-    SPICELIB Version 1.0.0, 23-MAY-2012 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     umbrella routine for finding illumination angle events.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ESRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 200 )

!
!     Indices of illumination angles in the ANGLES
!     array:
!
      INTEGER               PHSIDX
      PARAMETER           ( PHSIDX = 1 )

      INTEGER               INCIDX
      PARAMETER           ( INCIDX = 2 )

      INTEGER               EMTIDX
      PARAMETER           ( EMTIDX = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 50 )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    ANGNMS ( 3 )
      CHARACTER*(CORLEN)    SVCORR
      CHARACTER*(BDNMLN)    SVINAM
      CHARACTER*(MTHLEN)    SVMETH
      CHARACTER*(BDNMLN)    SVONAM
      CHARACTER*(FRNMLN)    SVREF
      CHARACTER*(BDNMLN)    SVTNAM

      DOUBLE PRECISION      ANGLES ( 3 )
      DOUBLE PRECISION      EMISTA ( 2 )
      DOUBLE PRECISION      ETTARG
      DOUBLE PRECISION      INCSTA ( 2 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PHSSTA ( 2 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SVNRML ( 3 )
      DOUBLE PRECISION      SVSSPT ( 3 )

      INTEGER               N
      INTEGER               FXFCDE
      INTEGER               FXCLSS
      INTEGER               FXTYID
      INTEGER               FXCENT
      INTEGER               SVAIDX
      INTEGER               SVOBS
      INTEGER               SVILUM
      INTEGER               SVTARG

      LOGICAL               FOUND
      LOGICAL               SVABLK ( NABCOR )

!
!     Saved Variables
!
      SAVE                  ANGNMS
      SAVE                  SVABLK
      SAVE                  SVAIDX
      SAVE                  SVCORR
      SAVE                  SVILUM
      SAVE                  SVINAM
      SAVE                  SVMETH
      SAVE                  SVNRML
      SAVE                  SVOBS
      SAVE                  SVONAM
      SAVE                  SVREF
      SAVE                  SVSSPT
      SAVE                  SVTARG
      SAVE                  SVTNAM

!
!     Initial values
!
      DATA                  ANGNMS / 'PHASE', &
                                     'INCIDENCE', &
                                     'EMISSION'   /

!
!     This routine should never be called directly.
!
      CALL CHKIN  ( 'ZZGFILU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFILU'           )
      RETURN



!$Procedure ZZGFILIN ( GF, illumination angle utility initialization )

      ENTRY ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                       FIXREF, ABCORR, OBSRVR, SPOINT )

! $ Abstract
!
!     Initialize the GF illumination angle constraint search utilities.
!
! $ Required_Reading
!
!     GF
!     NAIF_IDS
!     SPK
!     TIME
!
! $ Keywords
!
!     ANGLE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!      CHARACTER*(*)         METHOD
!      CHARACTER*(*)         ANGTYP
!      CHARACTER*(*)         TARGET
!      CHARACTER*(*)         ILLUM
!      CHARACTER*(*)         FIXREF
!      CHARACTER*(*)         ABCORR
!      CHARACTER*(*)         OBSRVR
!      DOUBLE PRECISION      SPOINT ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     ANGTYP     I   Type of illumination angle.
!     TARGET     I   Target body name.
!     ILLUM      I   Illumination source name.
!     FIXREF     I   Body-fixed, body-centered target body frame.
!     ABCORR     I   Aberration correction specifier.
!     OBSRVR     I   Observer name.
!     SPOINT     I   Body-fixed coordinates of a target surface point.
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
!                 'Ellipsoid'        The illumination angle
!                                    computation uses a triaxial
!                                    ellipsoid to model the surface
!                                    of the target body. The
!                                    ellipsoid's radii must be
!                                    available in the kernel pool.
!
!              Neither case nor blanks are significant in METHOD.
!              For example, the string ' eLLipsoid ' is valid.
!
!
!     ANGTYP   is a string specifying the type of illumination
!              angle for which a search is to be performed. The
!              possible values of ANGTYP are
!
!                 'PHASE'
!                 'INCIDENCE'
!                 'EMISSION'
!
!              When the illumination source is the sun, the
!              incidence angle is commonly called the "solar
!              incidence angle."
!
!              See the $Particulars section below for a detailed
!              description of these angles.
!
!              Neither case nor white space are significant in
!              ANGTYP. For example, the string ' Incidence ' is
!              valid.
!
!
!     TARGET   is the name of a target body. Optionally, you may
!              supply the integer ID code for the object as
!              an integer string. For example both 'MOON' and
!              '301' are legitimate strings that indicate the
!              moon is the target body.
!
!              The target and observer define a position vector
!              which points from the observer to the target.
!
!              Case and leading or trailing blanks are not
!              significant in the string TARGET.
!
!
!     ILLUM    is the name of the illumination source. This source
!              may be any ephemeris object. Case, blanks, and
!              numeric values are treated in the same way as for the
!              input TARGET.
!
!     FIXREF   is the name of the body-fixed, body-centered
!              reference frame associated with the target body. The
!              input surface point SPOINT is expressed relative to
!              this reference frame, and this frame is used to
!              define the orientation of the target body as a
!              function of time.
!
!              The string FIXREF is case-insensitive, and leading
!              and trailing blanks in FIXREF are not significant.
!
!     ABCORR   indicates the aberration corrections to be applied
!              when computing the target's position and orientation.
!              Any value accepted by SPKEZR may be used.
!
!              See the header of the SPICE routine SPKEZR for a
!              detailed description of the aberration correction
!              options.
!
!              Case and embedded blanks are not significant in
!              ABCORR.
!
!
!     OBSRVR   is the name of the body from which the occultation is
!              observed. Optionally, you may supply the integer NAIF
!              ID code for the body as a string.
!
!              Case and leading or trailing blanks are not
!              significant in the string OBSRVR.
!
!
!     SPOINT   is a surface point on the target body, expressed in
!              Cartesian coordinates, relative to the body-fixed
!              target frame designated by FIXREF.
!
!              SPOINT need not be visible from the observer's
!              location in order for the constraint specified by
!              RELATE and REFVAL (see descriptions below) to be
!              satisfied.
!
!              The components of SPOINT have units of km.
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
!     1)  If name of the target, observer, or illumination source
!         cannot be translated to a NAIF ID code, the error
!         SPICE(IDCODENOTFOUND) is signaled.
!
!     2)  If  target body coincides with the observer body OBSRVR or
!         the illumination source SOURCE, error,
!         the error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     3)  If transmission-style aberration corrections are requested,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     4)  If the aberration correction string is invalid, an error
!         is signaled by a routine in the call tree of this
!         routine.
!
!     5)  If the illumination angle type is not recognized, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     6)  If the input target body-fixed frame FIXREF is not
!         recognized, the error SPICE(UNKNOWNFRAME) is signaled. A
!         frame name may fail to be recognized because a required frame
!         specification kernel has not been loaded; another cause is a
!         misspelling of the frame name.
!
!     7)  If the input frame FIXREF is not centered at the target body,
!         the error SPICE(INVALIDFRAME) is signaled.
!
!     8)  If the input argument METHOD is not recognized, the error
!         SPICE(INVALIDMETHOD) is signaled.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFILU.
!
! $ Particulars
!
!     This routine must be called once before each GF search for
!     illumination angle events.
!
! $ Examples
!
!     See GFEVNT.
!
! $ Restrictions
!
!     1)  This is a SPICELIB private routine; it should not be called by
!         user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 30-MAY-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated header: added missing argument documentation.
!
!-    SPICELIB Version 1.1.0, 08-MAR-2017 (NJB)
!
!        Bug fixes: now checks FAILED after each BODS2C call
!        and after the BODVRD call.
!
!-    SPICELIB Version 1.0.0, 23-MAY-2012 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     illumination angle initialization routine
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFILIN' )

!
!     Find NAIF IDs for TARGET, OBSRVR, and ILLUM.
!
      CALL BODS2C ( TARGET, SVTARG, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFILIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFILIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( ILLUM, SVILUM, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The illumination source, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', ILLUM                                 )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFILIN'                                 )
         RETURN

      END IF


!
!     Make sure the observer and target are distinct.
!
      IF ( SVTARG .EQ. SVOBS ) THEN

         CALL SETMSG ( 'The observer and target must be ' &
         //            'distinct objects, but are not: ' &
         //            'OBSRVR = #; TARGET = #.'           )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( 'ZZGFILIN'                          )
         RETURN

      END IF

!
!     Make sure the target and illumination source are distinct.
!
      IF ( SVTARG .EQ. SVILUM ) THEN

         CALL SETMSG ( 'The target and illumination source must ' &
         //            'be distinct objects, but are not: ' &
         //            'TARGET = #; ILLUM = #.'                   )
         CALL ERRCH  ( '#', TARGET                                )
         CALL ERRCH  ( '#', ILLUM                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                 )
         CALL CHKOUT ( 'ZZGFILIN'                                 )
         RETURN

      END IF

!
!     Save the observer, target, and illumination source names.
!
      SVONAM = OBSRVR
      SVTNAM = TARGET
      SVINAM = ILLUM

!
!     Squeeze all blanks out of the aberration correction
!     string; ensure the string is in upper case.
!
      CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
      CALL UCASE  ( SVCORR,         SVCORR )

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      CALL ZZVALCOR ( SVCORR, SVABLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

!
!     Reject transmission corrections.
!
      IF ( SVABLK(XMTIDX) ) THEN

         CALL SETMSG ( 'Aberration correction was #; ' &
         //            'transmission corrections are not ' &
         //            'allowed by this routine.'        )
         CALL ERRCH  ( '#',  ABCORR                      )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'             )
         CALL CHKOUT ( 'ZZGFILIN'                        )
         RETURN

      END IF

!
!     Look up the radii for the target body.
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

!
!     Find the surface normal at the surface point. Create a
!     body-fixed state vector for the normal.
!
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

      CALL VEQU ( NORMAL, SVNRML )

!
!     Save the surface point in the body-fixed reference frame.
!
      CALL VEQU ( SPOINT, SVSSPT)

!
!     Save a left-justified, upper case copy of the computation method
!     for the illumination angles.
!
      CALL LJUST ( METHOD, SVMETH )
      CALL UCASE ( SVMETH, SVMETH )

      IF ( SVMETH .NE. 'ELLIPSOID' ) THEN

         CALL SETMSG ( 'The only supported computation method ' &
         //            'is ELLIPSOID; the input method was #.' )
         CALL ERRCH  ( '#',  METHOD                            )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                  )
         CALL CHKOUT ( 'ZZGFILIN'                              )
         RETURN

      END IF

!
!     Save a left-justified, upper case copy of the reference frame
!     name.
!
      CALL LJUST ( FIXREF, SVREF )
      CALL UCASE ( SVREF,  SVREF )

!
!     Look up the frame attributes; make sure the frame is centered
!     on the target body.
!
!
!     Determine the attributes of the frame designated by FIXREF.
!
      CALL NAMFRM ( FIXREF, FXFCDE )

      CALL FRINFO ( FXFCDE, FXCENT, FXCLSS, FXTYID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' // &
                       'the SPICE frame subsystem. Possibly '    // &
                       'a required frame definition kernel has ' // &
                       'not been loaded.'                        )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                     )
         CALL CHKOUT ( 'ZZGFILIN'                                )
         RETURN

      END IF

!
!     Make sure that FIXREF is centered at the target body's center.
!
      IF ( FXCENT .NE. SVTARG ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the ' &
         //            'target body #. The ID code of the frame ' &
         //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  FXCENT                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( 'ZZGFILIN'                                 )
         RETURN

      END IF

!
!     Save the index of the angle type.
!
      SVAIDX = ESRCHC ( ANGTYP, 3, ANGNMS )

      IF ( SVAIDX .EQ. 0 ) THEN

         CALL SETMSG ( 'Illumination angle type # is not recognized.' )
         CALL ERRCH  ( '#',  ANGTYP                                   )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                          )
         CALL CHKOUT ( 'ZZGFILIN'                                     )
         RETURN

      END IF


      CALL CHKOUT ( 'ZZGFILIN' )
      RETURN





!$Procedure ZZGFILDC ( GF, is illumination angle decreasing? )

      ENTRY ZZGFILDC ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     Indicate whether a specified illumination angle is decreasing at
!     a specified time.
!
! $ Required_Reading
!
!     GF
!     NAIF_IDS
!     SPK
!     TIME
!
! $ Keywords
!
!     ANGLE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!     EXTERNAL              UDFUNC
!     DOUBLE PRECISION      ET
!     LOGICAL               DECRES
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UDFUNC     I   Placeholder external routine argument.
!     ET         I   Ephemeris seconds past J2000 TDB.
!     DECRES     O   Flag indicating whether illumination angle is
!                    decreasing.
!
! $ Detailed_Input
!
!     UDFUNC   is a placeholder subroutine argument. This argument is
!              provided for compatibility with ZZGFSOLVX. It is not
!              used by the entry points of this package.
!
!     ET       is the time, expressed as seconds past J2000 TDB, at
!              which to determine whether or not the illumination
!              angle between the observer and target is decreasing.
!
! $ Detailed_Output
!
!     DECRES   is a logical flag that indicates whether the
!              observer-target illumination angle is decreasing at
!              ET. The observer, target, and aberration correction
!              used to compute the illumination angle are defined by
!              the latest call to the initialization entry point
!              ZZGFILIN.
!
!              DECRES is .TRUE. if and only if the observer-target
!              illumination angle is decreasing at ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the state of the target relative to the observer at ET can
!         not be found due to an SPK lookup failure, an error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFILU.
!
! $ Particulars
!
!     This routine is used by ZZGFRELX, and indirectly by GFILUM, to
!     determine the time intervals, within the confinement window, on
!     which the observer-target illumination angle is monotone
!     increasing or monotone decreasing.
!
! $ Examples
!
!     See GFILUM, GFEVNT, ZZGFRELX.
!
! $ Restrictions
!
!     1)  This is a SPICELIB private routine; it should not be called by
!         user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 29-MAY-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 08-MAR-2017 (NJB)
!
!        Bug fix: now checks FAILED after ZZILUSTA call.
!
!-    SPICELIB Version 1.0.0, 23-MAY-2012 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     indicate whether illumination angle is decreasing
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFILDC' )

!
!     Compute the rates of change of all of the illumination angles.
!
      CALL ZZILUSTA ( SVMETH, SVTNAM, SVINAM, ET, &
                      SVREF,  SVCORR, SVONAM, SVSSPT, &
                      SVNRML, PHSSTA, INCSTA, EMISTA  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILDC' )
         RETURN
      END IF

      IF ( SVAIDX .EQ. PHSIDX ) THEN

         RATE = PHSSTA(2)

      ELSE IF ( SVAIDX .EQ. INCIDX ) THEN

         RATE = INCSTA(2)

      ELSE IF ( SVAIDX .EQ. EMTIDX ) THEN

         RATE = EMISTA(2)

      ELSE
!
!        We should never get here.
!
         CALL SETMSG ( 'Unexpected value of SVAIDX: #.' )
         CALL ERRINT ( '#',  SVAIDX                     )
         CALL SIGERR ( 'SPICE(BUG)'                     )

      END IF
!
!     The observer-target illumination angle is decreasing if and only
!     the derivative of the angle with respect to time is negative.
!
      DECRES  =  RATE  .LT.  0.D0

      CALL CHKOUT ( 'ZZGFILDC' )
      RETURN



!$Procedure ZZGFILGQ ( GF, get illumination angle )

      ENTRY ZZGFILGQ ( ET, ANGLE )

! $ Abstract
!
!     Return the specified illumination angle at a specified epoch.
!
! $ Required_Reading
!
!     GF
!     NAIF_IDS
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!     GEOMETRY
!     ILLUMINATION ANGLE
!     SEARCH
!
! $ Declarations
!
!      DOUBLE PRECISION      ET
!      DOUBLE PRECISION      ANGLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     ANGLE      O   Illumination angle at time ET.
!
! $ Detailed_Input
!
!     ET       is the time, expressed as seconds past J2000 TDB, at
!              which the illumination angle is to be computed.
!
! $ Detailed_Output
!
!     ANGLE    is the illumination angle as seen by the observer at
!              time ET. The observer, target, and aberration
!              correction used to compute the illumination angle are
!              defined by the latest call to the initialization entry
!              point ZZGFILIN.
!
!              Units are radians.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the position of the target relative to the observer at ET
!         can not be found due to an SPK lookup failure, an error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFILU.
!
! $ Particulars
!
!     This routine determines the apparent illumination angle as seen
!     from the observer at time ET. This functionality supports GFREL's
!     comparisons of relative extrema in order to determine absolute
!     extrema.
!
! $ Examples
!
!     See GFREL.
!
! $ Restrictions
!
!     1)  This is a SPICELIB private routine; it should not be called by
!         user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 29-MAY-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 08-MAR-2017 (NJB)
!
!        Bug fix: now checks FAILED after ILLUMG call.
!
!-    SPICELIB Version 1.0.0, 23-MAY-2012 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     return illumination angle
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFILGQ' )

      CALL ILLUMG ( SVMETH, SVTNAM,    SVINAM,    ET,      SVREF, &
                    SVCORR, SVONAM,    SVSSPT,    ETTARG, &
                    SRFVEC, ANGLES(1), ANGLES(2), ANGLES(3)       )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILGQ' )
         RETURN
      END IF

      ANGLE = ANGLES( SVAIDX )

      CALL CHKOUT ( 'ZZGFILGQ' )
      RETURN
      END
