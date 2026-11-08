!$Procedure T_ILUFUN ( TSPICE illumination angle functions )

      DOUBLE PRECISION FUNCTION T_ILUFUN ( METHOD, TARGET, ILLUM, &
                                           ET,     FIXREF, ABCORR, &
                                           OBSRVR, SPOINT, NORMAL )

! $ Abstract
!
!     This is an umbrella routine for functions that return
!     illumination angles.
!
!     This is a TSPICE utility that supports F_ZZILUSTA.
!
! $ Required_Reading
!
!     GF
!     PCK
!     TIME
!     SPK
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLUM
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )

! $ Brief_I/O
!
!     Variable  I/O  Entry point
!     --------  ---  --------------------------------------------------
!     METHOD     I   T_ILUINI
!     TARGET     I   T_ILUINI
!     ILLUM      I   T_ILUINI
!     ET         I   T_ILUPHS, T_ILUINC, T_ILUEMI
!     FIXREF     I   T_ILUINI
!     ABCORR     I   T_ILUINI
!     OBSRVR     I   T_ILUINI
!     SPOINT     I   T_ILUINI
!     NORMAL     I   T_ILUINI
!
! $ Detailed_Input
!
!     See the headers of the entry points.
!
! $ Detailed_Output
!
!     See the headers of the entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     See ILLUMG.
!
! $ Particulars
!
!     This is a specialized routine intended for use only
!     by F_ZZILUSTA.
!
!     This suite of routines is intended to be used as follows:
!
!        1) The calling test program should first call
!
!              T_ILUINI
!
!           to store the inputs, other than time, that will
!           be used to compute illumination angles.
!
!        2) Call any of the entry points
!
!              T_ILUPHS
!              T_ILUINC
!              T_ILUEMI
!
!           to compute illumination angles at a time of interest. There
!           is no need to call T_ILUINI between calls to these routines
!           unless any of the inputs, other than time, have changed.
!
! $ Examples
!
!     See usage in F_ZZILUSTA.
!
! $ Restrictions
!
!     1) This routine is intended for use only by the GF subsystem.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
!
!-&

!
!     Entry points
!
      DOUBLE PRECISION      T_ILUINI
      DOUBLE PRECISION      T_ILUPHS
      DOUBLE PRECISION      T_ILUINC
      DOUBLE PRECISION      T_ILUEMI

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 10 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 50 )

!
!     Local variables
!
      CHARACTER*(CORLEN)    SVCORR
      CHARACTER*(BDNMLN)    SVILUM
      CHARACTER*(MTHLEN)    SVMETH
      CHARACTER*(BDNMLN)    SVOBS
      CHARACTER*(FRNMLN)    SVREF
      CHARACTER*(BDNMLN)    SVTARG

      DOUBLE PRECISION      LOCEMI ( 2 )
      DOUBLE PRECISION      LOCINC ( 2 )
      DOUBLE PRECISION      LOCPHS ( 2 )
      DOUBLE PRECISION      SVNRML ( 3 )
      DOUBLE PRECISION      SVPNT  ( 3 )

!
!     Saved variables
!
      SAVE                  SVCORR
      SAVE                  SVILUM
      SAVE                  SVMETH
      SAVE                  SVNRML
      SAVE                  SVOBS
      SAVE                  SVPNT
      SAVE                  SVREF
      SAVE                  SVTARG


      CALL CHKIN  ( 'T_ILUFUN'          )

      T_ILUFUN = 0.D0

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_ILUFUN'          )
      RETURN



!$Procedure T_ILUINI ( TSPICE: initialize entry points. )

      ENTRY T_ILUINI ( METHOD, TARGET, ILLUM,  FIXREF, &
                       ABCORR, OBSRVR, SPOINT, NORMAL )

! $ Abstract
!
!     Initialize the illumination angle functions in this
!     suite of test routines: store inputs to ZZILUMIN.
!
!     This is a TSPICE utility that supports F_ZZILUSTA.
!
! $ Required_Reading
!
!     GF
!     PCK
!     TIME
!     SPK
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         METHOD
!     CHARACTER*(*)         TARGET
!     CHARACTER*(*)         ILLUM
!     CHARACTER*(*)         FIXREF
!     CHARACTER*(*)         ABCORR
!     CHARACTER*(*)         OBSRVR
!     DOUBLE PRECISION      SPOINT ( 3 )
!     DOUBLE PRECISION      NORMAL ( 3 )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TARGET     I   Name of target body associated with surface point.
!     ILLUM      I   Name of illumination source.
!     FIXREF     I   Body-fixed reference frame.
!     ABCORR     I   Aberration correction.
!     OBSRVR     I   Name of observer.
!     SPOINT     I   Surface point.
!     NORMAL     I   Outward normal vector at surface point.
!
! $ Detailed_Input
!
!     METHOD         is a string specifying the computation method to
!                    be used by this routine. The only value currently
!                    allowed is 'ELLIPSOID'. This indicates that the
!                    target body shape is modeled as an ellipsoid.
!
!                    Case and leading and trailing blanks are not
!                    significant in METHOD.
!
!     TARGET         is the name of the target body associated with the
!                    surface point SPOINT. TARGET may be a body name or
!                    an ID code provided as as string.
!
!     ILLUM          is the name of the illumination source used to
!                    define the illumination angles computed by this
!                    routine. ILLUM may be a body name or an ID code
!                    provided as as string.
!
!     FIXREF         is the name of the body-centered, body-fixed
!                    reference frame relative to which SPOINT is
!                    specified. The frame's center must coincide with
!                    TARGET.
!
!     ABCORR         indicates the aberration corrections to be
!                    applied. Only reception corrections are supported.
!                    See the header of ILUMIN for a discussion of
!                    aberration corrections used in illumination angle
!                    computations.
!
!     OBSRVR         is the name of the observing body. OBSRVR may be a
!                    body name or an ID code provided as as string.
!
!     SPOINT         is a 3-vector containing the cartesian coordinates
!                    of the surface point at which the illumination
!                    angle rates are to be computed. SPOINT is
!                    expressed in the body-centered, body-fixed frame
!                    designated by FIXREF (see description above).
!
!                    Units are km.
!
!     NORMAL         is an outward normal vector to be used for
!                    emission angle and illumination source incidence
!                    angle calculations. NORMAL should be orthogonal
!                    at SPOINT to the target body's surface.
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
!     None.
!
! $ Files
!
!     See ILLUMG.
!
! $ Particulars
!
!     This routine stores inputs required by ZZILUSTA. The stored inputs
!     are used by the other entry points to compute illumination angles
!     a specified times.
!
! $ Examples
!
!     See usage in T_DZZILU.
!
! $ Restrictions
!
!     1) This is a specialized routine intended for use only
!         by F_ZZILUSTA.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
!
!-&

!
!     Give this function a return value.
!
      T_ILUINI = 0.D0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'T_ILUINI' )

      SVMETH = METHOD
      SVTARG = TARGET
      SVILUM = ILLUM
      SVREF  = FIXREF
      SVCORR = ABCORR
      SVOBS  = OBSRVR

      CALL VEQU ( SPOINT, SVPNT  )
      CALL VEQU ( NORMAL, SVNRML )

      CALL CHKOUT ( 'T_ILUINI' )
      RETURN



!$Procedure T_ILUPHS ( TSPICE: Compute phase angle )

      ENTRY T_ILUPHS ( ET )

! $ Abstract
!
!     Compute the phase angle at ET.
!
!     This is a TSPICE utility that supports F_ZZILUSTA.
!
! $ Required_Reading
!
!     GF
!     PCK
!     TIME
!     SPK
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch at which to compute phase angle.
!
!     The function returns the phase angle at ET defined by
!     the stored inputs.
!
! $ Detailed_Input
!
!     ET             is the epoch at which a phase angle is
!                    to be computed. ET is expressed as seconds
!                    past J2000 TDB.
!
! $ Detailed_Output
!
!     The function returns the phase angle at ET defined by
!     the inputs saved by the last call to T_ILUINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     See the Exceptions section of the header of ZZILUSTA.
!
! $ Files
!
!     See ILLUMG.
!
! $ Particulars
!
!     This routine uses ZZILUSTA to compute a specified phase angle
!     at a given time. The inputs other than time are saved values
!     stored by the last call to T_ILUINI.
!
!     See ILLUMG for a description of this angle.
!
! $ Examples
!
!     See usage in T_DZZILU.
!
! $ Restrictions
!
!     1) This is a specialized routine intended for use only
!         by F_ZZILUSTA.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
!
!-&

      T_ILUPHS = 0.D0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ILUPHS' )

      CALL ZZILUSTA ( SVMETH, SVTARG, SVILUM, ET, &
                      SVREF,  SVCORR, SVOBS,  SVPNT, &
                      SVNRML, LOCPHS, LOCINC, LOCEMI  )

      T_ILUPHS = LOCPHS(1)

      CALL CHKOUT ( 'T_ILUPHS' )
      RETURN




!$Procedure T_ILUINC ( TSPICE: Compute incidence angle )

      ENTRY T_ILUINC ( ET )

! $ Abstract
!
!     Compute the illumination source incidence angle at ET.
!
!     This is a TSPICE utility that supports F_ZZILUSTA.
!
! $ Required_Reading
!
!     GF
!     PCK
!     TIME
!     SPK
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch at which to compute incidence angle.
!
!     The function returns the illumination source angle at ET defined
!     by the stored inputs.
!
! $ Detailed_Input
!
!     ET             is the epoch at which the illumination source
!                    incidence angle is to be computed. ET is expressed
!                    as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     The function returns the illumination source incidence angle at
!     ET defined by the inputs saved by the last call to T_ILUINI.
!
!     See ILLUMG for a description of this angle.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     See the Exceptions section of the header of ZZILUSTA.
!
! $ Files
!
!     See ILLUMG.
!
! $ Particulars
!
!     This routine uses ZZILUSTA to compute a specified illumination
!     source incidence angle at a given time. The inputs other than
!     time are saved values stored by the last call to T_ILUINI.
!
! $ Examples
!
!     See usage in T_DZZILU.
!
! $ Restrictions
!
!     1) This is a specialized routine intended for use only
!         by F_ZZILUSTA.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
!
!-&

      T_ILUINC = 0.D0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ILUINC' )

      CALL ZZILUSTA ( SVMETH, SVTARG, SVILUM, ET, &
                      SVREF,  SVCORR, SVOBS,  SVPNT, &
                      SVNRML, LOCPHS, LOCINC, LOCEMI  )

      T_ILUINC = LOCINC(1)

      CALL CHKOUT ( 'T_ILUINC' )
      RETURN




!$Procedure T_ILUEMI ( TSPICE: Compute emission angle )

      ENTRY T_ILUEMI ( ET )

! $ Abstract
!
!     Compute the emission angle at ET.
!
!     This is a TSPICE utility that supports F_ZZILUSTA.
!
! $ Required_Reading
!
!     GF
!     PCK
!     TIME
!     SPK
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch at which to compute emission angle.
!
!     The function returns the emission angle at ET defined
!     by the stored inputs.
!
! $ Detailed_Input
!
!     ET             is the epoch at which the emission angle is
!                    to be computed. ET is expressed as seconds
!                    past J2000 TDB.
!
! $ Detailed_Output
!
!     The function returns the emission angle at ET defined by the
!     inputs saved by the last call to T_ILUINI.
!
!     See ILLUMG for a description of this angle.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     See the Exceptions section of the header of ZZILUSTA.
!
! $ Files
!
!     See ILLUMG.
!
! $ Particulars
!
!     This routine uses ZZILUSTA to compute a specified emission angle
!     at a given time. The inputs other than time are saved values
!     stored by the last call to T_ILUINI.
!
! $ Examples
!
!     See usage in T_DZZILU.
!
! $ Restrictions
!
!     1) This is a specialized routine intended for use only
!         by F_ZZILUSTA.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
!
!-&

      T_ILUEMI = 0.D0


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ILUEMI' )

      CALL ZZILUSTA ( SVMETH, SVTARG, SVILUM, ET, &
                      SVREF,  SVCORR, SVOBS,  SVPNT, &
                      SVNRML, LOCPHS, LOCINC, LOCEMI  )

      T_ILUEMI = LOCEMI(1)

      CALL CHKOUT ( 'T_ILUEMI' )
      RETURN

      END
