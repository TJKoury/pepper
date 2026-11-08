!$Procedure ZZILUSTA ( Illumination angle states )

      SUBROUTINE ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                            FIXREF, ABCORR, OBSRVR, SPOINT, &
                            NORMAL, PHSSTA, INCSTA, EMISTA  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute illumination angles and their rates of change at a
!     surface point.
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

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLUM
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PHSSTA ( 2 )
      DOUBLE PRECISION      INCSTA ( 2 )
      DOUBLE PRECISION      EMISTA ( 2 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TARGET     I   Name of target body associated with surface point.
!     ILLUM      I   Name of illumination source.
!     ET         I   Observation epoch (TDB).
!     FIXREF     I   Body-fixed reference frame.
!     ABCORR     I   Aberration correction.
!     OBSRVR     I   Name of observer.
!     SPOINT     I   Surface point.
!     NORMAL     I   Outward normal vector at surface point.
!     PHSSTA     O   Phase angle state.
!     INCSTA     O   Solar incidence angle state.
!     EMISTA     O   Emission angle state.
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
!     ET             is the observation time. ET is expressed as
!                    seconds past J2000 TDB.
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
!                    angle states are to be computed. SPOINT is
!                    expressed in the body-centered, body-fixed frame
!                    designated by FIXREF (see description above).
!
!                    Units are km.
!
!     NORMAL         is an outward normal vector to be used for
!                    emission angle and solar incidence angle
!                    calculations. NORMAL should be orthogonal to the
!                    plane tangent at SPOINT to the target body's
!                    surface.
!
! $ Detailed_Output
!
!     PHSSTA         is the phase angle and its rate of change with
!                    respect to TDB, evaluated at ET.
!
!     INCSTA         is the solar incidence angle and its rate of
!                    change with respect to TDB, evaluated at ET.
!
!     EMISTA         is the emission angle and its rate of change with
!                    respect to TDB, evaluated at ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the computation method is not recognized, the error
!         SPICE(INVALIDMETHOD) is signaled.
!
!     2)  If ABCORR specifies a transmission aberration correction,
!         the error SPICE(INVALIDOPTION) is signaled.
!
!     3)  If an error occurs while looking up a state vector, the
!         error will be signaled by a routine in the call tree of
!         this routine.
!
!     4)  If the input normal vector is zero, the error
!         SPICE(ZEROVECTOR) is signaled.
!
! $ Files
!
!     See GFILUM.
!
! $ Particulars
!
!     The term "state" used in the name of this routine refers to
!     the combination of a function and its derivative with respect
!     to time.
!
!     This routine centralizes computation of illumination angles and
!     their rates of change. It also exposes the illumination angle
!     rates of change used by the GF system in order to allow these
!     rates to be tested using the TSPICE system.
!
!     See the SPICELIB routine ILUMIN for a description of the
!     illumination angles computed by this routine.
!
! $ Examples
!
!     See usage in ZZGFILDC.
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
!-    SPICELIB Version 1.0.0, 02-APR-2012 (NJB)
!
!       Previous version was dated 21-MAR-2012
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VSEP

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
      CHARACTER*(*)         IREF
      PARAMETER           ( IREF = 'J2000' )

!
!     Local variables
!
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      ETSURF
      DOUBLE PRECISION      FXNSTA ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTSRC
      DOUBLE PRECISION      NRMSTA ( 6 )
      DOUBLE PRECISION      OBSSTA ( 6 )
      DOUBLE PRECISION      SRCSTA ( 6 )
      DOUBLE PRECISION      STARG  ( 6 )
      DOUBLE PRECISION      TMPXFM ( 6, 6 )
      DOUBLE PRECISION      UVEC   ( 3 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               USELT
      LOGICAL               XMIT


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZILUSTA' )

!
!     For now, only ellipsoids are supported as target shapes.
!
      IF (  .NOT.  EQSTR( METHOD, 'ELLIPSOID' )  ) THEN

         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( 'ZZILUSTA'                                      )
         RETURN

      END IF

!
!     Reject zero normal vectors.
!
      IF ( VZERO(NORMAL) ) THEN

         CALL SETMSG ( 'The input normal vector must not be zero, ' &
         //            'but sadly, it was.'                       )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                        )
         CALL CHKOUT ( 'ZZILUSTA'                                 )
         RETURN

      END IF

!
!     Look up the state of the target with respect to the
!     observer. We'll represent the state in an inertial
!     reference frame.
!
      CALL SPKCPT ( SPOINT,   TARGET, FIXREF, ET,    IREF, &
                    'TARGET', ABCORR, OBSRVR, STARG, LT    )

!
!     Compute the epoch associated with the surface point.
!
      CALL ZZCOREPC ( ABCORR, ET, LT, ETSURF )

!
!     Now let the surface point be the observer, let the observation
!     epoch be ETSURF, and find the apparent state of the illumination
!     source as seen from the surface point.
!
      CALL SPKCPO ( ILLUM,  ETSURF, IREF,   'OBSERVER', ABCORR, &
                    SPOINT, TARGET, FIXREF, SRCSTA,     LTSRC  )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZILUSTA' )
         RETURN
      END IF

!
!     We will need to transform the state of the normal vector to
!     the inertial frame. The epoch at which the transformation must be
!     evaluated is that associated with the surface point.

      CALL SXFORM ( FIXREF, IREF, ETSURF, XFORM )

!
!     Correct the body-fixed to inertial frame transformation for the
!     rate of change with respect to ET of observer-surface point light
!     time, if light time corrections are used.
!
!     Start out by parsing ABCORR.
!
      CALL ZZVALCOR ( ABCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZILUSTA' )
         RETURN
      END IF

      USELT = ATTBLK( LTIDX  )
      XMIT  = ATTBLK( XMTIDX )

      IF ( XMIT ) THEN

         CALL SETMSG ( 'Aberration correction # is for transmission; ' &
         //            'only reception corrections are supported by ' &
         //            'this routine.'                                )
         CALL ERRCH  ( '#', ABCORR                                    )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                         )
         CALL CHKOUT ( 'ZZILUSTA' )
         RETURN

      END IF


      IF ( USELT ) THEN
!
!        Compute the rate of change with respect to ET of the
!        observer-surface point light time. This rate is the range rate
!        divided by the speed of light.
!
         CALL VHAT ( STARG, UVEC )

         DLT = VDOT ( STARG(4), UVEC ) / CLIGHT()
!
!        Correct the state transformation.
!
         CALL ZZCORSXF ( .FALSE., DLT, XFORM, TMPXFM )

         CALL MOVED ( TMPXFM, 36, XFORM )

      END IF

!
!     Create a body-fixed state vector for the normal vector.
!     Convert the normal vector to unit length for safety.
!
      CALL VHAT   ( NORMAL, FXNSTA    )
      CALL CLEARD ( 3,      FXNSTA(4) )

!
!     Transform the state of the normal vector to the inertial
!     frame.
!
      CALL MXVG ( XFORM,  FXNSTA, 6, 6, NRMSTA )

!
!     We also must adjust the state of the illumination source for the
!     rate of change with respect to ET of the observer-surface point
!     light time. The velocity portion of the state we've computed is
!     the derivative with respect to ETSURF (time at the surface point)
!     of the surface point-illumination source vector. We must convert
!     this to a derivative with respect to ET.
!
!     This code assumes reception corrections.
!
      IF ( USELT ) THEN
!
!        ETSURF = ET - LT, so
!
!        d(ETSURF) / d(ET) = ( 1 - DLT )
!
         CALL VSCLIP ( 1.D0 - DLT,  SRCSTA(4) )

      END IF

!
!     The surface-point observer state we wish to use is the negative
!     of the observer-surface point state.
!
      CALL VMINUG ( STARG, 6, OBSSTA )

!
!     Compute the state (value and rate of change )
!     of the phase angle.
!
      PHSSTA( 1 ) =  VSEP  ( OBSSTA, SRCSTA )
      PHSSTA( 2 ) =  DVSEP ( OBSSTA, SRCSTA )

!
!     Compute the state of the illumination source
!     incidence angle.
!
      INCSTA( 1 ) =  VSEP  ( NRMSTA, SRCSTA )
      INCSTA( 2 ) =  DVSEP ( NRMSTA, SRCSTA )

!
!     Compute the state of the emission angle.
!
      EMISTA( 1 ) =  VSEP  ( NRMSTA, OBSSTA )
      EMISTA( 2 ) =  DVSEP ( NRMSTA, OBSSTA )


      CALL CHKOUT ( 'ZZILUSTA' )
      RETURN
      END


