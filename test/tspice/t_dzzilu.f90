!$Procedure T_DZZILU ( Illumination angle rates )

      SUBROUTINE T_DZZILU ( METHOD, TARGET, ILLUM,  ET, &
                            FIXREF, ABCORR, OBSRVR, SPOINT, &
                            NORMAL, DPHASE, DINCDN, DEMSSN  )

! $ Abstract
!
!     Compute by numeric methods the derivatives with respect to
!     time of illumination angles.
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
      DOUBLE PRECISION      DPHASE
      DOUBLE PRECISION      DINCDN
      DOUBLE PRECISION      DEMSSN

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
!     DPHASE     O   Phase angle rate.
!     DINCDN     O   Illumination source incidence angle rate.
!     DEMSSN     O   Emission angle rate.
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
!     DPHASE         is the rate of change of the phase angle with
!                    respect to TDB, evaluated at ET.
!
!     DINCDN         is the rate of change of the illumination source
!                    incidence angle with respect to TDB, evaluated at
!                    ET.
!
!     DEMSSN         is the rate of change of the emission angle with
!                    respect to TDB, evaluated at ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the computation method is not recognized, the error
!         will be signaled by a routine in the call tree of this
!         routine.
!
!     2)  If ABCORR specifies a transmission aberration correction, the
!         error will be signaled by a routine in the call tree of this
!         routine.
!
!     3)  If an error occurs while looking up a state vector, the
!         error will be signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See GFILUM.
!
! $ Particulars
!
!     This is a specialized routine intended for use only
!     by F_ZZILUSTA.
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
!-    SPICELIB Version 2.1.0, 27-AUG-2013 (NJB)
!
!        Bug fix: T_ILUINI is now called using function
!        call syntax.
!
!-    SPICELIB Version 2.0.0, 09-APR-2012 (NJB)
!
!-&


!
!     External routines
!
      DOUBLE PRECISION      T_ILUINI
      EXTERNAL              T_ILUINI

      DOUBLE PRECISION      T_ILUPHS
      EXTERNAL              T_ILUPHS

      DOUBLE PRECISION      T_ILUINC
      EXTERNAL              T_ILUINC

      DOUBLE PRECISION      T_ILUEMI
      EXTERNAL              T_ILUEMI

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      DOUBLE PRECISION      DELTA
      PARAMETER           ( DELTA = 240.D0 )

!
!     Local variables
!
      DOUBLE PRECISION      F
      DOUBLE PRECISION      RETVAL

      INTEGER               NTERMS
      INTEGER               NRTAIN


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'T_DZZILU' )

!
!     Initialize angle computation functions:
!     store inputs to ZZILUMIN.
!
      RETVAL =  T_ILUINI ( METHOD, TARGET, ILLUM,  FIXREF, &
                           ABCORR, OBSRVR, SPOINT, NORMAL )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'T_DZZILU' )
         RETURN
      END IF

!
!     Set the number of terms in the Chebyshev expansions
!     we'll use. Set the number of retained terms as well.
!
      NTERMS = 25
      NRTAIN = 6
!
!     Estimate the derivatives of the illumination angles
!     at ET.
!
      CALL T_DCHEB ( T_ILUPHS, ET, DELTA, NTERMS, NRTAIN, F, DPHASE )
      CALL T_DCHEB ( T_ILUINC, ET, DELTA, NTERMS, NRTAIN, F, DINCDN )
      CALL T_DCHEB ( T_ILUEMI, ET, DELTA, NTERMS, NRTAIN, F, DEMSSN )

      CALL CHKOUT ( 'T_DZZILU' )
      RETURN
      END
