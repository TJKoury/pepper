!$Procedure ZZGFSPQ ( GF, separation quantity )

      SUBROUTINE ZZGFSPQ ( ET, TARG1, TARG2, R1, R2, &
                           OBS, ABCORR, REF, VALUE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute the angular separation between the limbs of two objects.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     GEOMETRY
!     ANGLE
!
! $ Declarations

      IMPLICIT NONE
      DOUBLE PRECISION      ET
      INTEGER               TARG1
      INTEGER               TARG2
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      INTEGER               OBS
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         REF
      DOUBLE PRECISION      VALUE


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB
!     TARG1      I   NAIF ID for first target
!     TARG2      I   NAIF ID for second target
!     R1         I   Radius of a spherical model for TARG1
!     R2         I   Radius of a spherical model for TARG2
!     OBS        I   NAIF ID of observer
!     ABCORR     I   Aberration correction flag
!     REF        I   Reference frame of the angular separation
!     VALUE      O   Value of angular separation between objects
!
! $ Detailed_Input
!
!     ET       is the time in ephemeris seconds past J2000 TDB at
!              which the separation is to be measured.
!
!     TARG1
!     TARG2    the NAIF IDs of the two objects for which to
!              determine the angular separation.
!
!     R1
!     R2       are the radii of the two objects TARG1 and TARG2
!              respectively.
!
!     OBS      the NAIF ID identifying the body observing
!              TARG1 and TARG2.
!
!     ABCORR   the string description of the aberration corrections
!              to apply to the state evaluations to account for
!              one-way light time and stellar aberration.
!
!              This routine accepts the same aberration corrections
!              as does the SPICE routine SPKEZR. See the header of
!              SPKEZR for a detailed description of the aberration
!              correction options. For convenience, the options are
!              listed below:
!
!                 'NONE'     Apply no correction.
!
!                 'LT'       "Reception" case:  correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case:  correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case:  converged
!                            Newtonian light time correction.
!
!                 'CN+S'     "Reception" case:  converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case:  correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case:  correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case:  converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case:  converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 The ABCORR string lacks sensitivity to case, leading
!                 and trailing blanks.
!
!     REF         is the name of the reference frame relative to which
!                 the angular separation should be expressed. This may
!                 be any frame supported by the SPICE system, including
!                 built-in frames (documented in the Frames Required
!                 Reading) and frames defined by a loaded frame kernel.
!
!                 When REF designates a non-inertial frame, the
!                 orientation of the frame is evaluated at an epoch
!                 dependent on the selected aberration correction.
!
! $ Detailed_Output
!
!     VALUE   is the light-time (and stellar aberration corrected
!             if this feature is enabled) separation of the two
!             objects TARG1 and TARG2 as observed from OBS.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(BADRADIUS) signals if either R1 or R2 have a negative
!        value.
!
!     2) If the ephemeris data required to perform the needed state
!        look-ups are not loaded, routines called by this routine
!        will signal the error SPICE(SPKINSUFFDATA).
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine determines the apparent separation between the limbs
!     of two objects as observed from a third.  The value reported is
!     corrected for light time. Moreover, if at the time this routine
!     is called, stellar aberration corrections are enabled, this
!     correction will also be applied to the apparent positions of the
!     centers of the two objects.
!
! $ Examples
!
!     None.
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
!     W.L. Taber     (JPL)
!     L.S. Elson     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 28-JUN-2012 (EDW)
!
!        Minor edits to header, correcting spelling error and improving
!        syntax of Exceptions.
!
!        Corrected a typo in SETMSG error message where TARG1 was quoted
!        twice instead of quoting TARG1 and TARG2. This edit does change
!        the error message output.
!
!-    SPICELIB Version 1.0.0, 03-MAR-2009 (EDW)
!
!-&

! $ Index_Entries
!
!     compute the apparent relative angular separation
!
!-&

!
!     SPICELIB functions.
!
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP
      LOGICAL               FAILED
      LOGICAL               RETURN


!
!     Local Variables.
!
      DOUBLE PRECISION      ANG1
      DOUBLE PRECISION      ANG2
      DOUBLE PRECISION      PV1    ( 3 )
      DOUBLE PRECISION      PV2    ( 3 )
      DOUBLE PRECISION      RANGE1
      DOUBLE PRECISION      RANGE2
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      LT

!
!     ATOL is a tolerance value for computing arc sine.
!

      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL  = 1.D-12 )


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZGFSPQ' )

!
!     First check for bad inputs.
!
      IF ( R1 .LT. 0.D0 .OR. R2 .LT. 0.D0 ) THEN

         CALL SETMSG  ('A negative radius for a body was ' &
         //            'encountered. The radius for body # was given ' &
         //            'as #, the radius of body # was given as #. ')

         CALL ERRINT  ('#', TARG1         )
         CALL ERRDP   ('#', R1            )
         CALL ERRINT  ('#', TARG2         )
         CALL ERRDP   ('#', R2            )
         CALL SIGERR  ('SPICE(BADRADIUS)' )
         CALL CHKOUT  ('ZZGFSPQ'          )
         RETURN
      END IF


!
!     Get the state of the TARG1, TARG2 objects relative to OBS.
!
      CALL SPKEZP ( TARG1, ET, REF, ABCORR,  OBS, PV1, LT )

      IF  (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFSPQ' )
         RETURN
      END IF


       CALL SPKEZP ( TARG2, ET, REF, ABCORR, OBS, PV2, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFSPQ' )
         RETURN
      END IF

!
!     Compute the range to the objects of interest.
!
      RANGE1 = VNORM( PV1 )
      RANGE2 = VNORM( PV2 )

!
!     Compute the apparent angular radii as seen from OBS.
!
      IF ( RANGE1 .GT. R1 ) THEN

         ANG1 = DASINE( R1/RANGE1, ATOL )

         IF (  FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFSPQ' )
            RETURN
         END IF

      ELSE

         ANG1 = HALFPI ()

      END IF

      IF ( RANGE2 .GT. R2 ) THEN

         ANG2 = DASINE ( R2/RANGE2, ATOL )

         IF (  FAILED() ) THEN
            CALL CHKOUT ('ZZGFSPQ')
            RETURN
         END IF

      ELSE

         ANG2 = HALFPI()

      END IF

!
!     Finally compute the apparent separation.
!
      THETA = VSEP ( PV1, PV2 )
      VALUE = THETA - ANG1 - ANG2

      CALL CHKOUT ('ZZGFSPQ')
      RETURN
      END
