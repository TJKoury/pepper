!$Procedure ZZSEPQ ( Separation quantity from observer )

      DOUBLE PRECISION FUNCTION ZZSEPQ ( ET, &
                                         BOD1, BOD2, &
                                         R1,   R2, &
                                         OBS,  ABCORR, REF )

! $ Abstract
!
!     Compute the angular separation between two spherical or
!     point objects.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      INTEGER               BOD1
      INTEGER               BOD2
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      INTEGER               OBS
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         REF

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB
!     BOD1       I   NAIF ID for first target
!     BOD2       I   NAIF ID for second target
!     R1         I   Radius of a spherical model for BOD1
!     R2         I   Radius of a spherical model for BOD2
!     OBS        I   NAIF ID of observer
!     ABCORR     I   Aberration correction flag
!     REF        I   Reference frame of the angular separation
!
!     The function returns the angular separation between two bodies,
!     BOD1 and BOD2, with radius R1 and R2 respectively, as seen from
!     an observer OBS, possibly corrected for aberration corrections.
!
! $ Detailed_Input
!
!     ET       is the time in ephemeris seconds past J2000 TDB at
!              which the separation is to be measured.
!
!     BOD1
!     BOD2     the NAIF IDs of the two objects for which to
!              determine the angular separation.
!
!     R1
!     R2       are the radii of the two objects BOD1 and BOD2
!              respectively.
!
!     OBS      the NAIF ID identifying the body observing
!              BOD1 and BOD2.
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
!                 'LT'       "Reception" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case: converged
!                            Newtonian light time correction.
!
!                 'CN+S'     "Reception" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case: converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 The ABCORR string lacks sensitivity to case, leading
!                 and trailing blanks.
!
!     REF      is the name of the reference frame relative to which
!              the angular separation should be expressed. This may
!              be any frame supported by the SPICE system, including
!              built-in frames (documented in the Frames Required
!              Reading) and frames defined by a loaded frame kernel.
!
!              When REF designates a non-inertial frame, the
!              orientation of the frame is evaluated at an epoch
!              dependent on the selected aberration correction.
!
! $ Detailed_Output
!
!     The function returns the light-time (and stellar aberration
!     corrected if this feature is enabled) separation of the two
!     objects BOD1 and BOD2 as observed from OBS.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The kernel files to be used by this routine must be loaded
!         (normally using the SPICELIB routine FURNSH) before this
!         routine is called.
!
!     2)  If either R1 or R2 have a negative value, the error
!         SPICE(BADRADIUS) is signaled.
!
!     3)  If the ephemeris data required to perform the needed state
!         look-ups are not loaded, an error is signaled by a routine in
!         the call tree of this routine.
!
!     4)  If R1 < RANGE1 or R2 < RANGE2, the error SPICE(INSIDEBODY)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine determines the apparent separation between the limbs
!     of two objects as observed from a third. The value reported is
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
!     J. Diaz del Rio    (ODC Space)
!     L.S. Elson         (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 16-JAN-2021 (EDW) (JDR)
!
!        Updated the header to comply with NAIF standard.
!
!        Routine made public, implemented as a function, renamed
!        ZZSEPQ from ZZGFSPQ.
!
!        Added an error check for observer to target ranges
!        less-than target body radii.
!
!-    SPICELIB Version 1.0.1, 28-JUN-2012 (EDW)
!
!        Minor edits to header, correcting spelling error and improving
!        syntax of $Exceptions.
!
!        Corrected a typo in SETMSG error message where BOD1 was quoted
!        twice instead of quoting BOD1 and BOD2. This edit does change
!        the error message output.
!
!-    SPICELIB Version 1.0.0, 03-MAR-2009 (EDW) (LSE) (WLT)
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
!     Set an initial value to return in case of error.
!
      ZZSEPQ = 0.D0

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZSEPQ' )

!
!     First check for bad inputs.
!
      IF ( (R1 .LT. 0.D0) .OR. (R2 .LT. 0.D0) ) THEN

         CALL SETMSG  ('A negative radius for a body was ' &
         //            'encountered. The radius for body #1 was given ' &
         //            'as #2, the radius of body #3 was given as #4.')

         CALL ERRINT  ('#1', BOD1         )
         CALL ERRDP   ('#2', R1            )
         CALL ERRINT  ('#3', BOD2         )
         CALL ERRDP   ('#4', R2            )
         CALL SIGERR  ('SPICE(BADRADIUS)' )
         CALL CHKOUT  ('ZZSEPQ'          )
         RETURN
      END IF


!
!     Get the state of the BOD1, BOD2 objects relative to OBS.
!
      CALL SPKEZP ( BOD1, ET, REF, ABCORR,  OBS, PV1, LT )

      IF  (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZSEPQ' )
         RETURN
      END IF


       CALL SPKEZP ( BOD2, ET, REF, ABCORR, OBS, PV2, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZSEPQ' )
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
            CALL CHKOUT ( 'ZZSEPQ' )
            RETURN
         END IF

      ELSE

         CALL SETMSG  ('Observer object #1 located within surface ' &
                     //'of target 1 object #2. Range to target 1 #3, ' &
                     //'radius of target 1 #4')

         CALL ERRINT  ('#1', OBS           )
         CALL ERRINT  ('#2', BOD1         )
         CALL ERRDP   ('#3', RANGE1        )
         CALL ERRDP   ('#4', R1             )
         CALL SIGERR  ('SPICE(INSIDEBODY)' )
         CALL CHKOUT  ('ZZSEPQ'             )
         RETURN

      END IF

      IF ( RANGE2 .GT. R2 ) THEN

         ANG2 = DASINE ( R2/RANGE2, ATOL )

         IF (  FAILED() ) THEN
            CALL CHKOUT ('ZZSEPQ')
            RETURN
         END IF

      ELSE

         CALL SETMSG  ('Observer object #1 located within surface ' &
                     //'of target 2 object #2. Range to target 2 #3, ' &
                     //'radius of target 2 #4')

         CALL ERRINT  ('#1', OBS           )
         CALL ERRINT  ('#2', BOD2         )
         CALL ERRDP   ('#3', RANGE2        )
         CALL ERRDP   ('#4', R2             )
         CALL SIGERR  ('SPICE(INSIDEBODY)' )
         CALL CHKOUT  ('ZZSEPQ'             )
         RETURN

      END IF

!
!     Finally compute the apparent separation.
!
      THETA = VSEP( PV1, PV2 )
      ZZSEPQ = THETA - ANG1 - ANG2

      CALL CHKOUT ('ZZSEPQ')
      RETURN
      END
