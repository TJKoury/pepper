!$Procedure ZZGFSPQ ( GF, separation quantity )

      subroutine zzgfspq ( et, targ1, targ2, r1, r2, &
                           obs, abcorr, ref, value )
      use pepper_kinds, only: ip, wp

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

      implicit none
      real(wp)              et
      integer(ip)           targ1
      integer(ip)           targ2
      real(wp)              r1
      real(wp)              r2
      integer(ip)           obs
      character(*)          abcorr
      character(*)          ref
      real(wp)              value

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
      real(wp)              dasine
      real(wp)              halfpi
      real(wp)              vnorm
      real(wp)              vsep
      logical               failed
      logical               return

!
!     Local Variables.
!
      real(wp)              ang1
      real(wp)              ang2
      real(wp)              pv1    ( 3 )
      real(wp)              pv2    ( 3 )
      real(wp)              range1
      real(wp)              range2
      real(wp)              theta
      real(wp)              lt

!
!     ATOL is a tolerance value for computing arc sine.
!

      real(wp), parameter :: atol = 1.0e-12_wp

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     First check for bad inputs.
!
      if ( r1 < 0.0_wp .or. r2 < 0.0_wp ) then

         call setmsg  ('A negative radius for a body was ' &
         //            'encountered. The radius for body # was given ' &
         //            'as #, the radius of body # was given as #. ')

         call errint  ('#', targ1         )
         call errdp   ('#', r1            )
         call errint  ('#', targ2         )
         call errdp   ('#', r2            )
         call sigerr  ('SPICE(BADRADIUS)' )
         return
      end if

!
!     Get the state of the TARG1, TARG2 objects relative to OBS.
!
      call spkezp ( targ1, et, ref, abcorr,  obs, pv1, lt )

      if (  failed() ) return

       call spkezp ( targ2, et, ref, abcorr, obs, pv2, lt )

      if (  failed() ) return

!
!     Compute the range to the objects of interest.
!
      range1 = vnorm( pv1 )
      range2 = vnorm( pv2 )

!
!     Compute the apparent angular radii as seen from OBS.
!
      if ( range1 > r1 ) then

         ang1 = dasine( r1/range1, atol )

         if (  failed() ) return

      else

         ang1 = halfpi ()

      end if

      if ( range2 > r2 ) then

         ang2 = dasine ( r2/range2, atol )

         if (  failed() ) return

      else

         ang2 = halfpi()

      end if

!
!     Finally compute the apparent separation.
!
      theta = vsep ( pv1, pv2 )
      value = theta - ang1 - ang2

      return
      end
