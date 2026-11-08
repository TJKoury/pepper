!$Procedure ZZSEPQ ( Separation quantity from observer )

      real(wp) function zzsepq ( et, &
                                         bod1, bod2, &
                                         r1,   r2, &
                                         obs,  abcorr, ref )
      use pepper_kinds, only: ip, wp

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

      implicit none

      real(wp)              et
      integer(ip)           bod1
      integer(ip)           bod2
      real(wp)              r1
      real(wp)              r2
      integer(ip)           obs
      character*(*)         abcorr
      character*(*)         ref

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
      real(wp)              dasine
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

      real(wp)              atol
      parameter           ( atol  = 1.0e-12_wp )

!
!     Set an initial value to return in case of error.
!
      zzsepq = 0.0_wp

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin( 'ZZSEPQ' )

!
!     First check for bad inputs.
!
      if ( (r1 .lt. 0.0_wp) .or. (r2 .lt. 0.0_wp) ) then

         call setmsg  ('A negative radius for a body was ' &
         //            'encountered. The radius for body #1 was given ' &
         //            'as #2, the radius of body #3 was given as #4.')

         call errint  ('#1', bod1         )
         call errdp   ('#2', r1            )
         call errint  ('#3', bod2         )
         call errdp   ('#4', r2            )
         call sigerr  ('SPICE(BADRADIUS)' )
         call chkout  ('ZZSEPQ'          )
         return
      end if


!
!     Get the state of the BOD1, BOD2 objects relative to OBS.
!
      call spkezp ( bod1, et, ref, abcorr,  obs, pv1, lt )

      if  (  failed() ) then
         call chkout ( 'ZZSEPQ' )
         return
      end if


       call spkezp ( bod2, et, ref, abcorr, obs, pv2, lt )

      if (  failed() ) then
         call chkout ( 'ZZSEPQ' )
         return
      end if

!
!     Compute the range to the objects of interest.
!
      range1 = vnorm( pv1 )
      range2 = vnorm( pv2 )

!
!     Compute the apparent angular radii as seen from OBS.
!
      if ( range1 .gt. r1 ) then

         ang1 = dasine( r1/range1, atol )

         if (  failed() ) then
            call chkout ( 'ZZSEPQ' )
            return
         end if

      else

         call setmsg  ('Observer object #1 located within surface ' &
                     //'of target 1 object #2. Range to target 1 #3, ' &
                     //'radius of target 1 #4')

         call errint  ('#1', obs           )
         call errint  ('#2', bod1         )
         call errdp   ('#3', range1        )
         call errdp   ('#4', r1             )
         call sigerr  ('SPICE(INSIDEBODY)' )
         call chkout  ('ZZSEPQ'             )
         return

      end if

      if ( range2 .gt. r2 ) then

         ang2 = dasine ( r2/range2, atol )

         if (  failed() ) then
            call chkout ('ZZSEPQ')
            return
         end if

      else

         call setmsg  ('Observer object #1 located within surface ' &
                     //'of target 2 object #2. Range to target 2 #3, ' &
                     //'radius of target 2 #4')

         call errint  ('#1', obs           )
         call errint  ('#2', bod2         )
         call errdp   ('#3', range2        )
         call errdp   ('#4', r2             )
         call sigerr  ('SPICE(INSIDEBODY)' )
         call chkout  ('ZZSEPQ'             )
         return

      end if

!
!     Finally compute the apparent separation.
!
      theta = vsep( pv1, pv2 )
      zzsepq = theta - ang1 - ang2

      call chkout ('ZZSEPQ')
      return
      end
