!$Procedure ZZGFPAU ( Private --- GF, phase angle utility routine )

      subroutine zzgfpau ( target, illmn,  abcorr, obsrvr, &
                           udfunc, et,     decres, rvl, &
                           xtarg,  xillmn, xabcor, xobs, xablk )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the three entry points needed by
!     GFEVNT in order to find body centered phase angle events.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      implicit none

      include 'zzabcorr.inc'

      character(*)          target
      character(*)          illmn
      character(*)          abcorr
      character(*)          obsrvr
      real(wp)              et
      logical               decres
      real(wp)              rvl
      integer(ip)           xtarg
      integer(ip)           xillmn
      character(*)          xabcor
      integer(ip)           xobs
      logical               xablk  ( nabcor )


! $ Brief_I/O
!
!     VARIABLE  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     TARGET    I-O  ZZGFPAIN, ZZGFPAX
!     ILLMN     I-O  ZZGFPAIN, ZZGFPAX
!     ABCORR    I-O  ZZGFPAIN, ZZGFPAX
!     OBSRVR    I-O  ZZGFPAIN, ZZGFPAX
!     ET         I   ZZGFPADC, ZZGFPAGQ
!     UDFUNC     I   ZZGFPADC
!     DECRES     O   ZZGFPADC
!     RVL        O   ZZGFPAGQ
!     XTARG      O   ZZGFPAX
!     XILLMN     O   ZZGFPAX
!     XABCOR     O   ZZGFPAX
!     XOBS       O   ZZGFPAX
!     XABLK      O   ZZGFPAX
!
! $ Detailed_Input
!
!     TARGET   the string name of a target body.  Optionally, you may
!              supply the integer ID code for the object as an
!              integer string.  For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!              The target and observer define a position vector
!              that points from the observer to the target.
!
!     ILLMN    the string name of the illuminating body. This will
!              normally be 'SUN' but the algorithm can use any
!              ephemeris object
!
!     ABCORR   the string description of the aberration corrections to
!              apply to the state evaluations to account for one-way
!              light time and stellar aberration.
!
!              Any signal receive aberration correction accepted by
!              the SPICE routine SPKEZR is accepted here. See the header
!              of SPKEZR for a detailed description of the
!              aberration correction options. For convenience,
!              the options are listed below:
!
!                 'NONE'     Apply no correction. Returns the "true"
!                            geometric state.
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
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     OBSRVR   the string name of an observing body.  Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to indicate the observer as Earth.
!
!     ET       the time in TDB seconds past J2000 at which to calculate
!              the value of or characteristic of the phase angle
!              between SVTARG and SVILLMN as seen from SVOBS.
!
!     For more information, see individual entry points.
!
! $ Detailed_Output
!
!     DECRES   is .TRUE. if the phase angle is decreasing at ET.
!              Otherwise it is .FALSE..
!
!     RVL      the phase angle between SVTARG and SVILLMN as seen
!              from SVOBS at time ET.
!
!     XTARG    SPICE ID value for the target body initialized
!              via ZZGFPAIN.
!
!     XILLMN   SPICE ID value for the illuminator body initialized
!              via ZZGFPAIN.
!
!     XABCOR   String value for the aberration correction initialized
!              via ZZGFPAIN.
!
!     XOBS     SPICE ID value for the observing body initialized
!              via ZZGFPAIN.
!
!     XABLK    Aberration correction attribute block as initialized
!              by ABCORR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(BOGUSENTRY) signals if a direct call to ZZGFPAU occurs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as the umbrella routine for three entry points
!     needed by GFEVNT in solving for phase angle conditions.
!
!     The three entry points are
!
!        ZZGFPAIN --- an initialization routine that must be called
!                     prior to attempting to solve for any range
!                     rate event.
!
!        ZZGFPADC --- determines whether or not phase angle is
!                     decreasing at some time.
!
!        ZZGFPAGQ --- returns the phase angle of the two objects
!                     of concern as a function of ET.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     ZZGFPAIN must be called prior to use of any of the other
!     entry points (think constructor).
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     E.D. Wright    (JPL)
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 16-SEP-2012 (EDW) (NJB)
!
!-&

! $ Index_Entries
!
!     find phase angle events
!
!-&

      external              udfunc

!
!     SPICELIB functions
!
      logical               failed
      logical               return

      real(wp)              clight
      real(wp)              dvsep
      real(wp)              vdot


!
!     Local parameters
!

!
!     The phase angle calculation is invariant with respect to
!     reference frame. Use J2000 for convenience.
!
      character(*)          ref
      parameter           ( ref    = 'J2000' )


!
!     Local Variables
!
      character(corlen)     svabco

      real(wp)              dsep
      real(wp)              lt
      real(wp)              dlt
      real(wp)              s1   (6)
      real(wp)              s2   (6)
      real(wp)              uvec (3)

      integer(ip)           svillm
      integer(ip)           svobs
      integer(ip)           svtarg
      integer(ip)           i

      logical               svablk ( nabcor )
      logical               found


!
!     Saved Variables
!
      save                  svablk
      save                  svabco
      save                  svtarg
      save                  svobs
      save                  svillm

      call chkin  ( 'ZZGFPAU'           )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'ZZGFPAU'           )

      return



!$Procedure  ZZGFPAIN ( Private --- GF, phase angle initialization )

      entry zzgfpain ( target, illmn, abcorr, obsrvr )

! $ Abstract
!
!     This is the initialization entry point used for describing
!     the event that is to be solved for by ZZGFSOLV.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PHASE ANGLE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!      CHARACTER*(*)         TARGET
!      CHARACTER*(*)         ABCORR
!      CHARACTER*(*)         OBSRVR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARGET     I   Name of the target body.
!     ILLMN      I   Name of the illuminating body.
!     ABCORR     I   Aberration correction flag.
!     OBSRVR     I   Name of the observing body.
!
! $ Detailed_Input
!
!     TARGET   the string name of a target body.  Optionally, you may
!              supply the integer ID code for the object as an
!              integer string.  For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!              Case and leading or trailing blanks are not significant
!              in the string TARGET.
!
!     ILLMN    the string name of the illuminating body. This will
!              normally be 'SUN' but the algorithm can use any
!              ephemeris object
!
!              Case and leading or trailing blanks are not significant
!              in the string ILLMN.
!
!     ABCORR   the string description of the aberration corrections to
!              apply to the state evaluations to account for one-way
!              light time and stellar aberration.
!
!              Any aberration correction accepted by the SPICE
!              routine SPKEZR is accepted here. See the header
!              of SPKEZR for a detailed description of the
!              aberration correction options. For convenience,
!              the options are listed below:
!
!                 'NONE'     Apply no correction. Returns the "true"
!                            geometric state.
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
!                'CN+S'     "Reception" case:  converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!              Note that this routine accepts only reception mode
!              aberration corrections.
!
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     OBSRVR   the string name of an observing body.  Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to indicate the observer as Earth.
!
!              Case and leading or trailing blanks are not significant
!              in the string OBSRVR.
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
!     1) SPICE(IDCODENOTFOUND) signals if the object name for the
!        target, TARGET, is not a recognized name.
!
!     2) SPICE(IDCODENOTFOUND) signals if the object name for the
!        illuminator, ILLMN, is not a recognized name.
!
!     3) SPICE(IDCODENOTFOUND) signals if the object name for the
!        observer, OBSRVR, is not a recognized name.
!
!     4) SPICE(BODIESNOTDISTINCT) signals if the three objects
!        associated with a PHASE ANGLE search are not distinct.
!
!     5) SPICE(INVALIDOPTION) signals for any transmit mode aberration
!        correction.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
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
!     N.J. Bachman   (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 19-OCT-2010 (EDW)(NJB)
!
!-&

! $ Index_Entries
!
!     phase angle initialization routine
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZGFPAIN' )

!
!     Find NAIF IDs for TARGET, ILLMN, and OBSRVR.
!
      call bods2c ( target, svtarg, found )

      if ( .not. found ) then

         call setmsg ( 'The target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         call errch  ( '#', target                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFPAIN'                                 )
         return

      end if


      call bods2c ( illmn, svillm, found )

      if ( .not. found ) then

         call setmsg ( 'The illuminator object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         call errch  ( '#', obsrvr                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFPAIN'                                 )
         return

      end if


      call bods2c ( obsrvr, svobs, found )

      if ( .not. found ) then

         call setmsg ( 'The observer object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         call errch  ( '#', obsrvr                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFPAIN'                                 )
         return

      end if


!
!     Make sure the observer, illuminator, and target are distinct.
!
      if ( (svtarg == svobs)   .or. &
           (svtarg == svillm) .or. &
           (svobs  == svillm)      ) then

         call setmsg ( 'The observer, illuminator, and ' &
         //            'target must be distinct objects, but ' &
         //            'are not: OBSRVR = #, TARGET = #, ' &
         //            'are not: ILLMN= #.' )
         call errch  ( '#', obsrvr                         )
         call errch  ( '#', target                         )
         call errch  ( '#', illmn                          )
         call sigerr ( 'SPICE(BODIESNOTDISTINCT)'          )
         call chkout ( 'ZZGFPAIN'                          )
         return

      end if

!
!     Squeeze all blanks out of the aberration correction
!     string; ensure the string is in upper case.
!
      call cmprss ( ' ', 0, abcorr, svabco )
      call ucase  ( svabco,         svabco )

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      call zzvalcor ( svabco, svablk )

      if ( failed() ) then
         call chkout ( 'ZZGFPAIN' )
         return
      end if

!
!     Restrict correction to reception cases.
!

      if( svablk(xmtidx) ) then

         call setmsg ( 'Invalid aberration correction ''#''. ' &
         //            'Phase angle geometry calculations currently ' &
         //            'restricted to reception cases.')
         call errch  ( '#', abcorr )
         call sigerr ( 'SPICE(INVALIDOPTION)' )
         call chkout ( 'ZZGFPAIN' )
         return

      end if


      call chkout ( 'ZZGFPAIN' )
      return




!$Procedure ZZGFPADC ( Private --- GF, when phase angle is decreasing )

      entry zzgfpadc ( udfunc, et, decres )

! $ Abstract
!
!     Computes whether or not the phase angle between the observer
!     and the target is decreasing at time ET.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     LOGICAL               DECRES
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     DECRES     O   .TRUE. if phase angle is decreasing, .FALSE.
!                    otherwise.
!
! $ Detailed_Input
!
!     ET         the time in ephemeris seconds past J2000 TDB at which
!                to determine whether the phase angle between SVTARG
!                and SVILLMN as seen from SVOBS is decreasing.
!
! $ Detailed_Output
!
!     DECRES     is .TRUE. if the phase angle is decreasing at ET.
!                Otherwise it is .FALSE..
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
!     None.
!
! $ Particulars
!
!     A function f(x) is strictly decreasing at x0 if and only if there
!     exists some delta > 0 such that for all dx satisfying
!
!        0  <  dx  < delta
!
!     we have
!
!        f(x0)       <  f(x0 + dx)
!
!     and
!
!        f(x0 - dx)  <  f(x)
!
!     Note that a strictly decreasing function need not be
!     differentiable in a neighborhood of x0; it can have jump
!     discontinuities in any neighborhood of x0 and even at x0.
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
!     N.J. Bachman   (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 10-JAN-2012 (EDW)
!
!-&

! $ Index_Entries
!
!     when phase angle is decreasing
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFPADC' )


!
!     Get the state of the TARG object relative to OBS at ET.
!
      call spkez ( svtarg, et, ref, svabco, svobs, s1, lt )

      if  (  failed() ) then
         call chkout ( 'ZZGFPADC' )
         return
      end if


!
!     Get the state of the ILLMN object relative to TARG at ET
!     for no aberration correction, or ET - LT otherwise.
!
      if ( svablk(geoidx) ) then

!
!        No correction, geometric.
!
         call spkez ( svillm, et,      ref, svabco, svtarg, s2, lt )

      else

         call spkez ( svillm, et - lt, ref, svabco, svtarg, s2, lt )

         if (  failed() ) then
            call chkout ( 'ZZGFPADC' )
            return
         end if

!
!        Correct velocity for time derivative of the observer target
!        light-time. We need to do this since the SPK evaluation occurs
!        at ET - LT.
!
!        d( ET - LT ) = (1 - d LT )
!        ------------        ----
!        dt                  dt
!
!        LT = ||R||
!             -----
!               C
!                                     ^
!        d LT    = 1   d ||R||  = 1 < R, V >
!        ----      -   -------    -
!        dt        C   dt         C
!
!
         call vhat ( s1, uvec )

         dlt = vdot ( uvec, s1(4) ) / clight()

!
!        Apply the correction to the velocity vector components.
!
         call vsclip ( 1.0_wp - dlt,  s2(4) )

      end if

      if (  failed() ) then
         call chkout ( 'ZZGFPADC' )
         return
      end if

!
!                       ILLMN      OBS
!       ILLMN as seen      *       /
!       from TARG at       |      /
!       ET - LT.           |     /
!                         >|..../< phase angle
!                          |   /
!                        . |  /
!                      .   | /
!                     .     *     TARG as seen from OBS
!               SEP   .   TARG    at ET
!                      .  /
!                        /
!                       *
!
!     Calculate the derivative of the angle separating the vectors
!     relative to TARG.
!
!        PI = SEP + PHASE
!
!        dPHASE     dSEP
!        ------ = - ----
!         dt         dt
!
      dsep   = dvsep( s1, s2 )

      decres = -dsep < 0.0_wp

      call chkout ( 'ZZGFPADC' )
      return




!$Procedure ZZGFPAGQ ( Private --- GF, phase angle between two bodies )

      entry zzgfpagq ( et, rvl )

! $ Abstract
!
!     Determine the phase angle between the centers of the two
!     bodies.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PHASE ANGLE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!      DOUBLE PRECISION      ET
!      DOUBLE PRECISION      RVL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     RVL        O   Phase angle at time ET.
!
! $ Detailed_Input
!
!     ET         the time in ephemeris seconds past J2000 TDB at which
!                to compute the phase angle.
!
! $ Detailed_Output
!
!     RVL        the phase angle between SVTARG and SVILLMN as seen
!                from SVOBS at time ET.
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
!     None.
!
! $ Particulars
!
!     None.
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
!     N.J. Bachman   (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 19-OCT-2010 (EDW)
!
!-&

! $ Index_Entries
!
!     get phase angle between two bodies
!
!-&

      call zzgfpaq ( et, svtarg, svillm, svobs, svabco, rvl )

      return




!$Procedure ZZGFPAX ( Private -- GF, retrieve ZZGFPAIN values )

      entry zzgfpax ( xtarg, xillmn, xabcor, xobs, xablk )

! $ Abstract
!
!     Retrieve values set in ZZGFPAIN.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XTARG      O   Saved value for the target body.
!     XILLMN     O   Saved value for the illuminator body.
!     XABCOR     O   Saved value for the aberration correction.
!     XOBS       O   Saved value for the observing body.
!     XABLK      O   Saved value for the aberration correction
!                    attribute block.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     XTARG    SPICE ID value for the target body initialized
!              via ZZGFPAIN.
!
!     XILLMN   SPICE ID value for the illuminator body initialized
!              via ZZGFPAIN.
!
!     XABCOR   String value for the aberration correction initialized
!              via ZZGFPAIN.
!
!     XOBS     SPICE ID value for the observing body initialized
!              via ZZGFPAIN.
!
!     XABLK    Aberration correction attribute block as initialized
!              by ABCORR.
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
!     None.
!
! $ Particulars
!
!     None.
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
!     N.J. Bachman   (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 16-SEP-2012 (EDW)
!
!-&

! $ Index_Entries
!
!     get saved phase angle parameters
!
!-&

         xtarg  = svtarg
         xillmn = svillm
         xabcor = svabco
         xobs   = svobs

         do i=1, abatsz
            xablk(i) = svablk(i)
         end do

      return
      end


