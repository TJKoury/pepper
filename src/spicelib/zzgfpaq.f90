!$Procedure ZZGFPAQ ( Private --- GF, phase angle between bodies )

      subroutine zzgfpaq ( et, targ, illmn, obs, abcorr, value )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute the apparent phase angle for a target, observer,
!     illuminator set of ephemeris objects.
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

      implicit none

      double precision      et
      integer               targ
      integer               illmn
      integer               obs
      character*(*)         abcorr
      double precision      value

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     TARG       I   Target body ID.
!     ILLMN      I   Illuminating body ID.
!     OBS        I   Observer body ID.
!     ABCORR     I   Aberration correction flag.
!     VALUE      O   Value of phase angle.
!
! $ Detailed_Input
!
!     ET       the time in ephemeris seconds past J2000 TDB at which
!              to compute the phase angle.
!
!     TARG     the SPICE integer ID for the target body.
!
!     ILLMN    the SPICE integer ID for the illuminating body.
!
!     OBS      the SPICE integer ID for the observer.
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
!                 'CN+S'     "Reception" case:  converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
! $ Detailed_Output
!
!     VALUE      is the optionally light-time corrected phase angle
!                between TARG and ILLMN as observed from OBS.
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
!     This routine calculates the phase angle using the location of the
!     bodies (if point objects) or the center of the bodies (if finite
!     bodies).
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
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 23-JUN-2010 (EDW)
!
!-&

! $ Index_Entries
!
!   compute the phase of two objects wrt an illumination source
!
!-&

!
!     SPICELIB functions.
!

      logical               failed
      logical               return
      logical               eqstr

      double precision      vsep
      double precision      pi

!
!     Local Variables.
!
      double precision      lt
      double precision      sep
      double precision      pv1    ( 3 )
      double precision      pv2    ( 3 )

      character*(5)         ref

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFPAQ' )

!
!     This calculation is invariant with respect to reference frame.
!     Use J2000 for convenience.
!
      ref = 'J2000'

!
!     Get the position of the TARG object relative to OBS at ET.
!
      call spkezp ( targ, et, ref, abcorr,  obs, pv1, lt )

      if  (  failed() ) then
         call chkout ( 'ZZGFPAQ' )
         return
      end if


!
!     Get the state of the ILLMN object relative to TARG at ET
!     for no aberration correction, or ET - LT otherwise.
!
      if ( eqstr( abcorr, 'NONE' ) ) then

         call spkezp ( illmn, et,      ref, abcorr, targ, pv2, lt )

      else

         call spkezp ( illmn, et - lt, ref, abcorr, targ, pv2, lt )

      end if

      if (  failed() ) then
         call chkout ( 'ZZGFPAQ' )
         return
      end if

!
!                       ILLMN      OBS
!       ILLMN as seen      ^       /
!       from TARG at       |      /
!       ET - LT.           |     /
!                         >|..../< phase angle
!                          |   /
!                        . |  /
!                      .   | /
!                     .     v     TARG as seen from OBS
!               SEP   .   TARG    at ET
!                      .  /
!                        /
!                       v
!
!        PI = SEP + PHASE
!
!        so
!
!        PHASE = PI - SEP
!
!     Calculate the angle separating the vectors relative to TARG
!
      sep = vsep( pv1, pv2 )

!
!     The angle of interest is that between -PV1 and PV2 measured from
!     TARG. Subtract SEP from PI to calculate this angle.
!
      value = pi() - sep

!
!     All done.
!
      call chkout (  'ZZGFPAQ' )
      return

      end

