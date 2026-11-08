!$Procedure ZZGFRRQ ( Private - GF, range rate between objects )

      subroutine zzgfrrq ( et, targ, obs, abcorr, value )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute the apparent range rate between two ephemeris objects.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     RANGE RATE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      implicit none

      real(wp)              et
      integer(ip)           targ
      integer(ip)           obs
      character*(*)         abcorr
      real(wp)              value

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB
!     TARG       I   Target body ID
!     OBS        I   Observer body ID
!     ABCORR     I   Aberration correction flag
!     REF        I   Reference frame of the range rate
!     VALUE      O   Value of range rate between objects
!
! $ Detailed_Input
!
!     ET       is the time in ephemeris seconds past J2000 TDB at
!              which the range rate is to be measured.
!
!     TARG     the SPICE integer ID for the target body.
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
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
! $ Detailed_Output
!
!     VALUE    is the optionally light-time corrected range
!              rate of TARG observed from OBS.
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
!     This routine determines the apparent range rate of a target,
!     TARG, as seen from an observer, OBS, at epoch ET.
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
!-    SPICELIB version 1.0.1 08-JUL-2010 (EDW)
!
!        Minor typo correction to comments.
!
!-    SPICELIB version 1.0.0 09-JUN-2009 (NJB)(EDW)
!
!-&

! $ Index_Entries
!
!   compute the range rate between two objects.
!
!-&

!
!     SPICELIB functions.
!
      real(wp)              dvnorm

      logical               failed
      logical               return

!
!     Local Variables.
!
      real(wp)              lt
      real(wp)              state    ( 6 )

      character*(5)         ref

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin  ( 'ZZGFRRQ' )
      end if

!
!     We just want the range rate of TARG relative to OBS.
!     This calculation is invariant with respect to reference
!     frame; we use 'J2000'.
!
      ref = 'J2000'
      call spkez ( targ, et, ref, abcorr, obs, state, lt )

      if (  failed() ) then
          call chkout (  'ZZGFRRQ' )
          return
      end if

!
!     Calculate the derivative from the STATE vector.
!
      value = dvnorm( state )

!
!     All done.
!
      call chkout (  'ZZGFRRQ' )
      return

      end

