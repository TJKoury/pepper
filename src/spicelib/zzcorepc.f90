!$Procedure ZZCOREPC ( Correct epoch for aberration )

      subroutine zzcorepc ( abcorr, et, lt, etcorr )
      use pepper_kinds, only: wp
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute an aberration corrected epoch, given an aberration
!     correction specification, an epoch, and a light time.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ABERRATION
!     PARSING
!     PRIVATE
!     UTILITY
!
! $ Declarations

      character(*)          abcorr
      real(wp)              et
      real(wp)              lt
      real(wp)              etcorr

      include 'zzabcorr.inc'

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     ABCORR     I   Aberration correction string.
!     ET         I   Ephemeris time, seconds past J2000.
!     LT         I   Light time.
!     ETCORR     O   Light time-corrected epoch.
!
! $ Detailed_Input
!
!     ABCORR         is a string representing a aberration
!                    correction.  The supported values are:
!
!                       'CN'
!                       'CN+S'
!                       'LT'
!                       'LT+S'
!                       'NONE'
!                       'RL'
!                       'RL+S'
!                       'S'
!                       'XCN'
!                       'XCN+S'
!                       'XLT'
!                       'XLT+S'
!                       'XRL'
!                       'XRL+S'
!                       'XS'
!
!                    Note that some values not supported by the
!                    SPICELIB SPK subsystem are supported by
!                    this routine:
!
!                       - The letter 'R' indicates relativistic
!                         corrections.
!
!                       - Stellar aberration-only corrections are
!                         indicated by the strings
!
!                            'S'
!                            'XS'
!
!                    Case and leading and trailing blanks are not
!                    significant in ABCORR.
!
!     ET             is an epoch, expressed as seconds past J2000 TDB.
!
!     LT             is a light time value, expressed as TDB seconds.
!
! $ Detailed_Output
!
!     ETCORR         is the input epoch ET, corrected for light time:
!
!                       If the specified aberration correction calls
!                       for some type of light time correction (normal,
!                       converged Newtonian, relativistic), LT will be
!                       added to or subtracted from ET.  If the
!                       correction is of the transmission type, then
!
!                          ETCORR = ET + LT
!
!                       If the correction is of the reception type,
!                       then
!
!                          ETCORR = ET - LT
!
!                       If no light time correction is specified, then
!
!                          ETCORR = ET
!
! $ Parameters
!
!     See INCLUDE file zzabcorr.inc.
!
! $ Exceptions
!
!     1) If the input aberration correction choice is not recognized,
!        the error SPICE(INVALIDOPTION) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Many SPICELIB routines have logic branches based on the
!     attributes of aberration corrections.  Much duplicated
!     parsing code can be avoided by using this routine.
!
! $ Examples
!
!     See ZZDYNFRM.
!
! $ Restrictions
!
!     1) This is a SPICE private routine; the routine is subject
!        to change without notice.  User applications should not
!        call this routine.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 24-NOV-2004 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!

!
!     Local variables
!
      logical               corblk ( abatsz )

      if ( return() ) return

!
!     Parse the aberration correction string.  Obtain a correction
!     attribute block.
!
      call zzprscor ( abcorr, corblk )

      if ( corblk(ltidx) ) then
!
!        Light time corrections are used.  The output epoch
!        must be adjusted according to whether the correction
!        is for received or transmitted radiation.
!
         if ( corblk(xmtidx) ) then
!
!           This is the transmission case.
!
            etcorr = et + lt

         else
!
!           This is the reception case.
!
            etcorr = et - lt

         end if

      else
!
!        Light time corrections are not used.
!
         etcorr = et

      end if

      return
      end
