!$Procedure ZZGFUDLT ( Private --- GF, scalar function < ref value )

      subroutine zzgfudlt ( udfunc, et, isless )
      use pepper_kinds, only: wp

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     This routine determines if the value of the scalar quantity
!     function is less than a previously defined reference value.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!
! $ Declarations

      implicit none

      include               'zzholdd.inc'

      external              udfunc

      real(wp)              et
      logical               isless

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ZZGET      P   ZZHOLDD retrieves a stored DP value.
!     GF_REF     P   ZZHOLDD acts on the reference value
!                    for a GF search.
!     UDFUNC     I   Name of the routine that computes the scalar value
!                    of interest at ET.
!     ET         I   Time in TDB seconds for which to evaluate UDFUNC.
!     ISLESS     O   Boolean indicating if the scalar value is less than
!                    reference value.
!
! $ Detailed_Input
!
!     UDFUNC     the routine that returns the value of the scalar
!                quantity of interest at time ET. The calling sequence
!                for UDFUNC is:
!
!                   CALL UDFUNC ( ET, VALUE )
!
!                where:
!
!                   ET      a double precision value representing
!                           ephemeris time, expressed as seconds past
!                           J2000 TDB, at which to determine the scalar
!                           value.
!
!                   VALUE   is the value of the geometric quantity
!                           at ET.
!
!     ET         a double precision value representing ephemeris time,
!                expressed as seconds past J2000 TDB at which to
!                determine the value of UDFUNC.
!
! $ Detailed_Output
!
!     ISLESS     a scalar boolean indicating if the value of UDFUNC at
!                ET is less than REFVAL (true) or not (false).
!
!                 Functionally:
!
!                   ISLESS = UDFUNC( ET )  <  REFVAL
!
! $ Parameters
!
!    None.
!
! $ Exceptions
!
!     1) ZZHOLDD will signal the error SPICE(ZZHOLDNOPUT) if this
!        routine is called prior to storing a reference value
!        using a ZZHOLDD "PUT" operation.
!
! $ Files
!
!    None.
!
! $ Particulars
!
!     A ZZHOLDD "PUT" stored the reference value used in the logical
!     operation. A ZZHOLDD "GET" retrieves the value.
!
! $ Examples
!
!    See GFUDS.
!
! $ Restrictions
!
!    None.
!
! $ Literature_References
!
!    None.
!
! $ Author_and_Institution
!
!    N.J. Bachman   (JPL)
!    E.D. Wright    (JPL)
!
! $ Version
!
!-   SPICELIB Version 1.1.0, 06-AUG-2010 (EDW)
!
!       Change in ZZHOLDD functionality required edit to ZZHOLDD
!       call and inclusion of "zzholdd.inc" parameters file.
!
!-   SPICELIB Version 1.0.0, 16-FEB-2010 (EDW)
!
!-&

! $ Index_Entries
!
!    function less than reference value
!
!-&

!
!     SPICELIB functions
!
      logical               return
      logical               failed

!
!     Local Variables
!
      real(wp)              udval
      real(wp)              refval

      logical               ok


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Default value of false for ISLESS.
!
      isless = .false.

!
!     Call the routine, return the scalar value corresponding to ET.
!
      call udfunc ( et, udval )

!
!     Check for an error, return if found.
!
      if ( failed() ) return

!
!     Retrieve the stored reference value. Signal an error if a
!     get call fails.
!
      call zzholdd ( zzget, gf_ref, ok, refval )

      if ( .not. ok ) then
         call setmsg ( 'ZZHOLDD GET failed. This indicates a logic ' &
                 //    'error in the GF code due either to a failure ' &
                 //    'to store the GF reference value or a post ' &
                 //    'store reset of ZZHOLDD.' )
         call sigerr ( 'SPICE(ZZHOLDDGETFAILED)' )
         return
      end if

      isless = udval < refval

      return

      end



