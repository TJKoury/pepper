!$Procedure UDDC ( Derivative of function less than zero, df(x)/dx < 0 )

      subroutine uddc ( udfunc, x, dx, isdecr )

! $ Abstract
!
!     Return .TRUE. if the derivative of the callback function UDFUNC
!     at a given abscissa value is negative.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DERIVATIVE
!     MATH
!
! $ Declarations

      implicit none

      external              udfunc

      double precision      x
      double precision      dx
      logical               isdecr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UDFUNC     I   The routine that computes the scalar value
!                    of interest.
!     X          I   Independent variable of UDFUNC.
!     DX         I   Interval from X for derivative calculation.
!     ISDECR     O   Boolean indicating if the derivative is negative.
!
! $ Detailed_Input
!
!     UDFUNC   is the routine that returns the value of the scalar
!              quantity function of interest at X. The calling
!              sequence for UDFUNC is:
!
!                 CALL UDFUNC ( X, VALUE )
!
!              where:
!
!                 X       the double precision value of the
!                         independent variable of the function
!                         at which to determine the scalar value.
!
!                 VALUE   the double precision value returned by
!                         UDFUNC at X.
!
!              Functionally:
!
!                 VALUE = UDFUNC ( X )
!
!     X        is a scalar double precision value at which to determine
!              the derivative of UDFUNC.
!
!              For many SPICE uses, X will represent ephemeris time,
!              expressed as seconds past J2000 TDB.
!
!     DX       is a scalar double precision value representing half the
!              interval in units of X separating the evaluation
!              values of UDFUNC; the evaluations occur at (X + DX)
!              and (X - DX).
!
!              DX may be negative but must be non-zero.
!
! $ Detailed_Output
!
!     ISDECR   is a scalar boolean indicating if the first derivative
!              of UDFUNC with respect to the independent variable
!              at X is less than zero.
!
!              Functionally:
!
!                            d UDFUNC(x) |
!                 ISDECR =   ----------- |  <  0
!                                 dx     |
!                                         X
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If DX has a value of zero, an error is signaled by a routine
!         in the call tree of this routine.
!
! $ Files
!
!     If the evaluation of UDFUNC requires SPICE kernel data, the
!     appropriate kernels must be loaded before calling this routine.
!
!     -  SPK data: the calling application must load ephemeris data
!        for the targets, observer, and any intermediate objects in
!        a chain connecting the targets and observer for the time
!        used in the evaluation. If aberration corrections are
!        used, the states of target and observer relative to the
!        solar system barycenter must be calculable from the
!        available ephemeris data.
!
!     -  If non-inertial reference frames are used, then PCK
!        files, frame kernels, C-kernels, and SCLK kernels may be
!        needed.
!
!     Such kernel data are normally loaded once per program run, NOT
!     every time this routine is called.
!
! $ Particulars
!
!     This routine only wraps a UDDF call, examining the sign of the
!     derivative value returned by UDDF. Please refer to this routine
!     for further details.
!
! $ Examples
!
!     See GFUDS.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 31-MAR-2010 (EDW) (NJB)
!
!-&


! $ Index_Entries
!
!     first derivative of scalar function less than zero
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
      double precision      deriv

      if ( return () ) then
         return
      end if

      call chkin ( 'UDDC' )

      isdecr = .false.

!
!     Numerically calculate the derivative of UDFUNC at X.
!
      call uddf ( udfunc, x, dx, deriv )

      if (  failed() ) then
         call chkout ( 'UDDC' )
         return
      end if

      isdecr = deriv .lt. 0.d0

      call chkout ( 'UDDC' )
      return

      end
