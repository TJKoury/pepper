!$Procedure RMAINI ( Remainder --- integer )

      subroutine rmaini ( num, denom, q, rem )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Compute the integer quotient and non-negative remainder
!     of NUM and DENOM.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           num
      integer(ip)           denom
      integer(ip)           q
      integer(ip)           rem

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUM        I   Numerator used to compute quotient and remainder.
!     DENOM      I   Denominator used to compute quotient and remainder.
!     Q          O   Integer portion of the quotient NUM/DENOM.
!     REM        O   Remainder of the quotient NUM/DENOM.
!
! $ Detailed_Input
!
!     NUM      is the numerator of a quotient
!
!     DENOM    is the denominator of a quotient
!
! $ Detailed_Output
!
!     Q        is the largest integer less than or equal to the
!              quotient NUM/DENOM
!
!     REM      is the remainder of the integer division NUM/DENOM
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If DENOM is zero, the error SPICE(DIVIDEBYZERO) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Given the integer inputs NUM and DENOM, this routine
!     finds integers Q and REM that satisfy the following conditions:
!
!         1) NUM = DENOM * Q + REM
!
!         2) REM is a non negative integer less than the absolute
!            value of DENOM.
!
!     This routine serves as a macro. In this way the code to perform
!     this task can be written and maintained in a single location.
!
! $ Examples
!
!     One frequently needs to compute the  ``360 modulus'' of a
!     number. For positive numbers the FORTRAN intrinsic mod
!     function works well. However, for negative numbers the
!     intrinsic will return a negative modulus. This routine
!     can be used to compute the positive 360 pi modulus (MOD360) for
!     any integer I by the call:
!
!         CALL RMAINI ( I, 360, Q, MOD360 )
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Compute the remainder of an integer division
!
!-&


!
!     Take care of the zero-denominator case first
!
      if ( denom == 0.0_wp ) then
         call chkin  ( 'RMAINI'                                       )
         call setmsg ( 'Attempting to compute a quotient with a '     // &
                       'divide by zero.'                              )
         call sigerr ( 'SPICE(DIVIDEBYZERO)'                          )
         call chkout ( 'RMAINI'                                       )
         return
      end if

      q   = num / denom
      rem = num - denom*q

      if ( rem < 0 ) then
         q   = q   - 1
         rem = rem + denom
      end if

      return
      end
