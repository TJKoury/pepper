!$Procedure RMAIND ( Remainder --- double precision )

      subroutine rmaind ( num, denom, q, rem )
      use pepper_kinds, only: wp

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

      real(wp)              num
      real(wp)              denom
      real(wp)              q
      real(wp)              rem

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
!     Given the double precision inputs NUM and DENOM, this routine
!     finds double precision numbers Q and REM that satisfy the
!     following conditions:
!
!         1) NUM = DENOM * Q + REM
!
!         2) DENOM has integer value.
!
!         3) REM belongs to the half open interval [0, ABS(DENOM) )
!
!     This routine serves as a macro. In this way the code to perform
!     this task can be written and maintained in a single location.
!
! $ Examples
!
!     One frequently needs to compute the  ``Two pi modulus'' of a
!     number. For positive numbers the FORTRAN intrinsic mod
!     function works well. However, for negative numbers the
!     intrinsic will return a negative modulus. This routine
!     can be used to compute the positive two pi modulus (MOD2PI) for
!     any number X by the call:
!
!         CALL RMAIND ( X, TWOPI(), I, MOD2PI )
!
! $ Restrictions
!
!     1)  Arithmetic overflows are not trapped or detected by this
!         routine.
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
!     Compute the remainder of a double precision division
!
!-&


      real(wp)              mynum
      real(wp)              mydnom

!
!     Take care of the zero-denominator case first
!
      if ( denom == 0.0_wp ) then
         call setmsg ( 'Attempting to compute a quotient with a '     // &
                       'divide by zero.'                              )
         call sigerr ( 'SPICE(DIVIDEBYZERO)'                          )
         return
      end if

      mydnom = denom
      mynum  = num

      q   = dint ( mynum / mydnom )
      rem = mynum -    q * mydnom

      if ( rem < 0.0_wp ) then
         q   = q   - 1.0_wp
         rem = rem + mydnom
      end if

      return
      end
