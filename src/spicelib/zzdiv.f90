!$Procedure ZZDIV ( Safer division )

      real(wp) function zzdiv ( numr, denom )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Safely calculate the value NUMR/DENOM, avoiding the possibility
!     of floating point exceptions (FPE), due to numeric underflow,
!     numeric overflow, or divide-by-zero.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!
! $ Declarations

      implicit none

      real(wp)              numr
      real(wp)              denom

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NUMR       I   Numerator of division.
!     DENOM      I   Denominator of division.
!
! $ Detailed_Input
!
!     NUMR       Numerator for the division operation.
!
!     DENOM      Denominator for the division operation.
!
! $ Detailed_Output
!
!     ZZDIV      The value NUMR/DENOM.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(DIVIDEBYZERO) signals if DENOM equals zero. This
!        signal occurs for NUMR/0 and 0/0 cases.
!
!     2) SPICE(NUMERICOVERFLOW) signals if the logarithm base 10
!        of the division is greater than EXPNT (defined
!        below).
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     We want to avoid a floating point exception signal from
!     the platform. This routine does not trap exceptions,
!     the intended purpose is to prevent exceptions.
!
!     Given, for the IEEE 754 double-precision binary
!     floating-point format, the order of magnitude of the minimum
!     normal positive double equals -307 and the order of magnitude of
!     the maximum double equals 308.
!
!       -307 <= LOG10(|NUMR|) - LOG10(|DENOM|) <= 308
!
!     or
!
!         -307                         308
!        10     <=  |NUMR|/|DENOM| <= 10
!
!     Satisfying this condition should guarantee no floating
!     point exceptions.
!
!     Underflow returns zero without an error signal as per SPICE
!     convention.
!
!     Important, this routine does not calculate or enforce a
!     precision on the division evaluation. A safe evaluation
!     may result in a result unusable due to precision loss.
!
!     The routine does not depend on platform-specific arithmetic
!     exception handling, even though the bound for the computed
!     ratio is platform-specific.
!
!     The range [-307,308] is valid for IEEE double precision.
!     It may occur this routine runs on a non compliant platform,
!     so calculate the range based on the DPMAX() value.
!
!     Assign a parameter EXPNT such that EXPNT equals the order of
!     DPMAX. The routine uses the range [-(EXPNT-1), EXPNT].
!
!     This routine checks the difference between the base 10 logarithms
!     of NUMR and DENOM to ensure the magnitude of NUMR/DENOM is
!     within the range [-(EXPNT-1), EXPNT].
!
! $ Examples
!
!     Demonstrate the use of ZZDIV with DPMAX and zero as the
!     numerator and denominator.
!
!           PROGRAM ZZDIV_T
!           IMPLICIT NONE
!
!           DOUBLE PRECISION      NUMR
!           DOUBLE PRECISION      DENOM
!           DOUBLE PRECISION      DIV
!
!     C
!     C     SPICE functions.
!     C
!           DOUBLE PRECISION      ZZDIV
!           DOUBLE PRECISION      DPMAX
!
!
!     C
!     C     Set error reporting to REPORT.
!     C
!           CALL ERRACT( 'SET', 'REPORT' )
!
!
!     C
!     C     Standard, safe evaluation.
!     C
!           NUMR  = 1.D0
!           DENOM = 10.D0
!
!           DIV = ZZDIV( NUMR, DENOM )
!           WRITE(*,*) 'DIV 1/10      = ', DIV
!
!
!
!     C
!     C     A numeric underflow event as defined in ZZDIV.
!     C
!           NUMR  = 1.D0
!           DENOM = DPMAX()
!
!           DIV = ZZDIV( NUMR, DENOM )
!           WRITE(*,*) 'DIV 1/DPMAX() = ', DIV
!
!
!
!     C
!     C     A numeric overflow event as defined in ZZDIV.
!     C
!           NUMR  = DPMAX()
!           DENOM = 1.D0
!
!           DIV = ZZDIV( NUMR, DENOM )
!           WRITE(*,*) 'DIV DPMAX()/1 = ', DIV
!
!
!     C
!     C     A divide by zero event.
!     C
!           NUMR  = 1.D0
!           DENOM = 0.D0
!
!           DIV = ZZDIV( NUMR, DENOM )
!           WRITE(*,*) 'DIV 1/0       = ', DIV
!
!
!     C
!     C     A 0/0 event. ZZDIV treats this as a divide by zero
!     C     event.
!     C
!           NUMR  = 0.D0
!           DENOM = 0.D0
!
!           DIV = ZZDIV( NUMR, DENOM )
!           WRITE(*,*) 'DIV 0/0       = ', DIV
!
!
!           END
!
!   The program outputs:
!
!   -The function returns the evaluation value. No error.
!
!      DIV 1/10      =   0.10000000000000001
!
!
!
!      -The function returns zero for an underflow state. No error.
!
!      DIV 1/DPMAX() =    0.0000000000000000
!
!
!
!   -The function signals a NUMERICOVERFLOW error, and sets the
!    return value to zero.
!
!     =================================================================
!
!     Toolkit version: N0064
!
!     SPICE(NUMERICOVERFLOW) --
!
!     Numerical overflow event. Numerator value 1.7976931348623E+308,
!     denominator value 1.0000000000000E+00.
!
!     A traceback follows.  The name of the highest level module is
!     first.
!     ZZDIV
!
!     =================================================================
!      DIV DPMAX()/1 =    0.0000000000000000
!
!
!
!   -The function signals a DIVIDEBYZERO error, and sets the
!    return value to zero.
!
!     =================================================================
!
!     Toolkit version: N0064
!
!     SPICE(DIVIDEBYZERO) --
!
!     Numerical divide by zero event. Numerator value
!     1.0000000000000E+00.
!
!     A traceback follows.  The name of the highest level module is
!     first.
!     ZZDIV
!
!     =================================================================
!      DIV 1/0       =    0.0000000000000000
!
!
!
!   -The function signals a DIVIDEBYZERO error, and sets the
!    return value to zero.
!
!     =================================================================
!
!     Toolkit version: N0064
!
!     SPICE(DIVIDEBYZERO) --
!
!     Numerical divide by zero event. Numerator value
!     0.0000000000000E+00.
!
!     A traceback follows.  The name of the highest level module is
!     first.
!     ZZDIV
!
!     =================================================================
!      DIV 0/0       =    0.0000000000000000
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
!-    SPICELIB Version 1.0.0, 31-JAN-2014 (EDW)
!
!-&

! $ Index_Entries
!
!     division, avoid floating point exception
!
!-&

!
!     SPICELIB functions
!

      logical               return
      real(wp)              dpmax

!
!     Local variables
!

      real(wp)              lognum
      real(wp)              logden

!
!     The bounds on the potential result of the calculation.
!
      real(wp)              expnt

!
!     First entry flag.
!
      logical               first
      save

      data                  first / .true. /

!
!     Return on error.
!
      if ( return() ) then
         zzdiv = 0.0_wp
         return
      end if


!
!     Participate in error tracing.
!
      call chkin ( 'ZZDIV' )


!
!     Calculate the bounds parameter on first entry.
!     The double precision maximum value has the form
!     "d*(10**EXPNT)." The value of interest is "EXPNT."
!
      if (first) then

         first  = .false.

!
!        A "floor" evaluation.
!
         expnt = real( int( log10( dpmax() ) ), wp )

       end if


!
!     If the denominator is zero, return zero and signal an error.
!     This is equivalent to a signaling NaN (not-a-number) for
!     the 0/0 case.
!
      if ( denom == 0.0_wp ) then

         zzdiv = 0.0_wp
         call setmsg ( 'Numerical divide by zero event. ' &
                    // 'Numerator value #1.'             )
         call errdp  ( '#1', numr                        )
         call sigerr ( 'SPICE(DIVIDEBYZERO)'             )
         call chkout ( 'ZZDIV'                           )
         return

      end if


!
!     If the numerator is zero, the division is zero. DENOM
!     is known non-zero.
!
      if ( numr == 0.0_wp ) then

         zzdiv = 0.0_wp
         call chkout ( 'ZZDIV' )
         return

      end if


!
!     Calculate base 10 logarithms of the absolute value of the
!     numerator and denominator. Recall the base 10 log of a negative
!     real is a complex number (an irritating reality). Our interest
!     is the magnitude of the result, not the sign.
!
!     An earlier check returned if NUMR or DENOM equals zero, so the
!     LOG10 call is safe from an infinite return value. An infinite
!     return value defeats the purpose of this routine.
!
      lognum = log10( dabs(numr)  )
      logden = log10( dabs(denom) )

!
!     Local possible overflow check.
!
      if ( (lognum - logden) > expnt ) then

         zzdiv = 0.0_wp
         call setmsg ( 'Numerical overflow event. ' &
                    // 'Numerator value #1, denominator value #2.')
         call errdp  ( '#1', numr                                 )
         call errdp  ( '#2', denom                                )
         call sigerr ( 'SPICE(NUMERICOVERFLOW)'                   )
         call chkout ( 'ZZDIV'                                    )
         return

      end if

!
!     Local possible underflow check. Accept this may occur,
!     return a zero.
!
      if ( (lognum - logden) < -(expnt-1.0_wp) ) then

         zzdiv = 0.0_wp
         call chkout ( 'ZZDIV' )
         return

      end if

!
!     This operation should be safe. Probably.
!
      zzdiv = numr/denom

      call chkout ( 'ZZDIV' )

      return
      end
