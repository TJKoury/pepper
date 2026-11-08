!$Procedure ZZDIV ( Safer division )

      DOUBLE PRECISION FUNCTION ZZDIV ( NUMR, DENOM )

! $ Abstract
!
!     Safely calculate the value NUMR/DENOM, avoiding the possibility
!     of floating point exceptions (FPE), due to numeric underflow,
!     numeric overflow, or divide-by-zero.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      DOUBLE PRECISION      NUMR
      DOUBLE PRECISION      DENOM

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

      LOGICAL               RETURN
      DOUBLE PRECISION      DPMAX

!
!     Local variables
!

      DOUBLE PRECISION      LOGNUM
      DOUBLE PRECISION      LOGDEN

!
!     The bounds on the potential result of the calculation.
!
      DOUBLE PRECISION      EXPNT

!
!     First entry flag.
!
      LOGICAL               FIRST
      SAVE

      DATA                  FIRST / .TRUE. /

!
!     Return on error.
!
      IF ( RETURN() ) THEN
         ZZDIV = 0.D0
         RETURN
      END IF


!
!     Participate in error tracing.
!
      CALL CHKIN ( 'ZZDIV' )


!
!     Calculate the bounds parameter on first entry.
!     The double precision maximum value has the form
!     "d*(10**EXPNT)." The value of interest is "EXPNT."
!
      IF (FIRST) THEN

         FIRST  = .FALSE.

!
!        A "floor" evaluation.
!
         EXPNT = DBLE( INT( LOG10( DPMAX() ) ) )

       END IF


!
!     If the denominator is zero, return zero and signal an error.
!     This is equivalent to a signaling NaN (not-a-number) for
!     the 0/0 case.
!
      IF ( DENOM .EQ. 0.D0 ) THEN

         ZZDIV = 0.D0
         CALL SETMSG ( 'Numerical divide by zero event. ' &
                    // 'Numerator value #1.'             )
         CALL ERRDP  ( '#1', NUMR                        )
         CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'             )
         CALL CHKOUT ( 'ZZDIV'                           )
         RETURN

      END IF


!
!     If the numerator is zero, the division is zero. DENOM
!     is known non-zero.
!
      IF ( NUMR .EQ. 0.D0 ) THEN

         ZZDIV = 0.D0
         CALL CHKOUT ( 'ZZDIV' )
         RETURN

      END IF


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
      LOGNUM = LOG10( DABS(NUMR)  )
      LOGDEN = LOG10( DABS(DENOM) )

!
!     Local possible overflow check.
!
      IF ( (LOGNUM - LOGDEN) .GT. EXPNT ) THEN

         ZZDIV = 0.D0
         CALL SETMSG ( 'Numerical overflow event. ' &
                    // 'Numerator value #1, denominator value #2.')
         CALL ERRDP  ( '#1', NUMR                                 )
         CALL ERRDP  ( '#2', DENOM                                )
         CALL SIGERR ( 'SPICE(NUMERICOVERFLOW)'                   )
         CALL CHKOUT ( 'ZZDIV'                                    )
         RETURN

      END IF

!
!     Local possible underflow check. Accept this may occur,
!     return a zero.
!
      IF ( (LOGNUM - LOGDEN) .LT. -(EXPNT-1.D0) ) THEN

         ZZDIV = 0.D0
         CALL CHKOUT ( 'ZZDIV' )
         RETURN

      END IF

!
!     This operation should be safe. Probably.
!
      ZZDIV = NUMR/DENOM

      CALL CHKOUT ( 'ZZDIV' )

      RETURN
      END
