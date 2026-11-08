!$Procedure ZZMULT ( Safer multiplication )

      DOUBLE PRECISION FUNCTION ZZMULT ( A, B )

! $ Abstract
!
!     Safely calculate the value A*B, avoiding the possibility
!     of floating point exceptions (FPE) due to numeric underflow
!     or numeric overflow.
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

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     A          I   Multiplier.
!     B          I   Multiplicand.
!
! $ Detailed_Input
!
!     A          Multiplier for the multiplication operation.
!
!     B          Multiplicand for the multiplication operation.
!
! $ Detailed_Output
!
!     ZZMULT      The value A*B.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(NUMERICOVERFLOW) signals if the logarithm base 10
!        of the multiplication is greater than EXPNT (defined
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
!       -307 <= LOG10(|A|) + LOG10(|B|) <= 308
!
!     or
!
!         -307                   308
!        10     <=  |A|*|B| <= 10
!
!     Satisfying this condition should guarantee no floating
!     point exceptions.
!
!     Underflow returns zero without an error signal as per SPICE
!     convention.
!
!     Important, this routine does not calculate or enforce a
!     precision on the multiplication evaluation. A safe evaluation
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
!     This routine checks the sum of the base 10 logarithms
!     of A and B to ensure the magnitude of A*B is
!     within the range [-(EXPNT-1), EXPNT].
!
! $ Examples
!
!     Demonstrate the use of ZZMULT with DPMAX and zero as the
!     multiplier and multiplicand.
!
!           PROGRAM ZZMULT_T
!           IMPLICIT NONE
!
!           DOUBLE PRECISION      A
!           DOUBLE PRECISION      B
!           DOUBLE PRECISION      MULT
!
!     C
!     C     SPICE functions.
!     C
!           DOUBLE PRECISION      ZZMULT
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
!           A = 1.D0
!           B = 10.D0
!
!           MULT = ZZMULT( A, B )
!           WRITE(*,*) 'MULT 1*10         = ', MULT
!
!
!
!     C
!     C     A numeric overflow event as defined in ZZMULT.
!     C
!           A = 1.D0
!           B = DPMAX()
!
!           MULT = ZZMULT( A, B )
!           WRITE(*,*) 'MULT 1*DPMAX()    = ', MULT
!
!
!
!     C
!     C     A numeric overflow event as defined in ZZMULT.
!     C
!           A = 1.D208
!           B = 1.D307
!
!           MULT = ZZMULT( A, B )
!           WRITE(*,*) 'MULT 1.D515       = ', MULT
!
!
!
!     C
!     C     A multiply by zero event.
!     C
!           A = 1.D0
!           B = 0.D0
!
!           MULT = ZZMULT( A, B )
!           WRITE(*,*) 'MULT 1*0          = ', MULT
!
!
!
!     C
!     C     A multiply by zero event.
!     C
!           A = 0.D0
!           B = 0.D0
!
!           MULT = ZZMULT( A, B )
!           WRITE(*,*) 'MULT 0*0          = ', MULT
!
!
!           END
!
!   The program outputs:
!
!   -The function returns the evaluation value. No error.
!
!      MULT 1*10         =    10.000000000000000
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
!     Numerical overflow event. Multiplier value, 1.0000000000000E+00,
!     multiplicand value, 1.7976931348623E+308.
!
!     A traceback follows.  The name of the highest level module is
!     first.
!     ZZMULT
!
!     =================================================================
!      MULT 1*DPMAX()   =  0.0000000000000000
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
!     Numerical overflow event. Multiplier value, 1.0000000000000E+208,
!     multiplicand value, 1.0000000000000E+307.
!
!     A traceback follows.  The name of the highest level module is
!     first.
!     ZZMULT
!
!     =================================================================
!      MULT 1.D515       =    0.0000000000000000
!
!
!   -The function returns the evaluation value. No error.
!
!      MULT 1*0          =    0.0000000000000000
!
!
!   -The function returns the evaluation value. No error.
!
!      MULT 0*0          =    0.0000000000000000
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
!     multiplication, avoid floating point exception
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

      DOUBLE PRECISION      LOGA
      DOUBLE PRECISION      LOGB

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
         ZZMULT = 0.D0
         RETURN
      END IF


!
!     Participate in error tracing.
!
      CALL CHKIN ( 'ZZMULT' )


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
!     If either A or B equals zero the multiplication is zero.
!
      IF ( (A .EQ. 0.D0) .OR. (B .EQ. 0.D0) ) THEN

         ZZMULT = 0.D0
         CALL CHKOUT ( 'ZZMULT' )
         RETURN

      END IF


!
!     Calculate base 10 logarithms of the absolute value of the
!     numerator and denominator. Recall the base 10 log of a negative
!     real is a complex number (an irritating reality). Our interest
!     is the magnitude of the result, not the sign.
!
!     An earlier check returned if A or B equal zero.
!
      LOGA = LOG10( DABS(A) )
      LOGB = LOG10( DABS(B) )

!
!     Local possible overflow check.
!
      IF ( (LOGA + LOGB) .GT. EXPNT ) THEN

         ZZMULT = 0.D0
         CALL SETMSG ( 'Numerical overflow event. ' &
                    // 'Multiplier value, #1, ' &
                    // 'multiplicand value, #2.'  )
         CALL ERRDP  ( '#1', A                    )
         CALL ERRDP  ( '#2', B                    )
         CALL SIGERR ( 'SPICE(NUMERICOVERFLOW)'   )
         CALL CHKOUT ( 'ZZMULT'                   )
         RETURN

      END IF

!
!     Local possible underflow check. Accept this may occur,
!     return a zero.
!
      IF ( (LOGA + LOGB) .LT. -(EXPNT-1.D0) ) THEN

         ZZMULT = 0.D0
         CALL CHKOUT ( 'ZZMULT' )
         RETURN

      END IF

!
!     This operation should be safe. Probably.
!
      ZZMULT = A*B

      CALL CHKOUT ( 'ZZMULT' )

      RETURN
      END
