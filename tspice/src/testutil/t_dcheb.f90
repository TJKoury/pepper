!$Procedure      T_DCHEB ( Numeric derivative from Chebyshev fit )

      SUBROUTINE T_DCHEB ( FUNC, X, DELTA, NTERMS, NRTAIN, F, DF )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Numerically estimate the derivative of a scalar function
!     at a specified point by fitting a Chebyshev expansion
!     to the function and differentiating the expansion.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     INTERPOLATION
!     MATH
!     POLYNOMIAL
!
! $ Declarations

      DOUBLE PRECISION      FUNC
      EXTERNAL FUNC
      DOUBLE PRECISION      X
      DOUBLE PRECISION      DELTA
      INTEGER               NTERMS
      INTEGER               NRTAIN
      DOUBLE PRECISION      F
      DOUBLE PRECISION      DF

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     MAXSIZ     P   Maximum number of terms in expansion.
!     FUNC       I   Function to be approximated.
!     X          I   Point at which to compute derivative.
!     DELTA      I   Half-width of domain of fit.
!     NTERMS     I   Number of terms in fitted expansion.
!     NRTAIN     I   Number of retained terms used for derivative.
!     F          O   Value of expansion at X.
!     DF         O   Derivative estimate at X.
!
! $ Detailed_Input
!
!     FUNC           is the function to be differentiated.  FUNC must
!                    accept a single, double precision input argument
!                    and must return a double precision value.  FUNC
!                    should be declared EXTERNAL in the caller of this
!                    routine.
!
!
!     X              is the point in the domain of FUNC at which the
!                    derivative of FUNC is to be estimated.
!
!
!     DELTA          is the half width of the interval over which
!                    FUNC is to be fitted by a Chebyshev expansion.
!                    The interval used is
!
!                       [ X - DELTA,  X + DELTA ]
!
!
!     NTERMS         is the number of terms in the Chebyshev expansion
!                    used to approximate FUNC. The degree of the
!                    expansion is NTERMS-1.
!
!     NRTAIN         is the number of retained terms in the Chebyshev
!                    expansion. These terms are used to compute the
!                    derivative estimate. NRTAIN should be at least 1
!                    and cannot exceed NTERMS.
!
! $ Detailed_Output
!
!     F              is the result of evaluating the retained Chebyshev
!                    expansion at X.
!
!
!     DF             is the result of evaluating the derivative of the
!                    retained Chebyshev expansion at X.
!
!
! $ Parameters
!
!     MAXSIZ         is the maximum number of terms in the Chebyshev
!                    expansion. This is the maximum allowed value of
!                    NTERMS.
!
! $ Exceptions
!
!     1)  If NTERMS is less than 1 or greater than MAXSIZ, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2)  If NRTAIN is less than 1 or greater than NTERMS, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     3)  If DELTA is not strictly positive the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     4)  This routine does not attempt to ward off or diagnose
!         arithmetic overflows.
!
!     5)  See the routines CHBFIT and CHBINT for exceptions specific
!         to those routines.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides a numeric estimate of the derivative of
!     a function of one variable. The derivative is computed by
!
!        1) Fitting to the input function, over the interval
!
!              X - DELTA  :  X + DELTA
!
!           a Chebyshev expansion of a degree NTERMS-1.
!
!        2) Estimating the derivative of the function at X by
!           differentiating the first NRTAIN terms of the
!           expansion.
!
!     A heuristic comment:
!
!        It can be advantageous to use a large value of NTERMS together
!        with a smaller value of NRTAIN. The number of function samples
!        used to compute each Chebyshev coefficient is determined by
!        NTERMS (see the SUPPORT library routine CHBFIT for details).
!        Throwing away the higher-order terms of the expansion before
!        differentiating can remove noise from the function and improve
!        accuracy of the derivative estimate.
!
!
! $ Examples
!
!     See usage in the TSPICE routine T_DZZILU.
!
! $ Restrictions
!
!     1)  Maximum number of terms in the expansion is limited by the
!         parameter MAXSIZ.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 06-APR-2012 (NJB)
!
!-&

! $ Index_Entries
!
!     differentiate a Chebyshev expansion of a function
!     discrete derivative from Chebyshev fit
!     numeric derivative from Chebyshev fit
!
!-&



!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = 25 )

!
!     Local variables
!
      DOUBLE PRECISION      COEFFS ( MAXSIZ )
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      WORK   ( MAXSIZ )
      DOUBLE PRECISION      X2S    ( 2 )

      INTEGER               DEGP


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'T_DCHEB' )

!
!     Check NTERMS.
!
      IF ( ( NTERMS .LT. 1 ) .OR. ( NTERMS .GT. MAXSIZ ) ) THEN

         CALL SETMSG ( 'NTERMS must be in the range 1:# but was #.' )
         CALL ERRINT ( '#',  MAXSIZ                                 )
         CALL ERRINT ( '#',  NTERMS                                 )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                     )
         CALL CHKOUT ( 'T_DCHEB'                                    )
         RETURN

      END IF

!
!     Check NRTAIN.
!
      IF ( ( NRTAIN .LT. 1 ) .OR. ( NRTAIN .GT. NTERMS ) ) THEN

         CALL SETMSG ( 'NRTAIN must be in the range 1:# but was #.' )
         CALL ERRINT ( '#',  NTERMS                                 )
         CALL ERRINT ( '#',  NRTAIN                                 )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                     )
         CALL CHKOUT ( 'T_DCHEB'                                    )
         RETURN

      END IF

!
!     Check DELTA.
!
      IF ( DELTA .LE. 0D0 )  THEN

         CALL SETMSG ( 'DELTA must be positive but was #.' )
         CALL ERRDP  ( '#',  DELTA                         )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'            )
         CALL CHKOUT ( 'T_DCHEB'                           )
         RETURN

      END IF

!
!     Set domain bounds for Chebyshev fit:
!
      LEFT  = X - DELTA
      RIGHT = X + DELTA

!
!     Set transformation parameters:
!
      X2S( 1 ) = X
      X2S( 2 ) = DELTA

!
!     Fit N-term Cheby expansion to the input function on [LEFT,
!     RIGHT].
!
      CALL CHBFIT ( FUNC, LEFT, RIGHT, NTERMS, WORK, COEFFS )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'T_DCHEB' )
         RETURN
      END IF

!
!     Find the first derivative of the expansion at X.
!
      DEGP = NRTAIN - 1
      CALL CHBINT ( COEFFS, DEGP, X2S, X, F, DF )


      CALL CHKOUT ( 'T_DCHEB' )
      RETURN
      END
