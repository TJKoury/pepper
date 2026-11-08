!$Procedure RQUAD ( Roots of a quadratic equation )

      SUBROUTINE RQUAD (  A,  B,  C,  ROOT1,  ROOT2  )

! $ Abstract
!
!     Find the roots of a quadratic equation.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     POLYNOMIAL
!     ROOT
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      ROOT1 ( 2 )
      DOUBLE PRECISION      ROOT2 ( 2 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   Coefficient of quadratic term.
!     B          I   Coefficient of linear term.
!     C          I   Constant.
!     ROOT1      O   Root built from positive discriminant term.
!     ROOT2      O   Root built from negative discriminant term.
!
! $ Detailed_Input
!
!     A,
!     B,
!     C        are the coefficients of a quadratic polynomial
!
!                      2
!                 A * x   +  B * x  +  C.
!
! $ Detailed_Output
!
!     ROOT1,
!     ROOT2    are the roots of the equation
!
!                      2
!                 A * x   +  B * x  +  C = 0.
!
!
!              ROOT1 and ROOT2 are both arrays of length 2. The first
!              element of each array is the real part of a root; the
!              second element contains the complex part of the same
!              root.
!
!              When A is non-zero, ROOT1 represents the root
!
!                               _____________
!                              /  2
!                 - B   +    \/  B    -   4AC
!                 ---------------------------
!                               2A
!
!
!              and ROOT2 represents the root
!
!                               _____________
!                              /  2
!                 - B   -    \/  B    -   4AC
!                 --------------------------- .
!                               2A
!
!
!              When A is zero and B is non-zero, ROOT1 and ROOT2 both
!              represent the root
!
!                 - C / B.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input coefficients A and B are both zero, the error
!         SPICE(DEGENERATECASE) is signaled. The output arguments
!         are not modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     1)   Humor us and suppose we want to compute the "golden ratio."
!
!          The quantity r is defined by the equation
!
!             1/r = r/(1-r),
!
!          which is equivalent to
!
!              2
!             r   +  r  -  1  =  0.
!
!          The following code fragment does the job.
!
!
!             C
!             C     Compute "golden ratio." The root we want,
!             C
!             C                ___
!             C               /
!             C        -1 + \/  5
!             C        -----------,
!             C             2
!             C
!             C
!             C     is contained in ROOT1.
!             C
!
!                   CALL RQUAD ( 1.D0, 1.D0, -1.D0, ROOT1, ROOT2 )
!
!                   PRINT *, 'The "golden ratio" is ', ROOT1(1)
!
!
!     2)   The equation,
!
!              2
!             x   +  1  =  0
!
!          can be solved by the code fragment
!
!
!             C
!             C     Let's do one with imaginary roots just for fun.
!             C
!
!                   CALL RQUAD ( 1.D0,  0.D0,  1.D0,  ROOT1,  ROOT2 )
!
!                   PRINT *, 'ROOT1 is ', ROOT1
!                   PRINT *, 'ROOT2 is ', ROOT2
!
!          The printed results will be something like:
!
!
!             ROOT1 is 0.000000000000000    1.000000000000000
!             ROOT2 is 0.000000000000000   -1.000000000000000
!
! $ Restrictions
!
!     1)  No checks for overflow of the roots are performed.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 10-JUL-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     roots of a quadratic equation
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      CON
      DOUBLE PRECISION      DISCRM
      DOUBLE PRECISION      LIN
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SQR

      LOGICAL               ZEROED


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RQUAD' )
      END IF

!
!     The degree of the equation is zero unless at least one of the
!     second or first degree coefficients is non-zero.
!
      IF (  ( A .EQ. 0.D0 )  .AND.  ( B .EQ. 0.D0 )  )  THEN

         CALL SETMSG ( 'Both 1st and 2nd degree coefficients are zero.')
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'RQUAD'                                         )
         RETURN

      END IF

!
!     If we can scale the coefficients without zeroing any of them out,
!     we will do so, to help prevent overflow.
!
      SCALE  = MAX ( ABS(A), ABS(B), ABS(C) )


      ZEROED =      (  ( A .NE. 0.D0 ) .AND. ( A / SCALE .EQ. 0.D0 )  ) &
               .OR. (  ( B .NE. 0.D0 ) .AND. ( B / SCALE .EQ. 0.D0 )  ) &
               .OR. (  ( C .NE. 0.D0 ) .AND. ( C / SCALE .EQ. 0.D0 )  )


      IF ( .NOT. ZEROED ) THEN

         SQR = A / SCALE
         LIN = B / SCALE
         CON = C / SCALE

      ELSE

         SQR = A
         LIN = B
         CON = C

      END IF

!
!     If the second-degree coefficient is non-zero, we have a bona fide
!     quadratic equation, as opposed to a linear equation.
!
      IF ( SQR .NE. 0.D0 ) THEN
!
!        Compute the discriminant.
!
         DISCRM = LIN**2  -  4.D0 * SQR * CON

!
!        A non-negative discriminant indicates that the roots are
!        real.
!
         IF ( DISCRM .GE. 0.D0 ) THEN
!
!           The imaginary parts of both roots are zero.
!
            ROOT1(2) = 0.D0
            ROOT2(2) = 0.D0
!
!           We can take advantage of the fact that CON/SQR is the
!           product of the roots to improve the accuracy of the root
!           having the smaller magnitude.  We compute the larger root
!           first and then divide CON/SQR by it to obtain the smaller
!           root.
!
            IF ( LIN .LT. 0.D0 ) THEN
!
!              ROOT1 will contain the root of larger magnitude.
!
               ROOT1(1) =  ( - LIN + DSQRT(DISCRM) )  /  ( 2.D0 * SQR )

               ROOT2(1) =  ( CON / SQR )  /   ROOT1(1)

            ELSE IF ( LIN .GT. 0.D0 ) THEN
!
!              ROOT2 will contain the root of larger magnitude.
!
               ROOT2(1) =  ( - LIN - DSQRT(DISCRM) )  /  ( 2.D0 * SQR )

               ROOT1(1) =  ( CON / SQR )  /   ROOT2(1)

            ELSE
!
!              The roots have the same magnitude.
!
               ROOT1(1)  =    DSQRT( DISCRM )  /  ( 2.D0 * SQR )

               ROOT2(1)  =  - ROOT1(1)

            END IF

!
!        The only other possibility is that the roots are complex.
!
         ELSE
!
!           The roots are complex conjugates, so they have equal
!           magnitudes.
!
            ROOT1(1)  =  -LIN                /   ( 2.D0 * SQR )
            ROOT1(2)  =   DSQRT( -DISCRM )   /   ( 2.D0 * SQR )

            ROOT2(1)  =   ROOT1(1)
            ROOT2(2)  =  -ROOT1(2)

         END IF


!
!     If the second-degree coefficient is zero, we actually have a
!     linear equation.
!
      ELSE IF ( LIN .NE. 0.D0 ) THEN

         ROOT1(1) =  - CON / LIN
         ROOT1(2) =    0.D0

!
!        We set the second root equal to the first, rather than
!        leaving it undefined.
!
         CALL MOVED ( ROOT1, 2, ROOT2 )

      END IF


      CALL CHKOUT ( 'RQUAD' )
      RETURN
      END
