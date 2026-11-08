!$Procedure HRMESP ( Hermite polynomial interpolation, equal spacing )

      SUBROUTINE HRMESP ( N,  FIRST,  STEP,  YVALS,  X,  WORK,  F,  DF )

! $ Abstract
!
!     Evaluate, at a specified point, a Hermite interpolating polynomial
!     for a specified set of equally spaced abscissa values and
!     corresponding pairs of function and function derivative values.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     INTERPOLATION
!     POLYNOMIAL
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               N
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      YVALS ( 2*N )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      WORK  ( 2*N, 2 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      DF

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   Number of points defining the polynomial.
!     FIRST      I   First abscissa value.
!     STEP       I   Step size.
!     YVALS      I   Ordinate and derivative values.
!     X          I   Point at which to interpolate the polynomial.
!     WORK      I-O  Work space array.
!     F          O   Interpolated function value at X.
!     DF         O   Interpolated function's derivative at X.
!
! $ Detailed_Input
!
!     N        is the number of points defining the polynomial.
!              The array YVALS contains 2*N elements.
!
!     FIRST,
!     STEP     are, respectively, a starting abscissa value and a
!              step size that define the set of abscissa values
!
!                 FIRST   +   (I-1) * STEP,     I = 1, ..., N
!
!              STEP must be non-zero.
!
!     YVALS    is an array of length 2*N containing ordinate and
!              derivative values for each point in the domain
!              defined by FIRST, STEP, and N. The elements
!
!                 YVALS( 2*I - 1 )
!                 YVALS( 2*I     )
!
!              give the value and first derivative of the output
!              polynomial at the abscissa value
!
!                 FIRST   +   (I-1) * STEP
!
!              where I ranges from 1 to N.
!
!     WORK     is a work space array. It is used by this routine
!              as a scratch area to hold intermediate results.
!
!
!     X        is the abscissa value at which the interpolating
!              polynomial and its derivative are to be evaluated.
!
! $ Detailed_Output
!
!     F,
!     DF       are the value and derivative at X of the unique
!              polynomial of degree 2*N-1 that fits the points and
!              derivatives defined by FIRST, STEP, and YVALS.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If STEP is zero, the error SPICE(INVALIDSTEPSIZE) is
!         signaled.
!
!     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is
!         signaled.
!
!     3)  This routine does not attempt to ward off or diagnose
!         arithmetic overflows.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Users of this routine must choose the number of points to use
!     in their interpolation method. The authors of Reference [1] have
!     this to say on the topic:
!
!        Unless there is solid evidence that the interpolating function
!        is close in form to the true function f, it is a good idea to
!        be cautious about high-order interpolation. We
!        enthusiastically endorse interpolations with 3 or 4 points, we
!        are perhaps tolerant of 5 or 6; but we rarely go higher than
!        that unless there is quite rigorous monitoring of estimated
!        errors.
!
!     The same authors offer this warning on the use of the
!     interpolating function for extrapolation:
!
!        ...the dangers of extrapolation cannot be overemphasized:
!        An interpolating function, which is perforce an extrapolating
!        function, will typically go berserk when the argument x is
!        outside the range of tabulated values by more than the typical
!        spacing of tabulated points.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Fit a 7th degree polynomial through the points ( x, y, y' )
!
!           ( -1,      6,       3 )
!           (  1,      8,      11 )
!           (  3,   2210,    5115 )
!           (  5,  78180,  109395 )
!
!        and evaluate this polynomial at x = 2.
!
!        The returned value of ANSWER should be 141.D0, and the
!        returned derivative value should be 456.D0, since the unique
!        7th degree polynomial that fits these constraints is
!
!                     7       2
!           f(x)  =  x   +  2x  + 5
!
!
!        Example code begins here.
!
!
!              PROGRAM HRMESP_EX1
!              IMPLICIT NONE
!
!              DOUBLE PRECISION      ANSWER
!              DOUBLE PRECISION      DERIV
!              DOUBLE PRECISION      FIRST
!              DOUBLE PRECISION      STEP
!              DOUBLE PRECISION      YVALS (8)
!              DOUBLE PRECISION      WORK  (8,2)
!              INTEGER               N
!
!
!              N         =       4
!
!              YVALS(1)  =       6.D0
!              YVALS(2)  =       3.D0
!              YVALS(3)  =       8.D0
!              YVALS(4)  =      11.D0
!              YVALS(5)  =    2210.D0
!              YVALS(6)  =    5115.D0
!              YVALS(7)  =   78180.D0
!              YVALS(8)  =  109395.D0
!
!              FIRST     =  -1.D0
!              STEP      =   2.D0
!
!              CALL HRMESP ( N,    FIRST, STEP,   YVALS,
!             .              2.D0, WORK,  ANSWER, DERIV )
!
!              WRITE (*,*) 'ANSWER = ', ANSWER
!              WRITE (*,*) 'DERIV  = ', DERIV
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         ANSWER =    141.00000000000000
!         DERIV  =    456.00000000000000
!
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
!          "Numerical Recipes -- The Art of Scientific Computing,"
!          chapters 3.0 and 3.1, Cambridge University Press, 1986.
!
!     [2]  S. Conte and C. de Boor, "Elementary Numerical Analysis -- An
!          Algorithmic Approach," 3rd Edition, p 64, McGraw-Hill, 1980.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.2, 01-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard. Added code
!        example's solution.
!
!        Fixed formula in the description of YVALS argument in
!        $Detailed_Input.
!
!-    SPICELIB Version 1.2.1, 28-JAN-2014 (NJB)
!
!        Fixed a few comment typos.
!
!-    SPICELIB Version 1.2.0, 31-JAN-2002 (EDW)
!
!        Added the use of DBLE to convert integer values
!        used in DOUBLE PRECISION calculations.
!
!-    SPICELIB Version 1.1.0, 28-DEC-2001 (NJB)
!
!        Blanks following final newline were truncated to
!        suppress compilation warnings on the SGI-N32 platform.
!
!-    SPICELIB Version 1.0.0, 01-MAR-2000 (NJB)
!
!-&


! $ Index_Entries
!
!     interpolate function using Hermite polynomial
!     Hermite interpolation
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      NEWX
      DOUBLE PRECISION      TEMP
      DOUBLE PRECISION      XI
      DOUBLE PRECISION      XIJ

      INTEGER               I
      INTEGER               J
      INTEGER               NEXT
      INTEGER               PREV
      INTEGER               THIS


!
!     Check in only if an error is detected.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     No data, no interpolation.
!
      IF ( N .LT. 1 ) THEN

         CALL CHKIN  ( 'HRMESP'                              )
         CALL SETMSG ( 'Array size must be positive; was #.' )
         CALL ERRINT ( '#',  N                               )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                  )
         CALL CHKOUT ( 'HRMESP'                              )
         RETURN

      END IF


!
!     The step size must be non-zero.
!
      IF ( STEP .EQ. 0.D0 ) THEN

         CALL CHKIN  ( 'HRMESP'                 )
         CALL SETMSG ( 'Step size was zero.'    )
         CALL SIGERR ( 'SPICE(INVALIDSTEPSIZE)' )
         CALL CHKOUT ( 'HRMESP'                 )
         RETURN

      END IF

!
!     We can simplify the interpolation problem by shifting
!     and scaling the abscissa values so that they start at 1
!     and are separated by a unit step. All we need to do here is
!     shift and scale X.
!
      NEWX  =  (  ( X - FIRST ) / STEP  )  +  1.D0

!
!     For consistency with our scaled horizontal axis, we'll have
!     scale our local derivative values by STEP, and scale our final
!     computed derivative by 1/STEP.
!
!     Copy the input array into WORK.  Scale the derivatives at this
!     step. After this, the first column of WORK represents the first
!     column of our triangular interpolation table.
!
      DO I = 1, 2*N-1, 2
         WORK(I,1) = YVALS(I)
      END DO

      DO I = 2, 2*N, 2
         WORK(I,1) = YVALS(I) * STEP
      END DO

!
!     Compute the second column of the interpolation table: this
!     consists of the N-1 values obtained by evaluating the
!     first-degree interpolants at NEWX. We'll also evaluate the
!     derivatives of these interpolants at NEWX and save the results in
!     the second column of WORK. Because the derivative computations
!     depend on the function computations from the previous column in
!     the interpolation table, and because the function interpolation
!     overwrites the previous column of interpolated function values,
!     we must evaluate the derivatives first.
!

      DO I = 1, N-1

         C1     = DBLE( (I+1) )  - NEWX
         C2     =  NEWX  - DBLE(I)

!
!        The second column of WORK contains interpolated derivative
!        values.
!
!        The odd-indexed interpolated derivatives are simply the input
!        derivatives, after scaling.
!
         PREV = 2*I  - 1
         THIS = PREV + 1
         NEXT = THIS + 1

         WORK( PREV,  2 )  =    WORK( THIS, 1 )

!
!        The even-indexed interpolated derivatives are the slopes of
!        the linear interpolating polynomials for adjacent input
!        abscissa/ordinate pairs. No scaling is needed here.
!
         WORK( THIS, 2 ) = ( WORK(NEXT, 1) - WORK(PREV, 1) )

!
!        The first column of WORK contains interpolated function values.
!        The odd-indexed entries are the linear Taylor polynomials,
!        each input abscissa value, evaluated at NEWX.
!
         TEMP  = WORK(THIS, 1) * ( NEWX - DBLE(I) )  +  WORK(PREV, 1)


         WORK( THIS, 1 ) = (   C1 * WORK( PREV, 1 ) &
                             + C2 * WORK( NEXT, 1 )  )

         WORK( PREV, 1 ) = TEMP

      END DO

!
!     The last column entries were not computed by the preceding loop;
!     compute them now.
!
      WORK ( 2*N - 1,  2 )  =    WORK( 2*N,     1 )
      WORK ( 2*N - 1,  1 )  =    WORK( 2*N,     1 ) * ( NEWX - N ) &
                               + WORK( 2*N - 1, 1 )

!
!     Compute columns 3 through 2*N of the table.
!
      DO J = 2, 2*N-1

         DO I = 1, 2*N-J
!
!           In the theoretical construction of the interpolation table,
!           there are 2*N abscissa values, since each input abscissa
!           value occurs with multiplicity two. In this theoretical
!           construction, the Jth column of the interpolation table
!           contains results of evaluating interpolants that span J+1
!           consecutive abscissa values. The indices XI and XIJ below
!           are used to pick the correct abscissa values out of this
!           sequence of 2*N values.
!
            XI       =   DBLE( ( I     + 1 ) / 2 )
            XIJ      =   DBLE( ( I + J + 1 ) / 2 )

            C1       =     XIJ   -  NEWX
            C2       =     NEWX  -  XI

            DENOM    =     XIJ   -  XI

!
!           Compute the interpolated derivative at NEWX for the Ith
!           interpolant. This is the derivative with respect to NEWX of
!           the expression for the interpolated function value, which is
!           the second expression below. This derivative computation
!           is done first because it relies on the interpolated function
!           values from the previous column of the interpolation table.
!
!           The derivative expression here corresponds to equation
!           2.35 on page 64 in reference [2].
!
            WORK(I,2) = (              C1  * WORK(I,   2) &
                          +            C2  * WORK(I+1, 2) &
                          + ( WORK(I+1, 1) - WORK(I,   1) )  )  / DENOM
!
!           Compute the interpolated function value at NEWX for the Ith
!           interpolant.
!
            WORK(I,1) = ( C1 * WORK(I,1)  +  C2 * WORK(I+1,1) ) / DENOM

         END DO

      END DO

!
!     Our interpolated function value is sitting in WORK(1,1) at this
!     point.  The interpolated derivative is located in WORK(1,2).
!     We must undo the scaling of the derivative. We've already
!     checked that STEP is non-zero.
!
      F   =  WORK( 1, 1 )
      DF  =  WORK( 1, 2 )  /  STEP

      RETURN
      END
