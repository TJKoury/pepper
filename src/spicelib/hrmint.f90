!$Procedure HRMINT ( Hermite polynomial interpolation  )

      subroutine hrmint ( n,  xvals,  yvals,  x,  work,  f,  df )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a Hermite interpolating polynomial at a specified
!     abscissa value.
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

      implicit none

      integer(ip)           n
      real(wp)              xvals (   n )
      real(wp)              yvals ( 2*n )
      real(wp)              x
      real(wp)              work  ( 2*n, 2 )
      real(wp)              f
      real(wp)              df

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   Number of points defining the polynomial.
!     XVALS      I   Abscissa values.
!     YVALS      I   Ordinate and derivative values.
!     X          I   Point at which to interpolate the polynomial.
!     WORK      I-O  Work space array.
!     F          O   Interpolated function value at X.
!     DF         O   Interpolated function's derivative at X.
!
! $ Detailed_Input
!
!     N        is the number of points defining the polynomial.
!              The arrays XVALS and YVALS contain N and 2*N
!              elements respectively.
!
!     XVALS    is an array of length N containing abscissa values.
!
!     YVALS    is an array of length 2*N containing ordinate and
!              derivative values for each point in the domain
!              defined by XVALS. The elements
!
!                 YVALS( 2*I - 1 )
!                 YVALS( 2*I     )
!
!              give the value and first derivative of the output
!              polynomial at the abscissa value
!
!                 XVALS(I)
!
!              where I ranges from 1 to N.
!
!     WORK     is a work space array. It is used by this routine
!              as a scratch area to hold intermediate results.
!
!     X        is the abscissa value at which the interpolating
!              polynomial and its derivative are to be evaluated.
!
! $ Detailed_Output
!
!     F,
!     DF       are the value and derivative at X of the unique
!              polynomial of degree 2N-1 that fits the points and
!              derivatives defined by XVALS and YVALS.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If two input abscissas are equal, the error
!         SPICE(DIVIDEBYZERO) is signaled.
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
!           (  0,      5,       0 )
!           (  3,   2210,    5115 )
!           (  5,  78180,  109395 )
!
!        and evaluate this polynomial at x = 2.
!
!        The returned value should be 141.0, and the returned
!        derivative value should be 456.0, since the unique 7th degree
!        polynomial that fits these constraints is
!
!                     7       2
!           f(x)  =  x   +  2x  + 5
!
!
!        Example code begins here.
!
!
!              PROGRAM HRMINT_EX1
!              IMPLICIT NONE
!
!              DOUBLE PRECISION      ANSWER
!              DOUBLE PRECISION      DERIV
!              DOUBLE PRECISION      XVALS (4)
!              DOUBLE PRECISION      YVALS (8)
!              DOUBLE PRECISION      WORK  (8,2)
!              INTEGER               N
!
!              N         =   4
!
!              XVALS(1)  =      -1.D0
!              XVALS(2)  =       0.D0
!              XVALS(3)  =       3.D0
!              XVALS(4)  =       5.D0
!
!              YVALS(1)  =       6.D0
!              YVALS(2)  =       3.D0
!              YVALS(3)  =       5.D0
!              YVALS(4)  =       0.D0
!              YVALS(5)  =    2210.D0
!              YVALS(6)  =    5115.D0
!              YVALS(7)  =   78180.D0
!              YVALS(8)  =  109395.D0
!
!              CALL HRMINT ( N, XVALS, YVALS, 2.D0, WORK, ANSWER,
!             .                                           DERIV )
!
!              WRITE (*,*) 'ANSWER = ', ANSWER
!              WRITE (*,*) 'DERIV  = ', DERIV
!
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
!-    SPICELIB Version 1.2.2, 01-OCT-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard. Fixed
!        a few more comment typos. Added IMPLICIT NONE to code
!        example.
!
!-    SPICELIB Version 1.2.1, 28-JAN-2014 (NJB)
!
!        Fixed a few comment typos.
!
!-    SPICELIB Version 1.2.0, 01-FEB-2002 (NJB) (EDW)
!
!        Bug fix: declarations of local variables XI and XIJ
!        were changed from DOUBLE PRECISION to INTEGER.
!        Note: bug had no effect on behavior of this routine.
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
      logical               return

!
!     Local variables
!
      real(wp)              c1
      real(wp)              c2
      real(wp)              denom
      real(wp)              temp

      integer(ip)           i
      integer(ip)           j
      integer(ip)           next
      integer(ip)           prev
      integer(ip)           this
      integer(ip)           xi
      integer(ip)           xij

!
!     Check in only if an error is detected.
!
      if ( return() ) then
         return
      end if

!
!     No data, no interpolation.
!
      if ( n < 1 ) then

         call chkin  ( 'HRMINT'                              )
         call setmsg ( 'Array size must be positive; was #.' )
         call errint ( '#',  n                               )
         call sigerr ( 'SPICE(INVALIDSIZE)'                  )
         call chkout ( 'HRMINT'                              )
         return

      end if

!
!     Copy the input array into WORK.  After this, the first column
!     of WORK represents the first column of our triangular
!     interpolation table.
!
      do i = 1, 2*n
         work(i,1) = yvals(i)
      end do

!
!     Compute the second column of the interpolation table: this
!     consists of the N-1 values obtained by evaluating the
!     first-degree interpolants at X. We'll also evaluate the
!     derivatives of these interpolants at X and save the results in
!     the second column of WORK. Because the derivative computations
!     depend on the function computations from the previous column in
!     the interpolation table, and because the function interpolation
!     overwrites the previous column of interpolated function values,
!     we must evaluate the derivatives first.
!

      do i = 1, n-1

         c1     =  xvals(i+1) - x
         c2     =  x          - xvals(i)
         denom  =  xvals(i+1) - xvals(i)


         if ( denom == 0.0_wp ) then

            call chkin  ( 'HRMINT'                  )
            call setmsg ( 'XVALS(#) = XVALS(#) = #' )
            call errint ( '#',   i                  )
            call errint ( '#',   i+1                )
            call errdp  ( '#',   xvals(i)           )
            call sigerr ( 'SPICE(DIVIDEBYZERO)'     )
            call chkout ( 'HRMINT'                  )
            return

         end if

!
!        The second column of WORK contains interpolated derivative
!        values.
!
!        The odd-indexed interpolated derivatives are simply the input
!        derivatives.
!
         prev = 2*i  - 1
         this = prev + 1
         next = this + 1

         work( prev,  2 )  =    work( this, 1 )

!
!        The even-indexed interpolated derivatives are the slopes of
!        the linear interpolating polynomials for adjacent input
!        abscissa/ordinate pairs.
!
         work( this, 2 ) = ( work(next, 1) - work(prev, 1) ) / denom

!
!        The first column of WORK contains interpolated function values.
!        The odd-indexed entries are the linear Taylor polynomials,
!        for each input abscissa value, evaluated at X.
!
         temp  = work(this, 1) * ( x - xvals(i) )  +  work(prev, 1)


         work( this, 1 ) = (   c1 * work( prev, 1 ) &
                             + c2 * work( next, 1 )  )  /  denom

         work( prev, 1 ) =     temp

      end do

!
!     The last column entries were not computed by the preceding loop;
!     compute them now.
!
      work ( 2*n - 1,  2 )  =    work( 2*n,     1 )
      work ( 2*n - 1,  1 )  =    work( 2*n,     1 ) * ( x - xvals(n) ) &
                               + work( 2*n - 1, 1 )

!
!     Compute columns 3 through 2*N of the table.
!
      do j = 2, 2*n-1

         do i = 1, 2*n-j
!
!           In the theoretical construction of the interpolation table,
!           there are 2*N abscissa values, since each input abscissa
!           value occurs with multiplicity two. In this theoretical
!           construction, the Jth column of the interpolation table
!           contains results of evaluating interpolants that span J+1
!           consecutive abscissa values.  The indices XI and XIJ below
!           are used to pick the correct abscissa values out of the
!           physical XVALS array, in which the abscissa values are not
!           repeated.
!
            xi       =   ( i     + 1 ) / 2
            xij      =   ( i + j + 1 ) / 2

            c1       =     xvals(xij)   -   x
            c2       =     x            -   xvals(xi)

            denom    =     xvals(xij)   -   xvals(xi)

            if ( denom == 0.0_wp ) then

               call chkin  ( 'HRMINT'                  )
               call setmsg ( 'XVALS(#) = XVALS(#) = #' )
               call errint ( '#',   xi                 )
               call errint ( '#',   xij                )
               call errdp  ( '#',   xvals(xi)          )
               call sigerr ( 'SPICE(DIVIDEBYZERO)'     )
               call chkout ( 'HRMINT'                  )
               return

            end if

!
!           Compute the interpolated derivative at X for the Ith
!           interpolant. This is the derivative with respect to X of
!           the expression for the interpolated function value, which
!           is the second expression below. This derivative computation
!           is done first because it relies on the interpolated
!           function values from the previous column of the
!           interpolation table.
!
!           The derivative expression here corresponds to equation
!           2.35 on page 64 in reference [2].
!
            work(i,2) = (              c1  * work(i,   2) &
                          +            c2  * work(i+1, 2) &
                          + ( work(i+1, 1) - work(i,   1) )  ) / denom
!
!           Compute the interpolated function value at X for the Ith
!           interpolant.
!
            work(i,1) = ( c1 * work(i,1)  +  c2 * work(i+1,1) ) / denom

         end do

      end do

!
!     Our interpolated function value is sitting in WORK(1,1) at this
!     point.  The interpolated derivative is located in WORK(1,2).
!
      f   =  work( 1, 1 )
      df  =  work( 1, 2 )

      return
      end
