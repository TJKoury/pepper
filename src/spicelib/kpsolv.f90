!$Procedure KPSOLV ( Solve Kepler's Equation --- Vector Form )

      double precision function kpsolv ( evec )

! $ Abstract
!
!     Solve the equation X = < EVEC, U(X) > where U(X) is the unit
!     vector [ COS(X), SIN(X) ] and  < , > denotes the two-dimensional
!     dot product.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ROOTS
!
! $ Declarations

      implicit none

      double precision      evec ( 2 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     EVEC       I   A 2-vector whose magnitude is less than 1.
!
!     The function returns the solution to X = < EVEC, U(X) >
!
! $ Detailed_Input
!
!     EVEC     is any two dimensional vector whose magnitude is
!              less than 1.
!
! $ Detailed_Output
!
!     The function returns the value X such that the equation
!
!        X = EVEC(1)COS(X) + EVEC(2)SIN(X).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the magnitude of EVEC is greater than or equal to 1,
!         the error SPICE(EVECOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine uses bisection and Newton's method to find
!     the root of the equation
!
!        X = EVEC(1)COS(X) + EVEC(2)SIN(X).
!
!     This equation is just a "vector form" of Kepler's equation.
!
! $ Examples
!
!     Suppose you need to solve the equation
!
!         M = E - e SIN(E)                           [ 1 ]
!
!     for E. If we let X = E - M the equation is transformed to
!
!        0 = X - e SIN( X + M )
!
!          = X - e SIN( M ) COS(X) - e COS(M) SIN ( X )
!
!     Thus if we solve the equation
!
!        X = e SIN(M) COS(X) + e COS(M) SIN(X)
!
!     we can find the value of X we can compute E.
!
!     The code fragment below illustrates how this routine can
!     be used to solve equation [1].
!
!         EVEC(1) = ECC * DSIN(M)
!         EVEC(2) = ECC * DCOS(M)
!         E       = M   + KPSOLV( EVEC )
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
!-    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 26-AUG-1997 (WLT)
!
!        KPSOLV is now given an initial value of zero so that
!        if an error condition is detected, KPSOLV will have
!        a return value.
!
!-    SPICELIB Version 1.0.0, 03-JAN-1997 (WLT)
!
!-&


! $ Index_Entries
!
!     Solve the vector form of the Kepler equation
!
!-&


!
!     MXNEWT is the number of iterations we will perform
!     in the Newtons method for finding the solution to
!     the vector form of Kepler's equation.  It has been
!     empirically determined that 5 iterations is always
!     sufficient on computers have 64 bit double precision
!     numbers.
!
      integer               mxnewt
      parameter           ( mxnewt = 5 )


      double precision      cosx
      double precision      ecc
      double precision      ecc2
      double precision      h
      double precision      k
      double precision      sinx
      double precision      x
      double precision      xl
      double precision      xm
      double precision      xu
      double precision      y0
      double precision      ypx
      double precision      yx
      double precision      yxm

      integer               i
      integer               maxit

!
!     We give the function an initial value, just in case
!     we exit without solving Kepler's equation.
!
      kpsolv = 0.0d0

      h      = evec(1)
      k      = evec(2)

      ecc2   = h*h + k*k

      if ( ecc2 .ge. 1.0d0 ) then

         call chkin  ( 'KPSOLV' )
         call setmsg ( 'The magnitude of the vector EVEC = ( #, ' &
         //            '# ) must be less than 1.  However, the ' &
         //            'magnitude of this vector is #.' )

         call errdp  ( '#', h           )
         call errdp  ( '#', k           )
         call errdp  ( '#', dsqrt(ecc2) )

         call sigerr ( 'SPICE(EVECOUTOFRANGE)'  )
         call chkout ( 'KPSOLV' )
         return

      end if

!
!     We first approximate the equation 0 = X - H * COS(X) - K * SIN(X)
!     using bisection.  If we let Y(X) = X - H * COS(X) - K * SIN(X)
!
!        Y( ECC) =  ECC - <EVEC,U(X)>  =   ECC - ECC*COS(ANGLE_X) > 0
!        Y(-ECC) = -ECC - <EVEC,U(X)>  =  -ECC - ECC*COS(ANGLE_X) < 0
!
!     where ANGLE_X is the angle between U(X) and EVEC. Thus -ECC
!     and ECC necessarily bracket the root of the equation Y(X) = 0.
!
!     Also note that Y'(X) = 1 - < EVEC, V(X) > where V(X) is the
!     unit vector given by U'(X).  Thus Y is an increasing function
!     over the interval from -ECC to ECC.
!
!     The mid point of ECC and -ECC is 0 and Y(0) = -H.  Thus
!     we can do the first bisection step without doing
!     much in the way of computations.
!

      y0  = -h
      xm  =  0.0d0
      ecc =  dsqrt(ecc2)

      if      ( y0 .gt. 0.0d0 ) then

         xu  =  0.0d0
         xl  = -ecc

      else if ( y0 .lt. 0.0d0 ) then

         xu =  ecc
         xl =  0.0d0

      else

         kpsolv = 0.0d0
         return

      end if
!
!     Iterate until we are assured of being in a region where
!     Newton's method will converge quickly.  The formula
!     below was empirically determined to give good results.
!
      maxit = min(32, max( 1, nint(1.0d0/(1.0d0-ecc)) ) )

      do i = 1, maxit

!
!        Compute the next midpoint.  We bracket XM by XL and XU just in
!        case some kind of strange rounding occurs in the computation
!        of the midpoint.
!
         xm     = max( xl, min( xu, 0.5d0*(xl + xu) ) )

!
!        Compute Y at the midpoint of XU and XL
!
         yxm = xm - h*cos(xm) - k*sin(xm)
!
!        Determine the new upper and lower bounds.
!
         if ( yxm .gt. 0.0d0 ) then
            xu = xm
         else
            xl = xm
         end if


      end do

!
!     We've bisected into a region where we can now get rapid
!     convergence using Newton's method.
!
      x   = xm

      do i = 1, mxnewt

         cosx = dcos(x)
         sinx = dsin(x)
!
!        Compute Y and Y' at X.  Use these to get the next
!        iteration for X.
!
!        For those of you who might be wondering, "Why not put
!        in a check for YX .EQ. 0 and return early if we get
!        an exact solution?"  Here's why.  An empirical check
!        of those cases where you can actually escape from the
!        Do-loop  showed that the test YX .EQ. 0 is true
!        only about once in every 10000 case of random inputs
!        of EVEC.  Thus on average the check is a waste of
!        time and we don't bother with it.
!
         yx   = x     - h*cosx - k*sinx
         ypx  = 1.0d0 + h*sinx - k*cosx
         x    = x     - yx/ypx

      end do

      kpsolv = x

      return
      end
