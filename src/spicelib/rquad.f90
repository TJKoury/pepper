!$Procedure RQUAD ( Roots of a quadratic equation )

      subroutine rquad (  a,  b,  c,  root1,  root2  )
      use pepper_kinds, only: wp

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

      implicit none

      real(wp)              a
      real(wp)              b
      real(wp)              c
      real(wp)              root1 ( 2 )
      real(wp)              root2 ( 2 )

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
      logical               return

!
!     Local variables
!
      real(wp)              con
      real(wp)              discrm
      real(wp)              lin
      real(wp)              scale
      real(wp)              sqr

      logical               zeroed


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     The degree of the equation is zero unless at least one of the
!     second or first degree coefficients is non-zero.
!
      if (  ( a == 0.0_wp )  .and.  ( b == 0.0_wp )  )  then

         call setmsg ( 'Both 1st and 2nd degree coefficients are zero.')
         call sigerr ( 'SPICE(DEGENERATECASE)'                         )
         return

      end if

!
!     If we can scale the coefficients without zeroing any of them out,
!     we will do so, to help prevent overflow.
!
      scale  = max ( abs(a), abs(b), abs(c) )


      zeroed =      (  ( a /= 0.0_wp ) .and. ( a / scale == 0.0_wp )  ) &
               .or. (  ( b /= 0.0_wp ) .and. ( b / scale == 0.0_wp )  ) &
               .or. (  ( c /= 0.0_wp ) .and. ( c / scale == 0.0_wp )  )


      if ( .not. zeroed ) then

         sqr = a / scale
         lin = b / scale
         con = c / scale

      else

         sqr = a
         lin = b
         con = c

      end if

!
!     If the second-degree coefficient is non-zero, we have a bona fide
!     quadratic equation, as opposed to a linear equation.
!
      if ( sqr /= 0.0_wp ) then
!
!        Compute the discriminant.
!
         discrm = lin**2  -  4.0_wp * sqr * con

!
!        A non-negative discriminant indicates that the roots are
!        real.
!
         if ( discrm >= 0.0_wp ) then
!
!           The imaginary parts of both roots are zero.
!
            root1(2) = 0.0_wp
            root2(2) = 0.0_wp
!
!           We can take advantage of the fact that CON/SQR is the
!           product of the roots to improve the accuracy of the root
!           having the smaller magnitude.  We compute the larger root
!           first and then divide CON/SQR by it to obtain the smaller
!           root.
!
            if ( lin < 0.0_wp ) then
!
!              ROOT1 will contain the root of larger magnitude.
!
               root1(1) =  ( - lin + dsqrt(discrm) )  /  ( 2.0_wp * sqr )

               root2(1) =  ( con / sqr )  /   root1(1)

            else if ( lin > 0.0_wp ) then
!
!              ROOT2 will contain the root of larger magnitude.
!
               root2(1) =  ( - lin - dsqrt(discrm) )  /  ( 2.0_wp * sqr )

               root1(1) =  ( con / sqr )  /   root2(1)

            else
!
!              The roots have the same magnitude.
!
               root1(1)  =    dsqrt( discrm )  /  ( 2.0_wp * sqr )

               root2(1)  =  - root1(1)

            end if

!
!        The only other possibility is that the roots are complex.
!
         else
!
!           The roots are complex conjugates, so they have equal
!           magnitudes.
!
            root1(1)  =  -lin                /   ( 2.0_wp * sqr )
            root1(2)  =   dsqrt( -discrm )   /   ( 2.0_wp * sqr )

            root2(1)  =   root1(1)
            root2(2)  =  -root1(2)

         end if


!
!     If the second-degree coefficient is zero, we actually have a
!     linear equation.
!
      else if ( lin /= 0.0_wp ) then

         root1(1) =  - con / lin
         root1(2) =    0.0_wp

!
!        We set the second root equal to the first, rather than
!        leaving it undefined.
!
         call moved ( root1, 2, root2 )

      end if


      return
      end
