!$Procedure ELLTOF ( Elliptic time of flight )

      subroutine elltof ( ma, ecc, e )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Solve the time of flight equation MA = E - e sin(E) for the
!     elliptic eccentric anomaly E, given mean anomaly the MA and
!     the eccentricity ECC.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONIC
!
! $ Declarations

      implicit none

      real(wp)              ma
      real(wp)              ecc
      real(wp)              e

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MA         I   Mean anomaly at epoch.
!     ECC        I   Eccentricity.
!     E          O   Elliptic eccentric anomaly.
!
! $ Detailed_Input
!
!     MA       is the elliptic mean anomaly of an orbiting body at
!              some epoch t,
!
!                                3 1/2
!                 MA = (t-T)(mu/a )
!
!              where T is the time of periapsis passage, a is
!              the semi-major axis of the orbit, and mu is the
!              gravitational parameter of the primary body.
!
!     ECC      is the eccentricity of the orbit.
!
! $ Detailed_Output
!
!     E        is the corresponding eccentric anomaly. This is the
!              solution to the time of flight equation
!
!                 MA = E - e sin(E)
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the eccentricity (ECC) is outside the range [0,1),
!         the error SPICE(WRONGCONIC) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Iterate to solve
!
!        f(E,MA,e) = E - e sin(E) - MA = 0
!
! $ Examples
!
!     ELLTOF, HYPTOF, and PARTOF are used by CONICS.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
!          Astrodynamics," Dover Publications Inc., 1971.
!
!     [2]  E. W. Ng, "A General Algorithm for the Solution of Kepler's
!          Equation for Elliptic Orbits", Cel. Mech. 20, pp.243-249,
!          1979.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
!
!        The declaration for the SPICELIB function PI is now
!        preceded by an EXTERNAL statement declaring PI to be an
!        external function. This removes a conflict with any
!        compilers that have a PI intrinsic function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     elliptic time of flight
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
!
!        The routine now verifies that the eccentricity is in the
!        proper range---[0,1)---before proceeding.
!
!-&


!
!     SPICELIB functions
!
      logical               return
      real(wp)              dcbrt
      real(wp)              halfpi
      external              pi
      real(wp)              pi
      real(wp)              twopi

!
!     Local parameters
!
      real(wp)              tol
      parameter           ( tol = 1.0e-15_wp )

!
!     Local variables
!
      real(wp)              m
      real(wp)              mprime
      integer(ip)           n

      real(wp)              m0
      real(wp)              a
      real(wp)              b
      real(wp)              q
      real(wp)              r
      real(wp)              qr
      real(wp)              y

      real(wp)              fn
      real(wp)              deriv
      real(wp)              deriv2
      real(wp)              change



!
!     Standard SPICE error handling.
!
      if ( return () ) return

      if ( ecc < 0.0_wp  .or.  ecc >= 1.0_wp ) then
         call sigerr ( 'SPICE(WRONGCONIC)' )
         return
      end if


!
!     For reasons of numerical stability, we would like to restrict
!     our solution to the interval [0,pi]. Because E, M, and sin E
!     are always positive or negative together, we can pretend that M
!     is positive and adjust the sign of the result. And for M, E > pi,
!     we can define
!
!           M = 2n pi + M'     and    E = 2n pi + E'
!
!     where M' and E' are in the interval [-pi,pi]. Solving for E'
!     gives us E.
!
!     So, we begin by reducing the input mean anomaly to [0,pi].
!
      m = abs ( ma )

      if ( m > pi() ) then
         n      = int ( (m-pi()) / twopi() ) + 1
         mprime = m - n * twopi()
      else
         n      = 0
         mprime = m
      end if

      m = abs ( mprime )

!
!     The convergence of the iterative scheme below depends on a good
!     initial estimate for E.
!
!     For small eccentricity, the initial estimate E = M is sufficient.
!     However, as the eccentricity increases, so does the number of
!     iterations required for convergence. For sufficiently large
!     eccentricity, this estimate leads to divergence.
!
!     Ng [2] notes that the function y(M,e)
!
!           E - M
!          -------  =  sin(e y + M)
!             e
!
!     increases and decreases monotonically when M is in the ranges
!     [0,M0] and [m0,pi], respectively.
!
!     When M0 < M < pi, where M0 = (pi/2) - e, the cubic
!             -   -
!
!                              pi - M  2        pi - M    pi - M
!           B(M,e) = 1 - (1 -  -------)  (1 + 2 ------- - -------)
!                              pi - M0          pi - M0    1 + e
!
!     provides a good initial estimate of y for all values of e.
!
!
      m0 = halfpi() - ecc

      if ( m >= m0 ) then

         a = pi() - m
         b = pi() - m0

         y = 1.0_wp - (1.0_wp - a/b)**2 * (1.0_wp + 2.0_wp*a/b - a/(1.0_wp+ecc))
         e = ecc * sin(ecc*y + m) + m

!
!     The situation is a little more troublesome, however, when M < M0.
!     For small eccentricity, the cubic
!
!                                  2
!           A(M,e) = 1 - (1 - M/M0)  (1 + 2M/M0 - M/(1-e) )
!
!     gives a reasonable first estimate of y. However, as e -> 1,
!     successive approximations of the form
!
!                             k           k
!           C (M,e) = 1 - (-1)  (1 - M/M0)
!            k
!
!     are used, where k = 4 for e > 0.7, and k = 8 for e > 0.85.
!
!     For high eccentricity (e > 0.96) and low mean anomaly (M < 0.05),
!     these successive approximations eventually fail. Fortunately, in
!     just these cases, the cubic
!
!                           3    2  1/3           3    2  1/3
!           D(M,e) = [r + (q  + r )]     + [r - (q  + r )]
!
!     where
!
!           r = 3M/e,   q = (2/e)(1 - e)
!
!     provides a reasonable estimate of E directly.
!
!
      else if ( ecc <= 0.7_wp ) then

         y = 1.0_wp - (1.0_wp - m/m0)**2 * (1.0_wp + 2.0_wp*m/m0 - m/(1.0_wp-ecc))
         e = ecc * sin(ecc*y + m) + m

      else if ( ecc <= 0.85_wp ) then

         y = 1.0_wp - (1.0_wp - m/m0)**4
         e = ecc * sin(ecc*y + m) + m

      else if ( ecc <= 0.96_wp .or. m > 0.05_wp ) then

         y = 1.0_wp - (1.0_wp - m/m0)**8
         e = ecc * sin(ecc*y + m) + m

      else

         q  = (2.0_wp/ecc) * (1.0_wp - ecc)
         r  = 3.0_wp * (m/ecc)
         qr = sqrt ( q**3 + r**2 )

         e  = dcbrt ( r + qr ) + dcbrt ( r - qr )

      end if


!
!     Use the Newton second-order method,
!
!                                           2
!          E    = E  - (f/f')*(1 + f*f''/2f' )
!           i+1    i
!
!     where
!
!          f   = E - e sin(E) - M
!          f'  = 1 - e cos(E)
!          f'' =     e sin(E)
!
      change = 1.0_wp

      do while ( abs ( change ) > tol )

         fn     = e    - ecc * sin(e) - m
         deriv  = 1.0_wp - ecc * cos(e)
         deriv2 =        ecc * sin(e)

         change = (fn/deriv) * ( 1.0_wp + (fn*deriv2) / (2.0_wp*deriv**2) )
         e      = e - change

      end do

!
!     "Unwrap" E' into the actual value of E.
!
      if ( mprime < 0 ) then
         e = -e
      end if

      if ( n > 0 ) then
         e = e + n * twopi()
      end if

      if ( ma < 0 ) then
         e = -e
      end if


      return
      end
