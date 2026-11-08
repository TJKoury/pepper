!$Procedure KEPLEQ ( Solve Kepler's Equation --- Equinoctial Form )

      real(wp) function kepleq (ml,h,k)
      use pepper_kinds, only: wp

! $ Abstract
!
!     Solve the equinoctial version of Kepler's equation.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SPK
!
! $ Declarations

      implicit none

      real(wp)          ml
      real(wp)          h
      real(wp)          k

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ML         I   Mean longitude.
!     H          I   h component of equinoctial elements.
!     K          I   k component of equinoctial elements.
!
!     The function returns the solution to the equinoctial version of
!     Kepler's equation, given the mean longitude and the h and k
!     components of the equinoctial elements.
!
! $ Detailed_Input
!
!     ML       is the mean longitude of some body following two body
!              motion. (Mean longitude = Mean anomaly + argument of
!              periapse + longitude of ascending node.)
!
!     H        is the h component of the equinoctial element set
!              ( h = ECC*SIN( arg of periapse + long ascending node) )
!
!     K        is the k component of the equinoctial element set
!              ( k = ECC*COS( arg of periapse + long ascending node) )
!
! $ Detailed_Output
!
!     The function returns the solution to the equinoctial version of
!     Kepler's equation, given the mean longitude and the h and k
!     components of the equinoctial elements.
!
!     The solution is the value of F such that
!
!        ML = F + H * COS(F) - K * SIN(F)
!
!     Note that ECC = DSQRT ( K*K + H*H )
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the sum of the squares of H and K is not less than .9,
!         the error SPICE(ECCOUTOFBOUNDS) is signaled.
!
!     2)  If the iteration for a solution to the equinoctial Kepler's
!         equation does not converge in 10 or fewer steps, the error
!         SPICE(NOCONVERGENCE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine solves the equinoctial element version of
!     Kepler's equation.
!
!        ML = F + H * COS(F) - K * SIN(F)
!
!     Here F is an offset from the eccentric anomaly E.
!
!        F = E - argument of periapse - longitude of ascending node.
!
!     where E is eccentric anomaly.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  W. Owen and R. Vaughan, "Optical Navigation Program
!          Mathematical Models," JPL Engineering Memorandum 314-513,
!          August 9, 1991.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 26-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated
!        $Procedure section for consistency with KPSOLV routine.
!
!-    SPICELIB Version 1.0.0, 11-DEC-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Solve the equinoctial version of Kepler's equation
!
!-&


!
!     SPICELIB Functions
!
      real(wp)              kpsolv

!
!     Local variables
!
      real(wp)              evec ( 2 )
      real(wp)              e2

!
!     Make sure that H and K are in the expected range.
!
      e2 = h*h + k*k

      if ( e2 >= 0.81_wp ) then

         kepleq = 0.0_wp
         call chkin  ( 'KEPLEQ' )
         call setmsg ( 'The values of H and K supplied to KEPLEQ ' &
         //            'must satisfy the inequality H*H + K*K < ' &
         //            'ECC**2 where ECC is the eccentricity ' &
         //            'threshold of 0.9.  The values of H and K ' &
         //            'are: # and # respectively. H*H + K*K = ' &
         //            '#. ' )

         call errdp  ( '#', h  )
         call errdp  ( '#', k  )
         call errdp  ( '#', e2 )
         call sigerr ( 'SPICE(ECCOUTOFBOUNDS)' )
         call chkout ( 'KEPLEQ' )
         return

      end if

!
!     Instead of solving the equation
!
!            ML  = F + H*DCOS(F) - K*DSIN(F)
!
!     We set X equal to F - ML and solve the equivalent equation
!
!            0   = X + H*DCOS(ML+X) - K*DSIN(ML+X)
!
!                = X + H*{DCOS(ML)*DCOS(X) - DSIN(ML)*DSIN(X)}
!                    - K*{DSIN(ML)*DCOS(X) + DCOS(ML)*DSIN(X)}
!
!                = X + { H*DCOS(ML) - K*DSIN(ML) }*DCOS(X)
!                    - { H*DSIN(ML) + K*DCOS(ML) }*DSIN(X)
!
!
!     We can rearrange this to:
!
!                                 -                    -     -       -
!                                |  DCOS(ML)  -DSIN(ML) |   | DCOS(X) |
!            0 = X + [ H  -K ] * |  DSIN(ML)   DCOS(ML) | * | DSIN(X) |
!                                 -                    -     -       -
!
!     Finally if we let
!
!                                        -                    -
!                                       |  DCOS(ML)  -DSIN(ML) |
!      EVEC =  [ EX  EY ] = [ -H  K ] * |  DSIN(ML)   DCOS(ML) |
!                                        -                    -
!
!     and
!
!              DCOS(X)
!      U(X) =  DSIN(X)
!
!     Then we can rewrite the equation as:
!
!        0  =  X - < EVEC, U(X) >
!
!     where <,> denotes the dot product operation.  Note that X
!     is necessarily in the range from -ECC to ECC where ECC = | EVEC |
!
!     Once we've computed X, F is just ML + X.
!
!     For those of you who are fans of the classical keplerian
!     elements:
!
!        x = F - ML = E - M
!
!     where E denotes eccentric anomaly and M denotes mean anomaly.
!
!     The routine KPEVEC returns the value of X that solves
!     the equation X - < EVEC, UVEC(X) >
!

      evec(1) = -h*dcos(ml) + k*dsin(ml)
      evec(2) =  h*dsin(ml) + k*dcos(ml)
      kepleq  =  ml         + kpsolv( evec )

      return
      end
