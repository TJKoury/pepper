!$Procedure CONICS ( Determine state from conic elements )

      subroutine conics ( elts, et, state )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Determine the state (position, velocity) of an orbiting body
!     from a set of elliptic, hyperbolic, or parabolic orbital
!     elements.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONIC
!     EPHEMERIS
!
! $ Declarations

      implicit none

      real(wp)              elts    ( 8 )
      real(wp)              et
      real(wp)              state   ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ELTS       I   Conic elements.
!     ET         I   Input time.
!     STATE      O   State of orbiting body at ET.
!
! $ Detailed_Input
!
!     ELTS     are conic elements describing the orbit of a body
!              around a primary. The elements are, in order:
!
!                 RP      Perifocal distance.
!                 ECC     Eccentricity.
!                 INC     Inclination.
!                 LNODE   Longitude of the ascending node.
!                 ARGP    Argument of periapse.
!                 M0      Mean anomaly at epoch.
!                 T0      Epoch.
!                 MU      Gravitational parameter.
!
!              Units are km, rad, rad/sec, km**3/sec**2. The epoch
!              is given in ephemeris seconds past J2000. The same
!              elements are used to describe all three types
!              (elliptic, hyperbolic, and parabolic) of conic orbit.
!
!     ET       is the time at which the state of the orbiting body
!              is to be determined, in ephemeris seconds J2000.
!
! $ Detailed_Output
!
!     STATE    is the state (position and velocity) of the body at
!              time ET. Components are x, y, z, dx/dt, dy/dt, dz/dt.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the eccentricity supplied is less than 0, the error
!         SPICE(BADECCENTRICITY) is signaled.
!
!     2)  If a non-positive periapse distance is supplied, the error
!         SPICE(BADPERIAPSEVALUE) is signaled.
!
!     3)  If a non-positive value for the attracting mass is supplied,
!         the error SPICE(BADGM) is signaled.
!
!     4)  If ELTS is such that the resulting orbit at periapsis has
!         either its position or velocity equal to zero, or the square
!         of the resulting specific angular momentum's magnitude is
!         zero, an error is signaled by a routine in the call tree of
!         this routine. This is an indication of invalid ELTS elements.
!
!     5)  If ET is such that the offset in time from periapsis, at which
!         the state is to be determined, is so large that there is a
!         danger of floating point overflow during computation, an error
!         is signaled by a routine in the call tree of this routine.
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
!     Let VINIT contain the initial state of a spacecraft relative to
!     the center of a planet at epoch ET, and let GM be the gravitation
!     parameter of the planet. The call
!
!        CALL OSCELT ( VINIT, ET, GM, ELTS )
!
!     produces a set of osculating elements describing the nominal
!     orbit that the spacecraft would follow in the absence of all
!     other bodies in the solar system and non-gravitational forces
!     on the spacecraft.
!
!     Now let STATE contain the state of the same spacecraft at some
!     other epoch, LATER. The difference between this state and the
!     state predicted by the nominal orbit at the same epoch can be
!     computed as follows.
!
!        CALL CONICS ( ELTS, LATER, NOMINAL )
!        CALL VSUBG  ( NOMINAL, STATE, 6, DIFF )
!
!        WRITE (*,*) 'Perturbation in x, dx/dt = ', DIFF(1), DIFF(4)
!        WRITE (*,*) '                y, dy/dt = ', DIFF(2), DIFF(5)
!        WRITE (*,*) '                z, dz/dt = ', DIFF(3), DIFF(6)
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
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.1.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary entries in $Revisions section.
!
!        Added entry #4 and updated entry #5 in $Exceptions section.
!
!-    SPICELIB Version 4.0.0, 26-MAR-1998 (WLT)
!
!        There was a coding error in the computation of the mean
!        anomaly in the parabolic case. This problem has been
!        corrected.
!
!-    SPICELIB Version 3.0.1, 15-OCT-1996 (WLT)
!
!        Corrected a typo in the description of the units associated
!        with the input elements.
!
!-    SPICELIB Version 3.0.0, 12-NOV-1992 (WLT)
!
!        The routine was re-written to make use of NAIF's universal
!        variables formulation for state propagation (PROP2B). As
!        a result, several problems were simultaneously corrected.
!
!        A major bug was fixed that caused improper state evaluations
!        for ET's that precede the epoch of the elements in the
!        elliptic case.
!
!        A danger of non-convergence in the solution of Kepler's
!        equation has been eliminated.
!
!        In addition to this reformulation of CONICS checks were
!        installed that ensure the elements supplied are physically
!        meaningful. Eccentricity must be non-negative. The
!        distance at periapse and central mass must be positive. If
!        not errors are signaled.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 19-APR-1991 (WLT)
!
!        An error in the hyperbolic state generation was corrected.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     state from conic elements
!
!-&


! $ Revisions
!
!-    SPICELIB Version 3.0.0, 12-NOV-1992 (WLT)
!
!        The routine was re-written to make use of NAIF's universal
!        variables formulation for state propagation (PROP2B). As
!        a result, several problems were simultaneously corrected.
!
!        A major bug was fixed that caused improper state
!        evaluations for ET's that precede the epoch of the
!        elements in the elliptic case.
!
!        A danger of non-convergence in the solution of Kepler's
!        equation has been eliminated.
!
!        In addition to this reformulation of CONICS checks were
!        installed that ensure the elements supplied are physically
!        meaningful. Eccentricity must be non-negative. The
!        distance at periapse and central mass must be positive.
!        If not errors are signaled.
!
!        These changes were prompted by the discovery that the old
!        formulation had a severe bug for elliptic orbits and
!        epochs prior to the epoch of the input elements, and by
!        the discovery that the time of flight routines had
!        problems with convergence.
!
!-    SPICELIB Version 2.0.0, 19-APR-1991 (WLT)
!
!        The original version of the routine had a bug in that
!        it attempted to restrict the hyperbolic anomaly to
!        the interval 0 to 2*PI. This has been fixed.
!
!-&


!
!     SPICELIB functions
!
      logical               return
      real(wp)              twopi

!
!     Local variables
!
      real(wp)              rp
      real(wp)              ecc
      real(wp)              inc
      real(wp)              lnode
      real(wp)              argp
      real(wp)              m0
      real(wp)              t0
      real(wp)              mu
      real(wp)              n
      real(wp)              period

      real(wp)              ainvrs
      real(wp)              dt

      real(wp)              cosi
      real(wp)              sini
      real(wp)              cosn
      real(wp)              sinn
      real(wp)              cosw
      real(wp)              sinw

      real(wp)              snci
      real(wp)              cnci

      real(wp)              basisp     ( 3 )
      real(wp)              basisq     ( 3 )
      real(wp)              pstate     ( 6 )
      real(wp)              v



!
!      The only real work required by this routine is the construction
!      of a preliminary state vector from the input elements.  Once this
!      is in hand, we can simply let the routine PROP2B do the real
!      work, free from the instabilities inherent in the classical
!      elements formulation of two-body motion.
!
!      To do this we shall construct a basis of vectors that lie in the
!      plane of the orbit.  The first vector P shall point towards the
!      position of the orbiting body at periapse.  The second
!      vector Q shall point along the velocity vector of the body at
!      periapse.
!
!      The only other consideration is determining an epoch, TP, of
!      this state and the delta time ET - TP.
!


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'CONICS' )
      end if


!
!     Unpack the element vector.
!
      rp     = elts(1)
      ecc    = elts(2)
      inc    = elts(3)
      lnode  = elts(4)
      argp   = elts(5)
      m0     = elts(6)
      t0     = elts(7)
      mu     = elts(8)


!
!     Handle all of the exceptions first.
!

      if ( ecc .lt. 0 ) then
         call setmsg ( 'The eccentricity supplied was negative. Only '// &
                       'positive values are meaningful.  The value '  // &
                       'was #'                  )
         call errdp  ( '#', ecc                 )
         call sigerr ( 'SPICE(BADECCENTRICITY)' )
         call chkout ( 'CONICS'                 )
         return
      end if

      if ( rp .le. 0 ) then
         call setmsg ( 'The value of periapse range supplied was '    // &
                       'non-positive.  Only positive values are '     // &
                       'allowed.  The value supplied was #. '         )
         call errdp  ( '#', rp                                        )
         call sigerr ( 'SPICE(BADPERIAPSEVALUE)'                      )
         call chkout ( 'CONICS'                                       )
         return
      end if


      if ( mu .le. 0 ) then
         call setmsg ( 'The value of GM supplied was '                // &
                       'non-positive.  Only positive values are '     // &
                       'allowed.  The value supplied was #. '         )
         call errdp  ( '#', mu                                        )
         call sigerr ( 'SPICE(BADGM)'                                 )
         call chkout ( 'CONICS' )
         return
      end if

!
!     First construct the orthonormal basis vectors that span the orbit
!     plane.
!
      cosi      =  dcos ( inc   )
      sini      =  dsin ( inc   )
      cosn      =  dcos ( lnode )
      sinn      =  dsin ( lnode )
      cosw      =  dcos ( argp  )
      sinw      =  dsin ( argp  )

      snci      =  sinn * cosi
      cnci      =  cosn * cosi

      basisp(1) =  cosn * cosw  -  snci * sinw
      basisp(2) =  sinn * cosw  +  cnci * sinw
      basisp(3) =  sini * sinw

      basisq(1) = -cosn * sinw  -  snci * cosw
      basisq(2) = -sinn * sinw  +  cnci * cosw
      basisq(3) =  sini * cosw

!
!     Next construct the state at periapse.
!
!     The position at periapse is just BASISP scaled by the distance
!     at periapse.
!
!     The velocity must be constructed so that we can get an orbit
!     of this shape.  Recall that the magnitude of the specific angular
!     momentum vector is given by DSQRT ( MU*RP*(1+ECC) )
!     The velocity will be given by V * BASISQ.  But we must have the
!     magnitude of the cross product of position and velocity be
!     equal to DSQRT ( MU*RP*(1+ECC) ). So we must have
!
!        RP*V = DSQRT( MU*RP*(1+ECC) )
!
!     so that:
!
      v = dsqrt( mu*(1.0_wp + ecc) / rp )

      call vscl( rp, basisp, pstate(1) )
      call vscl( v,  basisq, pstate(4) )

!
!     Finally compute DT the elapsed time since the epoch of periapse.
!     Ellipses first, since they are the most common.
!
      if ( ecc .lt. 1.0_wp ) then

!
!        Recall that:
!
!        N ( mean motion ) is given by DSQRT( MU / A**3 ).
!        But since, A = RP / ( 1 - ECC ) ...
!
         ainvrs = (1.0_wp - ecc)   / rp
         n      = sqrt(mu*ainvrs) * ainvrs
         period = twopi()         / n

!
!        In general the mean anomaly is given by
!
!           M  = (T - TP) * N
!
!        Where TP is the time of periapse passage.  M0 is the mean
!        anomaly at time T0 so that
!        Thus
!
!           M0 = ( T0 - TP ) * N
!
!        So TP = T0-M0/N hence the time since periapse at time ET
!        is given by ET - T0 + M0/N.  Finally, since elliptic orbits are
!        periodic, we can mod this value by the period of the orbit.
!
         dt = mod ( et - t0 + m0/n, period )

!
!     Hyperbolas next.
!
      else if ( ecc .gt. 1 ) then


!
!        Again, recall that:
!
!        N ( mean motion ) is given by DSQRT( MU / |A**3| ).
!        But since, |A| = RP / ( ECC - 1 ) ...
!
         ainvrs = (ecc - 1.0_wp)    / rp
         n      = dsqrt(mu*ainvrs) * ainvrs
         dt     = et - t0          + m0/n

!
!     Finally, parabolas.
!
      else

         n  = dsqrt( mu / (2.0_wp*rp) ) / rp
         dt = et - t0                + m0/n

      end if

!
!     Now let PROP2B do the work of propagating the state.
!
      call prop2b ( mu, pstate, dt, state )

      call chkout ( 'CONICS' )
      return
      end
