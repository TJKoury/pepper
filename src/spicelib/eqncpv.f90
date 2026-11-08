!$Procedure EQNCPV (Equinoctial Elements to position and velocity)

      subroutine eqncpv ( et, epoch, eqel, rapol, decpol, state )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Compute the state (position and velocity) of an object whose
!     trajectory is described via equinoctial elements relative to some
!     fixed plane (usually the equatorial plane of some planet).
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      real(wp)              et
      real(wp)              epoch
      real(wp)              eqel ( 9 )
      real(wp)              rapol
      real(wp)              decpol
      real(wp)              state ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch in seconds past J2000 to find state
!     EPOCH      I   Epoch of elements in seconds past J2000
!     EQEL       I   Array of equinoctial elements
!     RAPOL      I   Right Ascension of the pole of the reference plane
!     DECPOL     I   Declination of the pole of the reference plane
!     STATE      O   State of the object described by EQEL.
!
! $ Detailed_Input
!
!     ET       is the epoch (ephemeris time) at which the state
!              of the target body is to be computed. ET is measured
!              in seconds past the J2000 epoch.
!
!     EPOCH    is the epoch of the equinoctial elements in seconds
!              past the J2000 epoch.
!
!     EQEL     is an array of 9 double precision numbers that are the
!              equinoctial elements for some orbit expressed relative to
!              the equatorial frame of the central body defined as
!
!              -  The Z-axis of the equatorial frame is the direction
!                 of the pole of the central body relative to some
!                 inertial frame;
!
!              -  The X-axis is given by the cross product of the Z-axis
!                 of the inertial frame with the direction of the pole
!                 of the central body; and
!
!              -  The Y-axis completes a right handed frame.
!
!              If the X-axis of the equatorial frame is aligned with the
!              X-axis of the inertial frame, then the X-axis of the
!              equatorial frame will be located at 90 degrees + RAPOL in
!              the inertial frame.
!
!              The specific arrangement of the elements is spelled out
!              below:
!
!                 EQEL(1)   is the semi-major axis (A) of the orbit in
!                           km.
!
!                 EQEL(2)   is the value of H at the specified epoch.
!                           ( E*SIN(ARGP+NODE) ).
!
!                 EQEL(3)   is the value of K at the specified epoch
!                           ( E*COS(ARGP+NODE) ).
!
!                 EQEL(4)   is the mean longitude (MEAN0+ARGP+NODE) at
!                           the epoch of the elements measured in
!                           radians.
!
!                 EQEL(5)   is the value of P (TAN(INC/2)*SIN(NODE)) at
!                           the specified epoch.
!
!                 EQEL(6)   is the value of Q (TAN(INC/2)*COS(NODE)) at
!                           the specified epoch.
!
!                 EQEL(7)   is the rate of the longitude of periapse
!                           (dARGP/dt + dNODE/dt ) at the epoch of
!                           the elements. This rate is assumed to hold
!                           for all time. The rate is measured in
!                           radians per second.
!
!                 EQEL(8)   is the derivative of the mean longitude
!                           ( dM/dt + dARGP/dt + dNODE/dt ). This
!                           rate is assumed to be constant and is
!                           measured in radians/second.
!
!                 EQEL(9)   is the rate of the longitude of the
!                           ascending node ( dNODE/dt). This rate is
!                           measured in radians per second.
!
!              where
!
!                 INC       is the inclination of the orbit,
!
!                 ARGP      is the argument of periapse,
!
!                 NODE      is longitude of the ascending node, and
!
!                 E         is eccentricity of the orbit.
!
!     RAPOL    is the Right Ascension of the pole of the reference plane
!              with respect to some inertial frame (measured in
!              radians).
!
!     DECPOL   is the Declination of the pole of the reference plane
!              with respect to some inertial frame (measured in
!              radians).
!
! $ Detailed_Output
!
!     STATE    is the state of the object described by EQEL relative to
!              the inertial frame used to define RAPOL and DECPOL. Units
!              are in km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the eccentricity corresponding to the input elements is
!         greater than 0.9, the error SPICE(ECCOUTOFRANGE) is signaled.
!
!     2)  If the semi-major axis of the elements is non-positive, the
!         error SPICE(BADSEMIAXIS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine evaluates the input equinoctial elements for
!     the specified epoch and return the corresponding state.
!
!     This routine was adapted from a routine provided by
!     Bob Jacobson of the Planetary Dynamics Group of
!     the Navigation and Flight Mechanics Section at JPL.
!
! $ Examples
!
!     Suppose you have classical elements and rates of
!     change of the ascending node and argument of periapse
!     for some satellite of the earth.
!
!     By transforming the classical elements
!     this routine can be used to compute the state of the
!     object at an arbitrary epoch. The code below illustrates
!     how you might do this.
!
!     The table below illustrates the meanings of the various
!     variables used in the discussion below.
!
!           Variable     Meaning
!           --------     ----------------------------------
!           A            Semi-major axis in km
!           ECC          Eccentricity of orbit
!           INC          Inclination of orbit
!           NODE         Longitude of the ascending node at epoch
!           OMEGA        Argument of periapse at epoch
!           M            Mean anomaly at epoch
!           DMDT         Mean anomaly rate in radians/second
!           DNODE        Rate of change of longitude of ascending node
!                        in radians/second
!           DARGP        Rate of change of argument of periapse in
!                        radians/second
!           EPOCH        is the epoch of the elements in seconds past
!                        the J2000 epoch.
!
!
!        EQEL(1) = A
!        EQEL(2) = ECC * DSIN ( OMEGA + NODE )
!        EQEL(3) = ECC * DCOS ( OMEGA + NODE )
!
!        EQEL(4) = M + OMEGA + NODE
!
!        EQEL(5) = TAN(INC/2.0D0) * DSIN(NODE)
!        EQEL(6) = TAN(INC/2.0D0) * DCOS(NODE)
!
!        EQEL(7) = DARGP
!        EQEL(8) = DARGP + DMDT + DNODE
!        EQEL(9) = DNODE
!
!
!        We shall compute the state of the satellite in the
!        pole and equator reference system.
!
!        RAPOL   = -HALFPI()
!        DECPOL  =  HALFPI()
!
!
!        Now compute the state at the desired epoch ET.
!
!        CALL EQNCPV ( ET, EPOCH, EQEL, RAPOL, DECPOL, STATE )
!
! $ Restrictions
!
!     1)  The equinoctial elements used by this routine are taken
!         from  "Tangent" formulation of equinoctial elements
!
!            p = tan(inclination/2) * sin(R.A. of ascending node)
!            q = tan(inclination/2) * cos(R.A. of ascending node)
!
!         Other formulations use Sine instead of Tangent. We shall
!         call these the "Sine" formulations.
!
!            p = sin(inclination/2) * sin(R.A. of ascending node)
!            q = sin(inclination/2) * cos(R.A. of ascending node)
!
!         If you have equinoctial elements from this alternative
!         formulation you should replace p and q  by the
!         expressions below.
!
!            P = P / DSQRT ( 1.0D0 - P*P - Q*Q )
!            Q = Q / DSQRT ( 1.0D0 - P*P - Q*Q )
!
!         This will convert the Sine formulation to the Tangent
!         formulation.
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
!     R.A. Jacobson      (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added SPK
!        required reading.
!
!-    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    SPICELIB Version 1.0.1, 31-JAN-2008 (BVS)
!
!        Removed non-standard header section heading
!        'Declarations_of_external_functions'.
!
!-    SPICELIB Version 1.0.0, 08-JAN-1997 (WLT) (RAJ)
!
!-&


! $ Index_Entries
!
!     Compute a state from equinoctial elements
!
!-&


!
!     SPICELIB Functions.
!
      real(wp)              twopi
      real(wp)              kepleq
      logical               return

      external              twopi
      external              kepleq
      external              return

!
!     LOCAL VARIABLES
!
      real(wp)              a
      real(wp)              b
      real(wp)              can
      real(wp)              cf
      real(wp)              cn
      real(wp)              dt
      real(wp)              di
      real(wp)              ecc
      real(wp)              nodedt
      real(wp)              node
      real(wp)              dlpdt
      real(wp)              dlp
      real(wp)              mldt

      real(wp)              trans(3,3)
      real(wp)              sa
      real(wp)              ca
      real(wp)              sd
      real(wp)              cd

      real(wp)              dx
      real(wp)              dx1
      real(wp)              dy
      real(wp)              dy1
      real(wp)              p


      real(wp)              q
      real(wp)              r
      real(wp)              ra
      real(wp)              prate
      real(wp)              rb
      real(wp)              san
      real(wp)              sf
      real(wp)              sn
      real(wp)              temp ( 3  )

      real(wp)              x1,y1
      real(wp)              h
      real(wp)              k

      real(wp)              l
      real(wp)              ml
      real(wp)              nfac
      real(wp)              eecan
      real(wp)              xhold(6)
      real(wp)              vf   (3)
      real(wp)              vg   (3)


      logical               first


!
!     Constants computed on first pass
!
      real(wp)              pi2

      save                  pi2
      save                  first

      data                  first / .true. /

!
!     Standard SPICE exception handling code.
!
      if ( return() ) return


!
!     The first time through this routine we fetch the various
!     constants we need for this routine.
!
      if ( first ) then
         first = .false.
         pi2   = twopi()
      end if

!
!     Take care of the various errors that can arise with the
!     input elements.
!
      if ( eqel(1) <= 0.0_wp ) then

         call setmsg ( 'The semi-major axis supplied to EQNCPV ' &
         //            'was non-positive. The value is required ' &
         //            'to be positive by this routine. The ' &
         //            'value supplied was #. ' )

         call errdp  ( '#', eqel(1) )
         call sigerr ( 'SPICE(BADSEMIAXIS)'  )
         return

      end if

      ecc = dsqrt ( eqel(2)*eqel(2) + eqel(3)*eqel(3) )

      if ( ecc > 0.9_wp ) then

         call setmsg ( 'The routine EQNCPV can reliably evaluate ' &
         //            'states from equinoctial elements if the ' &
         //            'eccentricity of the orbit associated ' &
         //            'with the elements is less than 0.9.  The ' &
         //            'eccentricity associated with the ' &
         //            'elements supplies is #.  The values of H ' &
         //            'and K are: # and # respectively. ' )

         call errdp  ( '#', ecc     )
         call errdp  ( '#', eqel(2) )
         call errdp  ( '#', eqel(3) )
         call sigerr ( 'SPICE(ECCOUTOFRANGE)'  )
         return

      end if




!
!     Form the transformation from planetary equator to the inertial
!     reference frame.
!
      sa          =  dsin(rapol)
      ca          =  dcos(rapol)

      sd          =  dsin(decpol)
      cd          =  dcos(decpol)

      trans(1,1)  =  -sa
      trans(1,2)  =  -ca*sd
      trans(1,3)  =   ca*cd

      trans(2,1)  =   ca
      trans(2,2)  =  -sa*sd
      trans(2,3)  =   sa*cd

      trans(3,1)  =   0.0_wp
      trans(3,2)  =   cd
      trans(3,3)  =   sd
!
!     Compute the offset of the input epoch (ET) from the
!     epoch of the elements.
!
      dt     =  et - epoch

!
!     Obtain the elements, rates, and other parameters. First get
!     the semi-major axis.
!
      a     = eqel(1)

!
!     Recall that H and K at the epoch of the elements are in
!     EQEL(2) and EQEL(3) respectively.
!
!        H_0 = E*Sin(ARGP_0 + NODE_0 )
!        K_0 = E*Cos(ARGP_0 + NODE_0 )
!
!     The values of H and K at the epoch of interest is
!
!        H_dt = E*Sin(ARGP_0 + NODE_0 + dt*d(ARGP+NODE)/dt )
!        K_dt = E*Cos(ARGP_0 + NODE_0 + dt*d(ARGP+NODE)/dt )
!
!     But using the identities Sin(A+B) = Sin(A)Cos(B) + Sin(B)Cos(A)
!                              Cos(A+B) = Cos(A)Cos(B) - Sin(A)Sin(B)
!
!     We can re-write the expression for H_dt and K_dt as
!
!        H_dt = E*Sin(ARGP_0 + NODE_0 )Cos(dt*d(ARGP+NODE)/dt )
!             + E*Cos(ARGP_0 + NODE_0 )Sin(dt*d(ARGP+NODE)/dt )
!
!
!             = H_0 * Cos(dt*d(ARGP+NODE)/dt )
!             + K_0 * Sin(dt*d(ARGP+NODE)/dt )
!     and
!
!        K_dt = E*Cos(ARGP_0 + NODE_0)Cos(dt*d(ARGP+NODE)/dt)
!             - E*Sin(ARGP_0 + NODE_0)Sin(dt*d(ARGP+NODE)/dt)
!
!             = K_0 * Cos(dt*d(ARGP+NODE)/dt)
!             - H_0 * Sin(dt*d(ARGP+NODE)/dt)
!
!     Thus we can easily compute H and K at the current epoch.
!     Recall that the derivative of the longitude of periapse is
!     in entry 7 of EQEL.
!
      dlpdt = eqel(7)
      dlp   = dt*dlpdt

      can   = dcos( dlp )
      san   = dsin( dlp )

      h     = eqel(2)*can + eqel(3)*san
      k     = eqel(3)*can - eqel(2)*san

!
!     The mean longitude at epoch is in the 4th element of EQEL.
!
      l     = eqel(4)

!
!     The values for P and Q at epoch are stored in entries 5 and 6
!     of the array EQEL.  Recall that
!
!        P_0 = TAN(INC/2)*SIN(NODE_0)
!        Q_0 = TAN(INC/2)*COS(NODE_0)
!
!     We need P and Q offset from the initial epoch by DT.
!
!        P   = TAN(INC/2)*SIN(NODE_0 + dt*dNODE/dt)
!        Q   = TAN(INC/2)*COS(NODE_0 + dt*dNODE/dt)
!
!     Applying the same identities as we did before we have
!
!        P    = P_0 * Cos( dt*dNODE/dt ) + Q_0 * Sin( dt*dNODE/dt )
!        Q    = Q_0 * Cos( dt*dNODE/dt ) - P_0 * Sin( dt*dNODE/dt )
!
      nodedt = eqel(9)
      node   = dt*nodedt

      cn     = dcos( node )
      sn     = dsin( node )

      p      = eqel(5)*cn + eqel(6)*sn
      q      = eqel(6)*cn - eqel(5)*sn

      mldt  = eqel(8)
!
!     We compute the rate of change of the argument of periapse
!     by taking the difference between the rate of the longitude
!     of periapse and the rate of the node.
!
      prate = dlpdt - nodedt

!
!     Form Broucke's beta parameter
!
      b     = dsqrt( 1.0_wp - h*h - k*k )
      b     = 1.0_wp/( 1.0_wp + b )

!
!     Construct the coordinate axes
!
      di     = 1.0_wp/(1.0_wp + p*p + q*q)

      vf(1)  = (1.0_wp - p*p + q*q) * di
      vf(2)  =  2.0_wp * p   * q    * di
      vf(3)  = -2.0_wp * p          * di

      vg(1)  =  2.0_wp * p * q      * di
      vg(2)  = (1.0_wp + p*p - q*q) * di
      vg(3)  =  2.0_wp * q          * di

!
!     Compute the mean longitude
!
      ml  =  l + dmod( mldt*dt, pi2 )

!
!     Obtain the eccentric longitude from Kepler's equation
!
      eecan  =  kepleq( ml, h, k )

!
!     Trigonometric functions of the eccentric longitude
!
      sf = dsin(eecan)
      cf = dcos(eecan)

!
!     Position in the orbit plane
!
      x1 = a*( (1.0_wp-(b*h**2))*cf + (h*k*b*sf - k) )
      y1 = a*( (1.0_wp-(b*k**2))*sf + (h*k*b*cf - h) )

!
!     Radial distance and functions of the radial distance
!
      rb = h*sf + k*cf
      r  = a*(1.0_wp-rb)
      ra = (mldt*a*a)/r
!
!
!     Velocity in the orbit plane
!
      dx1 = ra * ( -sf + h*b*rb )
      dy1 = ra * (  cf - k*b*rb )

!
!     Correction factor for periapsis rate
!
      nfac = 1.0_wp - ( dlpdt/mldt )

!
!     Include precession in velocity
!
      dx  = nfac*dx1 - prate*y1
      dy  = nfac*dy1 + prate*x1

!
!     Form the planetary mean equator position vector
!
      call vlcom ( x1, vf, y1, vg, xhold )

!
!     Form the planetary mean equator velocity vector
!
      temp (1) = -nodedt*xhold(2)
      temp (2) =  nodedt*xhold(1)
      temp (3) =  0.0_wp

      call vlcom3 ( 1.0_wp, temp, dx, vf, dy, vg, xhold(4) )

!
!     Transform to an inertial state vector
!
      call mxv ( trans, xhold(1), state(1) )
      call mxv ( trans, xhold(4), state(4) )

      return
      end
