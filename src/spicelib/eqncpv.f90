!$Procedure EQNCPV (Equinoctial Elements to position and velocity)

      SUBROUTINE EQNCPV ( ET, EPOCH, EQEL, RAPOL, DECPOL, STATE )

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

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      EQEL ( 9 )
      DOUBLE PRECISION      RAPOL
      DOUBLE PRECISION      DECPOL
      DOUBLE PRECISION      STATE ( 6 )

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
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      KEPLEQ
      LOGICAL               RETURN

      EXTERNAL              TWOPI
      EXTERNAL              KEPLEQ
      EXTERNAL              RETURN

!
!     LOCAL VARIABLES
!
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      CAN
      DOUBLE PRECISION      CF
      DOUBLE PRECISION      CN
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      DI
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      NODEDT
      DOUBLE PRECISION      NODE
      DOUBLE PRECISION      DLPDT
      DOUBLE PRECISION      DLP
      DOUBLE PRECISION      MLDT

      DOUBLE PRECISION      TRANS(3,3)
      DOUBLE PRECISION      SA
      DOUBLE PRECISION      CA
      DOUBLE PRECISION      SD
      DOUBLE PRECISION      CD

      DOUBLE PRECISION      DX
      DOUBLE PRECISION      DX1
      DOUBLE PRECISION      DY
      DOUBLE PRECISION      DY1
      DOUBLE PRECISION      P


      DOUBLE PRECISION      Q
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      PRATE
      DOUBLE PRECISION      RB
      DOUBLE PRECISION      SAN
      DOUBLE PRECISION      SF
      DOUBLE PRECISION      SN
      DOUBLE PRECISION      TEMP ( 3  )

      DOUBLE PRECISION      X1,Y1
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K

      DOUBLE PRECISION      L
      DOUBLE PRECISION      ML
      DOUBLE PRECISION      NFAC
      DOUBLE PRECISION      EECAN
      DOUBLE PRECISION      XHOLD(6)
      DOUBLE PRECISION      VF   (3)
      DOUBLE PRECISION      VG   (3)


      LOGICAL               FIRST


!
!     Constants computed on first pass
!
      DOUBLE PRECISION      PI2

      SAVE                  PI2
      SAVE                  FIRST

      DATA                  FIRST / .TRUE. /

!
!     Standard SPICE exception handling code.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'EQNCPV' )

!
!     The first time through this routine we fetch the various
!     constants we need for this routine.
!
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         PI2   = TWOPI()
      END IF

!
!     Take care of the various errors that can arise with the
!     input elements.
!
      IF ( EQEL(1) .LE. 0.0D0 ) THEN

         CALL SETMSG ( 'The semi-major axis supplied to EQNCPV ' &
         //            'was non-positive. The value is required ' &
         //            'to be positive by this routine. The ' &
         //            'value supplied was #. ' )

         CALL ERRDP  ( '#', EQEL(1) )
         CALL SIGERR ( 'SPICE(BADSEMIAXIS)'  )
         CALL CHKOUT ( 'EQNCPV' )
         RETURN

      END IF

      ECC = DSQRT ( EQEL(2)*EQEL(2) + EQEL(3)*EQEL(3) )

      IF ( ECC .GT. 0.9D0 ) THEN

         CALL SETMSG ( 'The routine EQNCPV can reliably evaluate ' &
         //            'states from equinoctial elements if the ' &
         //            'eccentricity of the orbit associated ' &
         //            'with the elements is less than 0.9.  The ' &
         //            'eccentricity associated with the ' &
         //            'elements supplies is #.  The values of H ' &
         //            'and K are: # and # respectively. ' )

         CALL ERRDP  ( '#', ECC     )
         CALL ERRDP  ( '#', EQEL(2) )
         CALL ERRDP  ( '#', EQEL(3) )
         CALL SIGERR ( 'SPICE(ECCOUTOFRANGE)'  )
         CALL CHKOUT ( 'EQNCPV' )
         RETURN

      END IF




!
!     Form the transformation from planetary equator to the inertial
!     reference frame.
!
      SA          =  DSIN(RAPOL)
      CA          =  DCOS(RAPOL)

      SD          =  DSIN(DECPOL)
      CD          =  DCOS(DECPOL)

      TRANS(1,1)  =  -SA
      TRANS(1,2)  =  -CA*SD
      TRANS(1,3)  =   CA*CD

      TRANS(2,1)  =   CA
      TRANS(2,2)  =  -SA*SD
      TRANS(2,3)  =   SA*CD

      TRANS(3,1)  =   0.0D0
      TRANS(3,2)  =   CD
      TRANS(3,3)  =   SD
!
!     Compute the offset of the input epoch (ET) from the
!     epoch of the elements.
!
      DT     =  ET - EPOCH

!
!     Obtain the elements, rates, and other parameters. First get
!     the semi-major axis.
!
      A     = EQEL(1)

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
      DLPDT = EQEL(7)
      DLP   = DT*DLPDT

      CAN   = DCOS( DLP )
      SAN   = DSIN( DLP )

      H     = EQEL(2)*CAN + EQEL(3)*SAN
      K     = EQEL(3)*CAN - EQEL(2)*SAN

!
!     The mean longitude at epoch is in the 4th element of EQEL.
!
      L     = EQEL(4)

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
      NODEDT = EQEL(9)
      NODE   = DT*NODEDT

      CN     = DCOS( NODE )
      SN     = DSIN( NODE )

      P      = EQEL(5)*CN + EQEL(6)*SN
      Q      = EQEL(6)*CN - EQEL(5)*SN

      MLDT  = EQEL(8)
!
!     We compute the rate of change of the argument of periapse
!     by taking the difference between the rate of the longitude
!     of periapse and the rate of the node.
!
      PRATE = DLPDT - NODEDT

!
!     Form Broucke's beta parameter
!
      B     = DSQRT( 1.D0 - H*H - K*K )
      B     = 1.D0/( 1.D0 + B )

!
!     Construct the coordinate axes
!
      DI     = 1.0D0/(1.D0 + P*P + Q*Q)

      VF(1)  = (1.D0 - P*P + Q*Q) * DI
      VF(2)  =  2.D0 * P   * Q    * DI
      VF(3)  = -2.D0 * P          * DI

      VG(1)  =  2.D0 * P * Q      * DI
      VG(2)  = (1.D0 + P*P - Q*Q) * DI
      VG(3)  =  2.D0 * Q          * DI

!
!     Compute the mean longitude
!
      ML  =  L + DMOD( MLDT*DT, PI2 )

!
!     Obtain the eccentric longitude from Kepler's equation
!
      EECAN  =  KEPLEQ( ML, H, K )

!
!     Trigonometric functions of the eccentric longitude
!
      SF = DSIN(EECAN)
      CF = DCOS(EECAN)

!
!     Position in the orbit plane
!
      X1 = A*( (1.D0-(B*H**2))*CF + (H*K*B*SF - K) )
      Y1 = A*( (1.D0-(B*K**2))*SF + (H*K*B*CF - H) )

!
!     Radial distance and functions of the radial distance
!
      RB = H*SF + K*CF
      R  = A*(1.0D0-RB)
      RA = (MLDT*A*A)/R
!
!
!     Velocity in the orbit plane
!
      DX1 = RA * ( -SF + H*B*RB )
      DY1 = RA * (  CF - K*B*RB )

!
!     Correction factor for periapsis rate
!
      NFAC = 1.0D0 - ( DLPDT/MLDT )

!
!     Include precession in velocity
!
      DX  = NFAC*DX1 - PRATE*Y1
      DY  = NFAC*DY1 + PRATE*X1

!
!     Form the planetary mean equator position vector
!
      CALL VLCOM ( X1, VF, Y1, VG, XHOLD )

!
!     Form the planetary mean equator velocity vector
!
      TEMP (1) = -NODEDT*XHOLD(2)
      TEMP (2) =  NODEDT*XHOLD(1)
      TEMP (3) =  0.0D0

      CALL VLCOM3 ( 1.0D0, TEMP, DX, VF, DY, VG, XHOLD(4) )

!
!     Transform to an inertial state vector
!
      CALL MXV ( TRANS, XHOLD(1), STATE(1) )
      CALL MXV ( TRANS, XHOLD(4), STATE(4) )

      CALL CHKOUT ( 'EQNCPV' )
      RETURN
      END
