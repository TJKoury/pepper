!$Procedure      SPKE53 ( Evaluate a type 53 SPK data record)

      SUBROUTINE SPKE53 ( ET, RECIN, STATE )
      use pepper_legacy

! $ Abstract
!
!     Evaluates a single SPK data record from a segment of type 53
!    (Precessing Conic Propagation).
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
      DOUBLE PRECISION      RECIN  ( * )
      DOUBLE PRECISION      STATE  ( 6 )


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Target epoch.
!     RECIN      I   Data record.
!     STATE      O   State (position and velocity).
!
! $ Detailed_Input
!
!     ET          is a target epoch, specified as ephemeris seconds past
!                 J2000, at which a state vector is to be computed.
!
!     RECIN       is a data record which, when evaluated at epoch ET,
!                 will give the state (position and velocity) of some
!                 body, relative to some center, in some inertial
!                 reference frame.
!
!                 The structure of RECIN is:
!
!                 RECIN(1)             epoch of the orbit elements
!                                      in ephemeris seconds past J2000.
!                 RECIN(2)-RECIN(4)    unit trajectory pole vector
!                 RECIN(5)-RECIN(7)    unit periapsis vector
!                 RECIN(8)             semi-latus rectum---p in the
!                                      equation:
!
!                                      r = p/(1 + ECC*COS(Nu))
!
!                 RECIN(9)             eccentricity
!                 RECIN(10)            time past periapsis at epoch
!                                      in seconds
!                 RECIN(11)            J2 processing flag describing
!                                      what J2 corrections are to be
!                                      applied when the orbit is
!                                      propagated.
!
!                                      All J2 corrections are applied
!                                      if this flag has a value that
!                                      is not 1,2 or 3.
!
!                                      If the value of the flag is 3
!                                      no corrections are done.
!
!                                      If the value of the flag is 1
!                                      no corrections are computed for
!                                      the precession of the line
!                                      of apsides.  However, regression
!                                      of the line of nodes is
!                                      performed.
!
!                                      If the value of the flag is 2
!                                      no corrections are done for
!                                      the regression of the line of
!                                      nodes. However, precession of the
!                                      line of apsides is performed.
!
!                                      Note that J2 effects are computed
!                                      only if the orbit is elliptic and
!                                      does not intersect the central
!                                      body.
!
!                 RECIN(12)-RECIN(14)  unit central body pole vector
!                 RECIN(15)            central body GM
!                 RECIN(16)            central body J2
!                 RECIN(17)            central body radius
!
!                 Units are radians, km, seconds
!
! $ Detailed_Output
!
!     STATE       is the state produced by evaluating RECIN at ET.
!                 Units are km and km/sec.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     1) If the eccentricity is 1 or less than zero, the error
!        'SPICE(BADECCENTRICITY)' will be signalled.
!
!     2) If the semi-latus rectum is 0, the error
!
!        'SPICE(BADLATUSRECTUM)' is signalled.
!
!     3) If the pole vector, trajectory pole vector or periapsis vector
!        have zero length, the error 'SPICE(BADVECTOR)' is signalled.
!
! $ Particulars
!
!     This algorithm applies J2 corrections for precessing the
!     node and argument of periapse for an object orbiting an
!     oblate spheroid.
!
!     Note the effects of J2 are incorporated only for elliptic
!     orbits that do not intersect the central body.
!
!     While the derivation of the effect of the various harmonics
!     of gravitational field are beyond the scope of this header
!     the effect of the J2 term of the gravity model are as follows
!
!     The line of node precesses and the rate of precession DNode/dt
!     is given by
!                             3 n J2
!         dNode/dNu =  -  -----------------  DCOS( inc )
!                            2 (P/RPL)**2
!
!      where n is the mean motion of the orbit.
!
!     (Since this is always less than zero for oblate spheroids, this
!      should be called regression of nodes.)
!
!     The line of apsides precesses and the rate of precession DPeri/dt
!     is given by
!                              3 n J2
!         dPeri/dNu =     ----------------- ( 5*DCOS ( inc ) - 1 )
!                            2 (P/RPL)**2
!
!     Details of these formula are given in the Danby's book (see
!     literature references below).
!
!     It is assumed that this routine is used in conjunction with
!     the routine SPKR53 as shown here:
!
!        CALL SPKR53 ( HANDLE, DESCR, ET, RECIN         )
!        CALL SPKE53 (                ET, RECIN, STATE  )
!
!     where it is known in advance that the HANDLE, DESCR pair points
!     to a type 53 data segment.
!
! $ Examples
!
!     The SPKEnn routines are almost always used in conjunction with
!     the corresponding SPKRnn routines, which read the records from
!     SPK files.
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment.  As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely.  Given that understanding, however, the SPKRnn
!     routines might be used to examine raw segment data before
!     evaluating it with the SPKEnn routines.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 53 ) THEN
!
!              CALL SPKR53 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE53 ( ET, RECORD, STATE )
!                  .
!                  .  Check out the evaluated state.
!                  .
!           END IF
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      S.   Schlaifer  (JPL)
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!     [1] `Fundamentals of Celestial Mechanics', Second Edition
!         by J.M.A. Danby;  Willman-Bell, Inc., P.O. Box 35025
!         Richmond Virginia;  pp 345-347.
!
! $ Version
!
!-    TSPICE Version 1.2.0, 10-NOV-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VCRSS, VHAT, VROTV, and VSCL calls.
!
!-    SPICELIB Version 1.1.0, 20-OCT-1999 (WLT)
!
!        Declared PI to be an EXTERNAL Functions.
!
!-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
!
!-&

! $ Index_Entries
!
!     evaluate type_53 spk segment
!
!-&




!
!     SPICELIB Functions
!


!
!     Local Variables
!
      DOUBLE PRECISION      CHCKPA
      DOUBLE PRECISION      CHCKPV
      DOUBLE PRECISION      CHCKTP
      DOUBLE PRECISION      COSINE
      DOUBLE PRECISION      COSINC
      DOUBLE PRECISION      DMDT
      DOUBLE PRECISION      DNODE
      DOUBLE PRECISION      DPERI
      DOUBLE PRECISION      EANOM
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      MANOM
      DOUBLE PRECISION      MYPI
      DOUBLE PRECISION      NEAR
      DOUBLE PRECISION      OJ2
      DOUBLE PRECISION      P
      DOUBLE PRECISION      PA    (3)
      DOUBLE PRECISION      PV    (3)
      DOUBLE PRECISION      K2PI
      DOUBLE PRECISION      RPL
      DOUBLE PRECISION      SINE
      DOUBLE PRECISION      SMA
      DOUBLE PRECISION      TA
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TP    (3)
      DOUBLE PRECISION      TWOPIV
      DOUBLE PRECISION      TZERO
      DOUBLE PRECISION      U     (3)
      DOUBLE PRECISION      VTEMP (3)
      DOUBLE PRECISION      Z


      INTEGER               J2FLG
      LOGICAL               FIRST

      SAVE

      DATA                  FIRST / .TRUE. /


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF


      CALL CHKIN ( 'SPKE53')

      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         TWOPIV =  TWOPI()
         MYPI   =  PI()

      END IF


!
!     Fetch the various entities from the input record, first the epoch.
!
      EPOCH  = RECIN(1)


!
!     The trajectory pole vector.
!
      TP(1)  = RECIN(2)
      TP(2)  = RECIN(3)
      TP(3)  = RECIN(4)


!
!     The periapsis vector.
!
      PA(1)  = RECIN(5)
      PA(2)  = RECIN(6)
      PA(3)  = RECIN(7)


!
!     Semi-latus rectum ( P in the P/(1 + ECC*COS(Nu)  ),
!     eccentricity, and time from periapsis at epoch.
!
      P      = RECIN(8)
      ECC    = RECIN(9)
      TZERO  = RECIN(10)


!
!     J2 processing flag.
!
      J2FLG  = INT( RECIN(11) )


!
!     Central body pole vector.
!
      PV(1)  = RECIN(12)
      PV(2)  = RECIN(13)
      PV(3)  = RECIN(14)


!
!     The central mass, J2 and radius of the central body.
!
      GM     = RECIN(15)
      OJ2    = RECIN(16)
      RPL    = RECIN(17)


!
!     Check all the inputs here for obvious failures.  Yes, perhaps
!     this is overkill.  However, there is a lot more computation
!     going on in this routine so that the small amount of overhead
!     here should not be significant.
!
      CHCKPA = ABS(PA(1)) + ABS(PA(2)) + ABS(PA(3))
      CHCKPV = ABS(PV(1)) + ABS(PV(2)) + ABS(PV(3))
      CHCKTP = ABS(TP(1)) + ABS(TP(2)) + ABS(TP(3))


      IF ( P .EQ. 0 ) THEN


         CALL SETMSG ( 'The semi-latus rectum supplied to the ' &
         //            'SPK type 53 evaluator was zero.  This ' &
         //            'value must be non-zero. '  )


         CALL SIGERR ( 'SPICE(BADLATUSRECTUM)' )
         CALL CHKOUT ( 'SPKE53'                )
         RETURN


      ELSE IF ( ECC .EQ. 1.0D0 .OR. ECC .LT. 0.0D0 ) THEN


         CALL SETMSG ( 'The eccentricity supplied for a type 53 ' &
         //            'segment is out of the range of ' &
         //            'acceptable values ( 0 <= ecc < 1 or ecc >1). ' &
         //            'The value supplied ' &
         //            'to the type 53 evaluator was #. ' )


         CALL ERRDP  ( '#',   ECC                )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)'  )
         CALL CHKOUT ( 'SPKE53'                  )
         RETURN


      ELSE IF ( CHCKTP .EQ. 0.0D0 ) THEN


         CALL SETMSG ( 'The trajectory pole vector supplied to ' &
         //            'SPKE53 had length zero. The most likely ' &
         //            'cause of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE53'             )
         RETURN


      ELSE IF ( CHCKPA .EQ. 0.0D0 ) THEN


         CALL SETMSG ( 'The periapse vector supplied to SPKE53 ' &
         //            'had length zero. The most likely cause ' &
         //            'of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE53'             )
         RETURN


      ELSE IF ( CHCKPV .EQ. 0.0D0 ) THEN


         CALL SETMSG ( 'The central pole vector supplied to ' &
         //            'SPKE53 had length zero. The most likely ' &
         //            'cause of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE53'             )
         RETURN


      END IF


!
!     Convert TP, PV and PA to unit vectors.
!     (It won't hurt to polish them up a bit here if they are already
!      unit vectors.)
!
      CALL VHATIP ( PA )
      CALL VHATIP ( TP )
      CALL VHATIP ( PV )


!
!     Compute the semi-major axis, mean motion, and distance a periapse.
!
      SMA    = P/( 1.0D0 - ECC**2   )
      DMDT   = DSQRT(ABS(GM/SMA**3) )
      NEAR   = P/( 1.0D0 +ECC       )


!
!
!     Next compute the eccentric anomaly and from that, TA, the true
!     anomaly.
!
      MANOM = (TZERO + ET - EPOCH)*DMDT

      IF ( ECC .LT. 1.0D0 ) THEN
!
!        Next compute the angle THETA such that THETA is between
!        -pi and pi and such than MANOM = THETA + K*2*pi for
!        some integer K.
!
         THETA  = MOD(MANOM,TWOPIV)

         IF ( ABS(THETA).GT. MYPI ) THEN
            THETA = THETA - SIGN(TWOPIV,THETA)
         END IF

         K2PI  = MANOM - THETA
!
!        Compute the eccentric anomaly associated with THETA.
!
         CALL ELLTOF ( THETA, ECC, EANOM )

         SINE   = DSIN(EANOM/2) * DSQRT( (1+ECC)/(1-ECC) )
         COSINE = DCOS(EANOM/2)

!
!        Finally, compute the accumulated true anomaly.  That is,
!        add in the accumulated angle K2PI (This works because like
!        THETA, TA is always between -PI and PI.)
!
         TA     = 2.0D0*DATAN2(SINE,COSINE) + K2PI


      ELSE


         CALL HYPTOF ( MANOM, ECC, EANOM   )
         TA = 2.0D0*DATAN( DSQRT((ECC+1)/(ECC-1)) * DTANH(EANOM/2) )


      END IF


!
!     If called for, handle precession needed due to the J2 term.
!
      IF (      J2FLG .NE. 3 &
          .AND. OJ2   .NE. 0.0D0 &
          .AND. ECC   .LT. 1.0D0 &
          .AND. NEAR  .GT. RPL   ) THEN


!
!        Determine how far the line of nodes and periapsis have moved.
!
         COSINC = VDOT(PV,TP)


         Z      = TA * 1.5D0 * OJ2 *(RPL/P)**2
         DNODE  = -Z * COSINC
         DPERI  =  Z * ( 2.5D0*COSINC**2 - 0.5D0 )


!
!        Regress the line of nodes by rotating the periapsis and
!        trajectory pole vectors about the the pole of the central
!        body.
!
         IF ( J2FLG .NE. 2 ) THEN

            CALL VROTV ( TP,    PV, DNODE, VTEMP )
            CALL VEQU  ( VTEMP,            TP    )

            CALL VROTV ( PA,    PV, DNODE, VTEMP )
            CALL VEQU  ( VTEMP,            PA    )

         END IF


!
!        Precess periapsis by rotating the periapsis vector about the
!        trajectory pole
!
         IF ( J2FLG .NE. 1 ) THEN
            CALL VROTV ( PA, TP, DPERI, VTEMP )
            CALL VEQU  ( VTEMP,         PA    )
         END IF


      END IF


!
!     That's it finish the state computation.  Rotate the periapsis
!     vector by the true anomaly about the trajectory pole vector.
!     This gives a unit vector that points towards the position at ET.
!
      CALL VROTV ( PA, TP, TA, U )


!
!     Compute the range from the central body and scale up the
!     position vector to get the direction vector.
!
      Z  = P / ( 1.0D0 + ECC*DCOS(TA) )


      CALL VSCL( Z, U,   STATE(1)     )


!
!
!     Finally, the velocity. Recall that the velocity is given by
!     adding the position direction to an eccentricity vector,
!     rotating by 90 degrees in the plane of the orbit, and
!     scaling by the appropriate factor.  The "eccentricity
!     vector" is ECC times the the unit vector parallel to the
!     position at periapse.  Thus, up to scale, the velocity is:
!
!        TP x ( U + ECC * PA )
!
      CALL VLCOM ( 1.0D0, U, ECC, PA, STATE(4) )
      CALL VCRSS ( TP,    STATE(4),   VTEMP    )
      CALL VEQU  ( VTEMP,             STATE(4) )

!
!     Finally, scale up the direction of the velocity to
!     get the velocity.
!
      Z = DSQRT ( GM/P )


      CALL VSCLIP ( Z, STATE(4) )


      CALL CHKOUT('SPKE53')
      RETURN
      END
