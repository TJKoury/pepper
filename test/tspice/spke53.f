C$Procedure      SPKE53 ( Evaluate a type 53 SPK data record)
 
      SUBROUTINE SPKE53 ( ET, RECIN, STATE )
 
C$ Abstract
C
C     Evaluates a single SPK data record from a segment of type 53
C    (Precessing Conic Propagation).
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECIN  ( * )
      DOUBLE PRECISION      STATE  ( 6 )
 
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Target epoch.
C     RECIN      I   Data record.
C     STATE      O   State (position and velocity).
C
C$ Detailed_Input
C
C     ET          is a target epoch, specified as ephemeris seconds past
C                 J2000, at which a state vector is to be computed.
C
C     RECIN       is a data record which, when evaluated at epoch ET,
C                 will give the state (position and velocity) of some
C                 body, relative to some center, in some inertial
C                 reference frame.
C
C                 The structure of RECIN is:
C
C                 RECIN(1)             epoch of the orbit elements
C                                      in ephemeris seconds past J2000.
C                 RECIN(2)-RECIN(4)    unit trajectory pole vector
C                 RECIN(5)-RECIN(7)    unit periapsis vector
C                 RECIN(8)             semi-latus rectum---p in the
C                                      equation:
C
C                                      r = p/(1 + ECC*COS(Nu))
C
C                 RECIN(9)             eccentricity
C                 RECIN(10)            time past periapsis at epoch
C                                      in seconds
C                 RECIN(11)            J2 processing flag describing
C                                      what J2 corrections are to be
C                                      applied when the orbit is
C                                      propagated.
C
C                                      All J2 corrections are applied
C                                      if this flag has a value that
C                                      is not 1,2 or 3.
C
C                                      If the value of the flag is 3
C                                      no corrections are done.
C
C                                      If the value of the flag is 1
C                                      no corrections are computed for
C                                      the precession of the line
C                                      of apsides.  However, regression
C                                      of the line of nodes is
C                                      performed.
C
C                                      If the value of the flag is 2
C                                      no corrections are done for
C                                      the regression of the line of
C                                      nodes. However, precession of the
C                                      line of apsides is performed.
C
C                                      Note that J2 effects are computed
C                                      only if the orbit is elliptic and
C                                      does not intersect the central
C                                      body.
C
C                 RECIN(12)-RECIN(14)  unit central body pole vector
C                 RECIN(15)            central body GM
C                 RECIN(16)            central body J2
C                 RECIN(17)            central body radius
C
C                 Units are radians, km, seconds
C
C$ Detailed_Output
C
C     STATE       is the state produced by evaluating RECIN at ET.
C                 Units are km and km/sec.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     1) If the eccentricity is 1 or less than zero, the error
C        'SPICE(BADECCENTRICITY)' will be signalled.
C
C     2) If the semi-latus rectum is 0, the error
C
C        'SPICE(BADLATUSRECTUM)' is signalled.
C
C     3) If the pole vector, trajectory pole vector or periapsis vector
C        have zero length, the error 'SPICE(BADVECTOR)' is signalled.
C
C$ Particulars
C
C     This algorithm applies J2 corrections for precessing the
C     node and argument of periapse for an object orbiting an
C     oblate spheroid.
C
C     Note the effects of J2 are incorporated only for elliptic
C     orbits that do not intersect the central body.
C
C     While the derivation of the effect of the various harmonics
C     of gravitational field are beyond the scope of this header
C     the effect of the J2 term of the gravity model are as follows
C
C     The line of node precesses and the rate of precession DNode/dt
C     is given by
C                             3 n J2
C         dNode/dNu =  -  -----------------  DCOS( inc )
C                            2 (P/RPL)**2
C
C      where n is the mean motion of the orbit.
C
C     (Since this is always less than zero for oblate spheroids, this
C      should be called regression of nodes.)
C
C     The line of apsides precesses and the rate of precession DPeri/dt
C     is given by
C                              3 n J2
C         dPeri/dNu =     ----------------- ( 5*DCOS ( inc ) - 1 )
C                            2 (P/RPL)**2
C
C     Details of these formula are given in the Danby's book (see
C     literature references below).
C
C     It is assumed that this routine is used in conjunction with
C     the routine SPKR53 as shown here:
C
C        CALL SPKR53 ( HANDLE, DESCR, ET, RECIN         )
C        CALL SPKE53 (                ET, RECIN, STATE  )
C
C     where it is known in advance that the HANDLE, DESCR pair points
C     to a type 53 data segment.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRnn
C     routines might be used to examine raw segment data before
C     evaluating it with the SPKEnn routines.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 53 ) THEN
C
C              CALL SPKR53 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE53 ( ET, RECORD, STATE )
C                  .
C                  .  Check out the evaluated state.
C                  .
C           END IF
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      S.   Schlaifer  (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C     [1] `Fundamentals of Celestial Mechanics', Second Edition
C         by J.M.A. Danby;  Willman-Bell, Inc., P.O. Box 35025
C         Richmond Virginia;  pp 345-347.
C
C$ Version
C
C-    TSPICE Version 1.2.0, 10-NOV-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VCRSS, VHAT, VROTV, and VSCL calls.
C
C-    SPICELIB Version 1.1.0, 20-OCT-1999 (WLT)
C
C        Declared PI to be an EXTERNAL Functions.
C
C-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
C
C-&
 
C$ Index_Entries
C
C     evaluate type_53 spk segment
C
C-&
 
 
 
 
C
C     SPICELIB Functions
C
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOT
      LOGICAL               RETURN
 
 
C
C     Local Variables
C
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
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
 
      CALL CHKIN ( 'SPKE53')
 
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         TWOPIV =  TWOPI()
         MYPI   =  PI()
 
      END IF
 
 
C
C     Fetch the various entities from the input record, first the epoch.
C
      EPOCH  = RECIN(1)
 
 
C
C     The trajectory pole vector.
C
      TP(1)  = RECIN(2)
      TP(2)  = RECIN(3)
      TP(3)  = RECIN(4)
 
 
C
C     The periapsis vector.
C
      PA(1)  = RECIN(5)
      PA(2)  = RECIN(6)
      PA(3)  = RECIN(7)
 
 
C
C     Semi-latus rectum ( P in the P/(1 + ECC*COS(Nu)  ),
C     eccentricity, and time from periapsis at epoch.
C
      P      = RECIN(8)
      ECC    = RECIN(9)
      TZERO  = RECIN(10)
 
 
C
C     J2 processing flag.
C
      J2FLG  = INT( RECIN(11) )
 
 
C
C     Central body pole vector.
C
      PV(1)  = RECIN(12)
      PV(2)  = RECIN(13)
      PV(3)  = RECIN(14)
 
 
C
C     The central mass, J2 and radius of the central body.
C
      GM     = RECIN(15)
      OJ2    = RECIN(16)
      RPL    = RECIN(17)
 
 
C
C     Check all the inputs here for obvious failures.  Yes, perhaps
C     this is overkill.  However, there is a lot more computation
C     going on in this routine so that the small amount of overhead
C     here should not be significant.
C
      CHCKPA = ABS(PA(1)) + ABS(PA(2)) + ABS(PA(3))
      CHCKPV = ABS(PV(1)) + ABS(PV(2)) + ABS(PV(3))
      CHCKTP = ABS(TP(1)) + ABS(TP(2)) + ABS(TP(3))
 
 
      IF ( P .EQ. 0 ) THEN
 
 
         CALL SETMSG ( 'The semi-latus rectum supplied to the '
     .   //            'SPK type 53 evaluator was zero.  This '
     .   //            'value must be non-zero. '  )
 
 
         CALL SIGERR ( 'SPICE(BADLATUSRECTUM)' )
         CALL CHKOUT ( 'SPKE53'                )
         RETURN
 
 
      ELSE IF ( ECC .EQ. 1.0D0 .OR. ECC .LT. 0.0D0 ) THEN
 
 
         CALL SETMSG ( 'The eccentricity supplied for a type 53 '
     .   //            'segment is out of the range of '
     .   //            'acceptable values ( 0 <= ecc < 1 or ecc >1). '
     .   //            'The value supplied '
     .   //            'to the type 53 evaluator was #. ' )
 
 
         CALL ERRDP  ( '#',   ECC                )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)'  )
         CALL CHKOUT ( 'SPKE53'                  )
         RETURN
 
 
      ELSE IF ( CHCKTP .EQ. 0.0D0 ) THEN
 
 
         CALL SETMSG ( 'The trajectory pole vector supplied to '
     .   //            'SPKE53 had length zero. The most likely '
     .   //            'cause of this problem is a corrupted SPK '
     .   //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE53'             )
         RETURN
 
 
      ELSE IF ( CHCKPA .EQ. 0.0D0 ) THEN
 
 
         CALL SETMSG ( 'The periapse vector supplied to SPKE53 '
     .   //            'had length zero. The most likely cause '
     .   //            'of this problem is a corrupted SPK '
     .   //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE53'             )
         RETURN
 
 
      ELSE IF ( CHCKPV .EQ. 0.0D0 ) THEN
 
 
         CALL SETMSG ( 'The central pole vector supplied to '
     .   //            'SPKE53 had length zero. The most likely '
     .   //            'cause of this problem is a corrupted SPK '
     .   //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE53'             )
         RETURN
 
 
      END IF
 
 
C
C     Convert TP, PV and PA to unit vectors.
C     (It won't hurt to polish them up a bit here if they are already
C      unit vectors.)
C
      CALL VHATIP ( PA )
      CALL VHATIP ( TP )
      CALL VHATIP ( PV )
 
 
C
C     Compute the semi-major axis, mean motion, and distance a periapse.
C
      SMA    = P/( 1.0D0 - ECC**2   )
      DMDT   = DSQRT(ABS(GM/SMA**3) )
      NEAR   = P/( 1.0D0 +ECC       )
 
 
C
C
C     Next compute the eccentric anomaly and from that, TA, the true
C     anomaly.
C
      MANOM = (TZERO + ET - EPOCH)*DMDT
 
      IF ( ECC .LT. 1.0D0 ) THEN
C
C        Next compute the angle THETA such that THETA is between
C        -pi and pi and such than MANOM = THETA + K*2*pi for
C        some integer K.
C
         THETA  = MOD(MANOM,TWOPIV)
 
         IF ( ABS(THETA).GT. MYPI ) THEN
            THETA = THETA - SIGN(TWOPIV,THETA)
         END IF
 
         K2PI  = MANOM - THETA
C
C        Compute the eccentric anomaly associated with THETA.
C
         CALL ELLTOF ( THETA, ECC, EANOM )
 
         SINE   = DSIN(EANOM/2) * DSQRT( (1+ECC)/(1-ECC) )
         COSINE = DCOS(EANOM/2)
 
C
C        Finally, compute the accumulated true anomaly.  That is,
C        add in the accumulated angle K2PI (This works because like
C        THETA, TA is always between -PI and PI.)
C
         TA     = 2.0D0*DATAN2(SINE,COSINE) + K2PI
 
 
      ELSE
 
 
         CALL HYPTOF ( MANOM, ECC, EANOM   )
         TA = 2.0D0*DATAN( DSQRT((ECC+1)/(ECC-1)) * DTANH(EANOM/2) )
 
 
      END IF
 
 
C
C     If called for, handle precession needed due to the J2 term.
C
      IF (      J2FLG .NE. 3
     .    .AND. OJ2   .NE. 0.0D0
     .    .AND. ECC   .LT. 1.0D0
     .    .AND. NEAR  .GT. RPL   ) THEN
 
 
C
C        Determine how far the line of nodes and periapsis have moved.
C
         COSINC = VDOT(PV,TP)
 
 
         Z      = TA * 1.5D0 * OJ2 *(RPL/P)**2
         DNODE  = -Z * COSINC
         DPERI  =  Z * ( 2.5D0*COSINC**2 - 0.5D0 )
 
 
C
C        Regress the line of nodes by rotating the periapsis and
C        trajectory pole vectors about the the pole of the central
C        body.
C
         IF ( J2FLG .NE. 2 ) THEN

            CALL VROTV ( TP,    PV, DNODE, VTEMP )
            CALL VEQU  ( VTEMP,            TP    )

            CALL VROTV ( PA,    PV, DNODE, VTEMP ) 
            CALL VEQU  ( VTEMP,            PA    )

         END IF
 
 
C
C        Precess periapsis by rotating the periapsis vector about the
C        trajectory pole
C
         IF ( J2FLG .NE. 1 ) THEN
            CALL VROTV ( PA, TP, DPERI, VTEMP )
            CALL VEQU  ( VTEMP,         PA    )
         END IF
 
 
      END IF
 
 
C
C     That's it finish the state computation.  Rotate the periapsis
C     vector by the true anomaly about the trajectory pole vector.
C     This gives a unit vector that points towards the position at ET.
C
      CALL VROTV ( PA, TP, TA, U )
 
 
C
C     Compute the range from the central body and scale up the
C     position vector to get the direction vector.
C
      Z  = P / ( 1.0D0 + ECC*DCOS(TA) )
 
 
      CALL VSCL( Z, U,   STATE(1)     )
 
 
C
C
C     Finally, the velocity. Recall that the velocity is given by
C     adding the position direction to an eccentricity vector,
C     rotating by 90 degrees in the plane of the orbit, and
C     scaling by the appropriate factor.  The "eccentricity
C     vector" is ECC times the the unit vector parallel to the
C     position at periapse.  Thus, up to scale, the velocity is:
C
C        TP x ( U + ECC * PA )
C
      CALL VLCOM ( 1.0D0, U, ECC, PA, STATE(4) )
      CALL VCRSS ( TP,    STATE(4),   VTEMP    )
      CALL VEQU  ( VTEMP,             STATE(4) )
 
C
C     Finally, scale up the direction of the velocity to
C     get the velocity.
C
      Z = DSQRT ( GM/P )
 
 
      CALL VSCLIP ( Z, STATE(4) )
 
 
      CALL CHKOUT('SPKE53')
      RETURN
      END
