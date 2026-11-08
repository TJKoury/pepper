!$Procedure   ZZEPRC76   ( Earth precession, 1976 IAU model )

      SUBROUTINE ZZEPRC76 ( ET, PRECXF )
      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute the state transformation matrix implementing the IAU 1976
!     precession model.
!
! $ Required_Reading
!
!     ROTATION
!
! $ Keywords
!
!     FRAMES
!     GEOMETRY
!     MATRIX
!     PRIVATE
!     TRANSFORMATION
!
! $ Declarations

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      PRECXF ( 6, 6 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris time, in seconds past J2000 TDB.
!     PRECXF     O   Precession state transformation matrix at ET.
!
! $ Detailed_Input
!
!     ET             is the epoch at which the precession matrix is
!                    to be computed.  ET is barycentric dynamical time,
!                    expressed as seconds past J2000.
!
! $ Detailed_Output
!
!     PRECXF         is a 6x6 matrix that transforms states from the
!                    J2000 frame to the mean equator and equinox frame
!                    of the earth at the epoch ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     According to reference [2], the precession model used in this
!     routine is that used in the JPL navigation program "Regres."
!
!     The precession matrix is defined using the Euler angles
!
!        zeta ,   z ,  and theta
!            A     A            A
!
!
!     Equation (5-147) of [2] gives the matrix determined by these
!     angles as
!
!        A  =  [ -z   ]   [ theta  ]   [ -zeta  ]
!                  A   3         A  2         A  3
!
!
!     Formulas for the Euler angles are from [2], equation
!     (5-143):
!                                              2                3
!         zeta   =  2306".2181*T  +  0".30188*T   +  0".017998*T
!             A
!
!
!                                              2                3
!         z      =  2306".2181*T  +  1".09468*T   +  0".018203*T
!          A
!
!
!                                              2                3
!         theta  =  2004".3109*T  -  0".42665*T   -  0".041833*T
!              A
!
! $ Examples
!
!     1) Convert a state vector S from J2000 to Earth Mean equator and
!        equinox of date coordinates at epoch ET.  Call the resulting
!        vector SMOD.
!
!           CALL ZZEPRC76 ( ET,     PRECXF        )
!           CALL MXVG     ( PRECXF, S, 6, 6, SMOD )
!
! $ Restrictions
!
!     1) This is a SPICE private routine; the routine is subject to
!        change without notice.  User applications should not call this
!        routine.
!
!     2) Though reference [1] does not specify limitations on the range
!        of valid time inputs for this precession model, the fact that
!        the rotation angles used in the model are defined by
!        polynomials implies that the model is not valid for all time.
!
! $ Literature_References
!
!     [1] "Explanatory Supplement to the Astronomical Almanac"
!          edited by P. Kenneth Seidelmann. University Science
!          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
!
!     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
!         Acceleration Vectors of Tracking Station" by T. D. Moyer.
!         Draft of JPL Publication documenting the JPL navigation
!         program "Regres."
!
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 18-APR-2016 (NJB)
!
!        Corrected typo in header abstract: changed "1876"
!        to "1976."
!
!-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
!
!-&

! $ Index_Entries
!
!     IAU 1976 earth precession transformation
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD

!
!     Local parameters
!
      DOUBLE PRECISION      ZETA1
      PARAMETER           ( ZETA1  = 2306.2181D0 )

      DOUBLE PRECISION      ZETA2
      PARAMETER           ( ZETA2  = 0.30188D0   )

      DOUBLE PRECISION      ZETA3
      PARAMETER           ( ZETA3  = 0.017998D0  )

      DOUBLE PRECISION      Z1
      PARAMETER           ( Z1     = 2306.2181D0 )

      DOUBLE PRECISION      Z2
      PARAMETER           ( Z2     = 1.09468D0   )

      DOUBLE PRECISION      Z3
      PARAMETER           ( Z3     = 0.018203D0  )

      DOUBLE PRECISION      THETA1
      PARAMETER           ( THETA1 = 2004.3109D0 )

      DOUBLE PRECISION      THETA2
      PARAMETER           ( THETA2 = -0.42665D0 )

      DOUBLE PRECISION      THETA3
      PARAMETER           ( THETA3 = -0.041833D0 )

!
!     Local variables
!
      DOUBLE PRECISION      CENT
      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      DZ
      DOUBLE PRECISION      DZETA
      DOUBLE PRECISION      EULANG ( 6 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      T
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TS
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      ZETA


!
!     No check-in required; this routine does not participate in
!     SPICELIB error handling.
!
!
!     Compute the precession angles first.  The time argument has
!     units of Julian centuries.  The polynomial expressions yield
!     angles in units of arcseconds prior to scaling.  After scaling,
!     the angles are in units of radians.
!
      CENT    =  JYEAR() * 100.D0
      T       =  ET    / CENT
      SCALE   =  RPD() / 3600.D0

      ZETA    =  T  * (  ZETA1  + T*(   ZETA2  +   T*ZETA3  )  ) * SCALE
      Z       =  T  * (  Z1     + T*(   Z2     +   T*Z3     )  ) * SCALE
      THETA   =  T  * (  THETA1 + T*(   THETA2 +   T*THETA3 )  ) * SCALE

      TS      =  1.D0 / CENT
      DZETA   =  TS * (  ZETA1  + T*( 2*ZETA2  + 3*T*ZETA3  )  ) * SCALE
      DZ      =  TS * (  Z1     + T*( 2*Z2     + 3*T*Z3     )  ) * SCALE
      DTHETA  =  TS * (  THETA1 + T*( 2*THETA2 + 3*T*THETA3 )  ) * SCALE

!
!     Now compute the precession matrix.
!
      EULANG(1)  =  -Z
      EULANG(2)  =   THETA
      EULANG(3)  =  -ZETA
      EULANG(4)  =  -DZ
      EULANG(5)  =   DTHETA
      EULANG(6)  =  -DZETA

      CALL EUL2XF  ( EULANG, 3, 2, 3, PRECXF )

      END
