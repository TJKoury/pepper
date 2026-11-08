!$Procedure   ZZEPRC76   ( Earth precession, 1976 IAU model )

      subroutine zzeprc76 ( et, precxf )
      use pepper_kinds, only: wp
      implicit none

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

      real(wp)              et
      real(wp)              precxf ( 6, 6 )

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
      real(wp)              jyear
      real(wp)              rpd

!
!     Local parameters
!
      real(wp), parameter :: zeta1 = 2306.2181_wp

      real(wp), parameter :: zeta2 = 0.30188_wp

      real(wp), parameter :: zeta3 = 0.017998_wp

      real(wp), parameter :: z1 = 2306.2181_wp

      real(wp), parameter :: z2 = 1.09468_wp

      real(wp), parameter :: z3 = 0.018203_wp

      real(wp), parameter :: theta1 = 2004.3109_wp

      real(wp), parameter :: theta2 = -0.42665_wp

      real(wp), parameter :: theta3 = -0.041833_wp

!
!     Local variables
!
      real(wp)              cent
      real(wp)              dtheta
      real(wp)              dz
      real(wp)              dzeta
      real(wp)              eulang ( 6 )
      real(wp)              scale
      real(wp)              t
      real(wp)              theta
      real(wp)              ts
      real(wp)              z
      real(wp)              zeta


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
      cent    =  jyear() * 100.0_wp
      t       =  et    / cent
      scale   =  rpd() / 3600.0_wp

      zeta    =  t  * (  zeta1  + t*(   zeta2  +   t*zeta3  )  ) * scale
      z       =  t  * (  z1     + t*(   z2     +   t*z3     )  ) * scale
      theta   =  t  * (  theta1 + t*(   theta2 +   t*theta3 )  ) * scale

      ts      =  1.0_wp / cent
      dzeta   =  ts * (  zeta1  + t*( 2*zeta2  + 3*t*zeta3  )  ) * scale
      dz      =  ts * (  z1     + t*( 2*z2     + 3*t*z3     )  ) * scale
      dtheta  =  ts * (  theta1 + t*( 2*theta2 + 3*t*theta3 )  ) * scale

!
!     Now compute the precession matrix.
!
      eulang(1)  =  -z
      eulang(2)  =   theta
      eulang(3)  =  -zeta
      eulang(4)  =  -dz
      eulang(5)  =   dtheta
      eulang(6)  =  -dzeta

      call eul2xf  ( eulang, 3, 2, 3, precxf )

      end
