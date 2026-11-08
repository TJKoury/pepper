!$Procedure   ZZEPRCSS   ( Earth precession, 1976 IAU model )

      subroutine zzeprcss ( et, precm )
      implicit none

! $ Abstract
!
!     Return the 1976 IAU Earth precession matrix for a specified time.
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
!     TRANSFORMATION
!
! $ Declarations

      double precision      et
      double precision      precm ( 3, 3 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris time, in seconds past J2000.
!     PRECM      O   Precession matrix at ET.
!
! $ Detailed_Input
!
!     ET             is the epoch at which the precession matrix is
!                    to be computed.  ET is barycentric dynamical time,
!                    expressed as seconds past J2000.
!
! $ Detailed_Output
!
!     PRECM          is a 3x3 matrix representing the precession of
!                    the Earth from J2000 to the epoch ET.  The
!                    rows of PRECM are the basis vectors for the Earth
!                    mean equator and equinox frame of date, evaluated
!                    at ET.
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
!     1) Convert a vector V from J2000 to Earth Mean equator and equinox
!        of date coordinates at epoch ET.  Call the resulting vector
!        VMOD.
!
!           CALL ZZEPRCSS ( ET,    PRECM       )
!           CALL MXV      ( PRECM, V,     VMOD )
!
! $ Restrictions
!
!     1)  This is a preliminary version of the routine.
!
!     2)  Though reference [1] does not specify limitations on the
!         range of valid time inputs for this precession model, the
!         fact that the rotation angles used in the model are defined
!         by polynomials implies that the model is not valid for all
!         time.
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
!-    Beta Version 1.0.0, 24-SEP-1996 (NJB)
!
!-&

! $ Index_Entries
!
!     Earth precession matrix based on 1976 IAU model
!
!-&


!
!     SPICELIB functions
!
      double precision      jyear
      double precision      rpd

!
!     Local parameters
!
      double precision      zeta1
      parameter           ( zeta1  = 2306.2181d0 )

      double precision      zeta2
      parameter           ( zeta2  = 0.30188d0   )

      double precision      zeta3
      parameter           ( zeta3  = 0.017998d0  )

      double precision      z1
      parameter           ( z1     = 2306.2181d0 )

      double precision      z2
      parameter           ( z2     = 1.09468d0   )

      double precision      z3
      parameter           ( z3     = 0.018203d0  )

      double precision      theta1
      parameter           ( theta1 = 2004.3109d0 )

      double precision      theta2
      parameter           ( theta2 = -0.42665d0 )

      double precision      theta3
      parameter           ( theta3 = -0.041833d0 )

!
!     Local variables
!
      double precision      scale
      double precision      t
      double precision      zeta
      double precision      z
      double precision      theta


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
      t      =  et    / ( jyear() * 100.d0 )
      scale  =  rpd() / 3600.d0

      zeta   =  t  * (  zeta1  + t*( zeta2  + t*zeta3  )  ) * scale
      z      =  t  * (  z1     + t*( z2     + t*z3     )  ) * scale
      theta  =  t  * (  theta1 + t*( theta2 + t*theta3 )  ) * scale

!
!     Now compute the precession matrix.
!
      call eul2m  ( -z, theta, -zeta, 3, 2, 3, precm )

      end
