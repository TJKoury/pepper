!$Procedure  ZZWIND2D ( Find winding number of polygon about point )

      INTEGER FUNCTION ZZWIND2D ( N, VERTCS, POINT )
      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Find the winding number of a planar polygon about a specified
!     point in 2-dimensional space.
!
! $ Required_Reading
!
!     PLANES
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!     PLANE
!
! $ Declarations

      INTEGER               N
      DOUBLE PRECISION      VERTCS ( 2, N )
      DOUBLE PRECISION      POINT  ( 2 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of vertices of polygon.
!     VERTCS     I   Vertices of polygon.
!     POINT      I   Point in PLANE.
!
!     The function returns the winding number of the input polygon
!     about the input point.
!
! $ Detailed_Input
!
!     N,
!     VERTCS         are, respectively, the number vertices defining
!                    the polygon and the vertices themselves.  Each
!                    pair of consecutive vectors in the array VERTCS
!                    defines an edge of the polygon.
!
! $ Detailed_Output
!
!     The function returns the winding number of the input polygon
!     about the input point. The winding number measures the "net"
!     number of times the polygon wraps around POINT:  this is
!     the number of times the polygon wraps around POINT in the
!     counterclockwise sense minus the number of times the polygon
!     wraps around POINT in the clockwise sense.
!
!     The possible values and meanings of the winding number are:
!
!        ZZWIND2D > 0:  The polygon winds about POINT a total
!                       of ZZWIND2D times in the counterclockwise
!                       direction.
!
!                       POINT is inside the polygon.
!
!
!        ZZWIND2D < 0:  The polygon winds about POINT a total
!                       of ZZWIND2D times in the clockwise
!                       direction.
!
!                       POINT is inside the polygon.
!
!
!        ZZWIND2D = 0:  The number of times the polygon wraps around
!                       POINT in the counterclockwise sense is equal
!                       to the number of times the polygon wraps around
!                       POINT in the clockwise sense.
!
!                       POINT is outside the polygon.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the number of boundary vectors N is not at least 3,
!         or if the number exceeds MAXFOV, the error
!         SPICE(INVALIDCOUNT) will be signaled.
!
!     2)  The input point and vertices are expected to lie in
!         the input plane.  To avoid problems introduced by
!         round-off errors, all of these vectors are projected
!         orthogonally onto the plane before the winding number
!         is computed.  If the input point or vertices are "far"
!         from the input plane, no error will be signaled.
!
!     3)  If the input plane as a zero normal vector, the error
!         SPICE(ZEROVECTOR) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Find the winding number of a 2-D polygon about a specified
!     point.
!
!     This routine supports determination of whether an ellipsoidal
!     body is in the field of view of a remote-sensing instrument
!     with a field of view having polygonal cross section.
!
!     The winding number is actually defined for closed, piecewise
!     differentiable curves in the complex plane. If z(t), t in
!     [0, 2*Pi], is a parameterization of such a curve, then if the
!     symbol I is used to represent the integration operator, z0 is the
!     complex point of interest, and w is the winding number, we have
!
!                1
!        w =  -------  *  I  ( d ( log(z-z0) ) )
!             2*Pi*i     z(t)
!
!
!                1
!          =  -------  *  I  ( ( 1 / (z-z0) ) dz )
!             2*Pi*i     z(t)
!
!
!     Because of Cauchy's theorem, we can transform the problem,
!     without loss of generality (leaving out *many* steps here), to
!     one for which the curve has the simple form
!
!                        i n*(t-t0)
!        z(t) =  z0 + r e
!
!     for some real values r, n, and t0.  So
!
!
!             1
!      w = -------  *  I  ( 1 / (z-z0) )
!          2*Pi*i     z(t)
!
!
!             1      t=2*pi        i n*(t-t0)           i n*(t-t0)
!        = ------- *   I   ( (1/r e         ) * ( r i n e          )dt )
!          2*Pi*i     t=0
!
!
!             1     t=2*pi
!        = ------- *  I (   i n dt )
!          2*Pi*i    t=0
!
!             1
!        = ------  *  ( 2 * Pi * i * n )
!          2*Pi*i
!
!
!        =    n
!
!
!     Given the simplified form of z(t) we've chosen, it's now clear
!     that n is the winding number.
!
!     In the simple case of a polygonal curve, the integral can be
!     computed for a corresponding polygon whose vertices have been
!     scaled to have equal magnitude; the integral can be expressed as
!     the telescoping sum
!
!         N
!        ___
!        \
!        /    ( argument of vertex(i+1) - argument of vertex(i) )
!        ---
!        i=1
!
!     where vertex N+1 is considered have length identical to that of
!     vertex 1 and argument differing from that of vertex 1 by w*2*pi.
!
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
!     [1] `Calculus and Analytic Geometry', Thomas and Finney.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 08-JUL-2008 (NJB)
!
!-&

! $ Index_Entries
!
!     find winding number of polygon about point
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOTG
      DOUBLE PRECISION      VSEPG

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      ATOTAL
      DOUBLE PRECISION      RNEXT  ( 2 )
      DOUBLE PRECISION      RPERP  ( 2 )
      DOUBLE PRECISION      RVEC   ( 2 )
      DOUBLE PRECISION      SEP

      INTEGER               I
      INTEGER               J

!
!     Initialize the function return value.
!
      ZZWIND2D = 0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZWIND2D' )

!
!     Check the number of sides of the polygon.
!
      IF ( N .LT. 3 ) THEN

         CALL SETMSG ( 'Polygon must have at least 3 sides; N = #.' )
         CALL ERRINT ( '#',  N                                      )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                      )
         CALL CHKOUT ( 'ZZWIND2D'                                     )
         RETURN

      END IF

!
!     The total "wrap angle" starts at zero.
!
      ATOTAL  = 0.D0

      CALL VSUBG ( VERTCS(1,1), POINT, 2, RVEC )

      DO I = 2, N+1

         IF ( I .LE. N ) THEN
            J = I
         ELSE
            J = 1
         END IF

!
!        Find the angular separation of RVEC and the next vector
!        RNEXT.
!
         CALL VSUBG ( VERTCS(1,J), POINT, 2, RNEXT )

         SEP  =  VSEPG ( RNEXT, RVEC, 2 )

!
!        Create a normal vector to RVEC by rotating RVEC pi/2 radians
!        counterclockwise.  We'll use this vector RPERP to determine
!        whether the next point is reached by clockwise or
!        counterclockwise rotation from RVEC.
!
         RPERP(1) = -RVEC(2)
         RPERP(2) =  RVEC(1)


         IF (  VDOTG( RNEXT, RPERP, 2 )  .GE.  0.D0  ) THEN
!
!           RNEXT is reached by counterclockwise rotation from
!           RVEC.  Note that in the case of zero rotation, the
!           sign doesn't matter because the contribution is zero.
!
            ATOTAL = ATOTAL + SEP
         ELSE
            ATOTAL = ATOTAL - SEP
         END IF

!
!        Update RVEC.
!
         CALL MOVED ( RNEXT, 2, RVEC )

      END DO

!
!     The above sum is 2 * pi * <the number of times the polygon
!     wraps around P>.  Let ZZWIND2D be the wrap count.
!
      ZZWIND2D = NINT ( ATOTAL / TWOPI() )

      CALL CHKOUT ( 'ZZWIND2D' )
      RETURN
      END
