!$Procedure  ZZWIND ( Find winding number of polygon about point )

      INTEGER FUNCTION ZZWIND ( PLANE, N, VERTCS, POINT )
      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Find the winding number of a planar polygon, embedded in
!     3-dimensional space, about a specified point.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      DOUBLE PRECISION      PLANE  ( UBPL )
      INTEGER               N
      DOUBLE PRECISION      VERTCS ( 3, * )
      DOUBLE PRECISION      POINT  ( 3 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     PLANE      I   A SPICELIB plane.
!     N          I   Number of vertices of polygon.
!     VERTCS     I   Vertices of polygon.
!     POINT      I   Point in PLANE.
!     UBPL       P   Upper bound of SPICELIB plane array.
!
!     The function returns the winding number of the input polygon
!     about the input point.
!
! $ Detailed_Input
!
!     PLANE          is a SPICELIB plane containing a polygon and
!                    a point.
!
!     N,
!     VERTCS         are, respectively, the number vertices defining
!                    the polygon and the vertices themselves.  Each
!                    pair of consecutive vectors in the array VERTCS
!                    defines an edge of the polygon.
!
!     POINT          is a point lying in PLANE; the winding number
!                    of the polygon about POINT is sought.
!
! $ Detailed_Output
!
!     The function returns the winding number of the input polygon
!     about the input point.  The winding number measures the "net"
!     number of times the polygon wraps around POINT:  this is
!     the number of times the polygon wraps around POINT in the
!     counterclockwise sense minus the number of times the polygon
!     wraps around POINT in the clockwise sense.
!
!     The possible values and meanings of the winding number are:
!
!        ZZWIND > 0:    The polygon winds about POINT a total
!                       of ZZWIND times in the counterclockwise
!                       direction.
!
!                       POINT is inside the polygon.
!
!
!        ZZWIND < 0:    The polygon winds about POINT a total
!                       of ZZWIND times in the clockwise
!                       direction.
!
!                       POINT is inside the polygon.
!
!
!        ZZWIND = 0:    The number of times the polygon wraps around
!                       POINT in the counterclockwise sense is equal
!                       to the number of times the polygon wraps around
!                       POINT in the clockwise sense.
!
!                       POINT is outside the polygon.
!
! $ Parameters
!
!     UBPL           is the array upper bound for SPICELIB planes.
!
! $ Exceptions
!
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
!     Find the winding number of a 3-D polygon about a specified
!     point.  Although in principle the polygon is two-dimensional,
!     it is embedded in 3-space.
!
!     This routine supports determination of whether an ellipsoidal
!     body is in the field of view of a remote-sensing instrument
!     with a field of view having polygonal cross section.
!
!     The winding number is actually defined for closed, piecewise
!     differentiable curves in the complex plane.  If z(t), t in
!     [0, 2*Pi], is a parameterization of such a curve, then if the
!     symbol I is used to represent the integration operator, z0 is the
!     complex point of interest, and w is the winding number, we have
!
!        w = ( argument of z(2*pi) - argument of z(0) ) / (2*pi)
!
!          = (1/i) * ( log( z(2*pi)-z0 ) - log( z(0)-z0 ) ) / (2*pi)
!
!     Note the above is true because the curve is closed, so the real
!     parts of the logarithms cancel.  Then
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
!     In the simple case of a polygonal curve, the integral can
!     be computed from the original definition of the winding number:
!
!        w = ( argument of z(2*pi) - argument of z(0) ) / (2*pi)
!
!     The difference of arguments
!
!        argument of z(2*pi) - argument of z(0)
!
!     can be expressed as the telescoping sum
!
!         N
!        ___
!        \
!        /    ( argument of vertex(i+1) - argument of vertex(i) )
!        ---
!        i=1
!
!     where vertex N+1 is considered identical to vertex 1.
!
!
! $ Examples
!
!     See usage in ZZELVUPY.
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
!-    SPICELIB Version 1.0.0, 11-AUG-2005 (NJB)
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
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VSEP

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local variables
!
      DOUBLE PRECISION      ATOTAL
      DOUBLE PRECISION      CONS
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      RNEXT  ( 3 )
      DOUBLE PRECISION      RPERP  ( 3 )
      DOUBLE PRECISION      RVEC   ( 3 )
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      VTEMP  ( 3 )

      INTEGER               I
      INTEGER               J

!
!     Initialize the function return value.
!
      ZZWIND = 0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZWIND' )

!
!     Check the number of sides of the polygon.
!
      IF ( N .LT. 3 ) THEN

         CALL SETMSG ( 'Polygon must have at least 3 sides; N = #.' )
         CALL ERRINT ( '#',  N                                      )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                      )
         CALL CHKOUT ( 'ZZWIND'                                     )
         RETURN

      END IF

!
!     Unpack the plane's normal and constant.
!
      CALL PL2NVC ( PLANE, NORMAL, CONS )

!
!     Check the normal vector.
!
      IF ( VZERO(NORMAL) ) THEN

         CALL SETMSG ( 'Plane''s normal vector is zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'               )
         CALL CHKOUT ( 'ZZWIND'                          )
         RETURN

      END IF

!
!     We want the normal vector to point on the same side of the
!     plane as the boundary vectors.  Negate the normal
!     if necessary to make this true.  We don't touch CONS because
!     it's not used later, but in principle it should be negated.
!
      IF (  VDOT( NORMAL, VERTCS(1,1) )  .LT.  0.D0  ) THEN

         CALL VMINUS ( NORMAL, VTEMP  )
         CALL VEQU   ( VTEMP,  NORMAL )

      END IF

!
!     Find the angular argument of each point; find the difference
!     of this angle from the preceding angle; add the difference to
!     the total.
!
      CALL VSUB  ( VERTCS(1,1), POINT, VTEMP )

!
!     Get the component RVEC of the difference vector orthogonal to
!     the plane's normal vector.
!
      CALL VPERP ( VTEMP, NORMAL, RVEC )

!
!     The total "wrap angle" starts at zero.
!
      ATOTAL  = 0.D0

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
         CALL VSUB  ( VERTCS(1,J), POINT,   VTEMP )
         CALL VPERP ( VTEMP,       NORMAL,  RNEXT )

         SEP  =  VSEP ( RNEXT, RVEC )

!
!        Create a normal vector to RVEC by rotating RVEC pi/2 radians
!        counterclockwise.  We'll use this vector RPERP to determine
!        whether the next point is reached by clockwise or
!        counterclockwise rotation from RVEC.
!
         CALL UCRSS ( NORMAL, RVEC, RPERP )

         IF (  VDOT( RNEXT, RPERP )  .GE.  0.D0  ) THEN
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
         CALL VEQU ( RNEXT, RVEC )

      END DO

!
!     The above sum is 2 * pi * <the number of times the polygon
!     wraps around P>.  Let ZZWIND be the wrap count.
!
      ZZWIND = NINT ( ATOTAL / TWOPI() )

      CALL CHKOUT ( 'ZZWIND' )
      RETURN
      END
