C$Procedure T_SURFPV ( Surface intercept position and velocity )

      SUBROUTINE T_SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND )

C$ Abstract
C
C     Find the state (position and velocity) of the surface intercept 
C     defined by a specified ray, ray velocity, and ellipsoid.
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
C     ROTATION
C     SPK
C     TIME
C
C$ Keywords
C
C     FRAME
C     MATRIX
C     ROTATION
C     STATE
C
C$ Declarations
 
      IMPLICIT NONE

      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D2 )

      DOUBLE PRECISION      STVRTX ( 6 )
      DOUBLE PRECISION      STDIR  ( 6 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      STX    ( 6 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MARGIN     P   Margin used for numeric bounds.
C     STVRTX     I   State of ray's vertex.
C     STDIR      I   State of ray's direction vector.
C     A          I   Length of ellipsoid semi-axis along the x-axis.
C     B          I   Length of ellipsoid semi-axis along the y-axis.
C     C          I   Length of ellipsoid semi-axis along the z-axis.
C     STX        O   State of surface intercept.
C     FOUND      O   Flag indicating whether intercept state was found.
C
C$ Detailed_Input
C
C     STVRTX         is the state of a ray's vertex. The first three
C                    components of STVRTX are the vertex's x, y, and z
C                    position components; the vertex's x, y, and z
C                    velocity components follow.
C
C                    The reference frame relative to which STVRTX is
C                    specified has axes aligned with with those of a
C                    triaxial ellipsoid. See the description below of
C                    the arguments A, B, and C.
C
C                    The vertex may be inside or outside of this
C                    ellipsoid, but not on it.
C
C                    No assumption is made about the units of length
C                    and time, but these units must be consistent with
C                    those of the other inputs.
C
C
C     STDIR          is the state of the input ray's direction vector.
C                    The first three components of STDIR are a non-zero
C                    vector giving the x, y, and z components of the
C                    ray's direction; the direction vector's x, y, and
C                    z velocity components follow.
C
C                    STDIR is specified relative to the same reference
C                    frame as is STVRTX.
C
C
C     A,
C     B,
C     C              are, respectively, the lengths of a triaxial
C                    ellipsoid's semi-axes lying along the x, y, and
C                    z axes of the reference frame relative to which
C                    STVRTX and STDIR are specified.
C                    
C$ Detailed_Output
C
C     STX            is the state of the intercept of the input ray on
C                    the surface of the input ellipsoid. The first
C                    three components of STX are the intercept's x, y,
C                    and z position components; the intercept's x, y,
C                    and z velocity components follow.
C
C                    STX is specified relative to the same reference
C                    frame as are STVRTX and STDIR.
C
C                    STX is defined if and only if both the intercept
C                    and its velocity are computable, as indicated by
C                    the output argument FOUND.
C
C
C     FOUND          is a logical flag indicating whether STX is
C                    defined. FOUND is .TRUE. if and only if both the
C                    intercept and its velocity are computable. Note
C                    that in some cases the intercept may computable
C                    while the velocity is not; this can happen for
C                    near-tangency cases.
C
C$ Parameters
C
C     MARGIN         is a parameter used to derive numeric
C                    bounds:
C
C                       - The minimum allowed semi-axis length
C                         is MARGIN / DPMAX()
C
C                       - The maximum magnitude of dX/dt,
C                         where X is image of the surface
C                         intercept after the ellipsoid and
C                         ray are transformed so that
C                         the ellipsoid maps to the unit
C                         sphere, is MARGIN / DPMAX().
C
C                       - The maximum magnitude of STX(4:6),
C                         the actual velocity of the surface
C                         intercept, is MARGIN / DPMAX().
C
C$ Exceptions
C
C     1) If the input ray's direction vector is zero, the error
C        SPICE(ZEROVECTOR) is signaled.
C
C     2) If any input axis length is less than MARGIN / DPMAX(),
C        the error SPICE(BADAXISLENGTH) is signaled.
C
C     3) If the vertex of the ray is on the ellipsoid,
C        the error SPICE(INVALIDVERTEX) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides an alternative, analytic ray-ellipsoid
C     surface intercept state computation algorithm meant to duplicate
C     the results of the algorithm used in SURFPV.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C   
C$ Version
C
C-    SPICELIB Version 1.0.0, 30-MAY-2008 (NJB)
C
C-&
 
C$ Index_Entries
C
C     state of surface intercept point on ellipsoid
C     surface intercept and velocity on ellipsoid
C
C-&



C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C
      INTEGER               MSGDIM
      PARAMETER           ( MSGDIM = 7 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 32 )

C
C     Local variables
C
      CHARACTER*(MSGLEN)    MSSG  ( MSGDIM )

      DOUBLE PRECISION      BOUND
      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      DS
      DOUBLE PRECISION      DU     ( 3 )
      DOUBLE PRECISION      DV     ( 3 )
      DOUBLE PRECISION      DVPAR  ( 3 )
      DOUBLE PRECISION      DVPERP ( 3 )
      DOUBLE PRECISION      DVPMAG
      DOUBLE PRECISION      DX     ( 3 )
      DOUBLE PRECISION      INVMAT ( 3, 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      MAXMAG
      DOUBLE PRECISION      MINRAD
      DOUBLE PRECISION      MINS
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SBOUND
      DOUBLE PRECISION      SCLMAT ( 3, 3 )
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      UPERPN ( 3 )
      DOUBLE PRECISION      UST    ( 6 )
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VDU
      DOUBLE PRECISION      VPAR   ( 3 )
      DOUBLE PRECISION      VPERPN ( 3 )
      DOUBLE PRECISION      VPMAG
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XSDIR  ( 6 )

      INTEGER               BAD

      LOGICAL               EXTER

C
C     Saved variables
C
      SAVE                  MSSG
 
C
C     Initial values
C
      DATA                  MSSG  / 'Axis A was nonpositive.',
     .                              'Axis B was nonpositive.',
     .                              'Axes A and B were nonpositive.',
     .                              'Axis C was nonpositive.',
     .                              'Axes A and C were nonpositive.',
     .                              'Axes B and C were nonpositive.',
     .                              'All three axes were nonpositive.' /
 

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SURFPV' )

C
C     No result has been found.
C
      FOUND = .FALSE.

C
C     Reject the direction vector if it's zero.
C
      IF ( VZERO(STDIR) ) THEN

         CALL SETMSG ( 'Ray''s direction vector was zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                 )
         CALL CHKOUT ( 'T_SURFPV'                          )
         RETURN

      END IF

C
C     Check the ellipsoid radii.
C
      MINRAD = MARGIN / DPMAX()

      BAD    = 0
 
      IF ( A .LE. MINRAD ) THEN
         BAD = BAD + 1
      END IF

      IF ( B .LE. MINRAD ) THEN
         BAD = BAD + 2
      END IF

      IF ( C .LE. MINRAD ) THEN
         BAD = BAD + 4
      END IF
 
      IF ( BAD .GT. 0 ) THEN

         CALL SETMSG ( MSSG (BAD) // ' ? '              )
         CALL ERRCH  ( '?', 'The A,B, and C axes were ' 
     .   //                 '#, #, and # respectively.' )
         CALL ERRDP  ( '#', A                           )
         CALL ERRDP  ( '#', B                           )
         CALL ERRDP  ( '#', C                           )
         CALL SIGERR ( 'SPICE(BADAXISLENGTH)'           )
         CALL CHKOUT ( 'T_SURFPV'                       )
         RETURN

      END IF
  
C
C     Form the transformation matrix that maps the ellipsoid
C     to the unit sphere.
C
      CALL CLEARD ( 9, SCLMAT )
      
      SCLMAT(1,1) = 1.D0 / A
      SCLMAT(2,2) = 1.D0 / B
      SCLMAT(3,3) = 1.D0 / C

C
C     Transform the input states using SCLMAT. Since SCLMAT is
C     a constant matrix, we can scale positions and velocities
C     individually.
C
      CALL MXV ( SCLMAT, STVRTX,    V  )
      CALL MXV ( SCLMAT, STVRTX(4), DV )

      CALL MXV ( SCLMAT, STDIR,     XSDIR     )
      CALL MXV ( SCLMAT, STDIR(4),  XSDIR(4)  )

C
C     Reject the vertex if it's on the ellipsoid. 
C     We check this by determining whether the transformed
C     vertex is on or in the unit sphere.
C
      LEVEL =  V(1)**2 + V(2)**2 + V(3)**2

      IF ( LEVEL .EQ. 1.D0 ) THEN

         CALL SETMSG ( 'Ray''s scaled vertex (# # #) has '
     .   //            'level surface parameter #. Vertex '
     .   //            'must not be on the ellipsoid.'     )
         CALL ERRDP  ( '#', V(1)                           )
         CALL ERRDP  ( '#', V(2)                           )
         CALL ERRDP  ( '#', V(3)                           )
         CALL ERRDP  ( '#', LEVEL                          )
         CALL SIGERR ( 'SPICE(INVALIDVERTEX)'              )
         CALL CHKOUT ( 'T_SURFPV'                          )
         RETURN

      END IF

C
C     The vertex may be either outside or inside the ellipsoid.
C     Classify the cases based on the level surface upon which
C     the vertex lies.
C
      EXTER = LEVEL .GT. 1.D0

C
C     Map the direction state to the state of a unit vector.
C
      CALL DVHAT ( XSDIR,  UST )

      CALL VEQU  ( UST,    U  )
      CALL VEQU  ( UST(4), DU )

C
C     Find the component of V parallel to the unit ray direction
C     vector. If U is the direction vector and V is the ray's vertex,
C     then the component we seek is
C
C        VPAR = < V, U > U                                           (1)
C
C     and the time derivative of VPAR is
C
C        d(VPAR)/dt =     ( < dV/dt, U > + < V, dU/dt > ) U  
C                      +  < V, U> * dU/dt                            (2)
C
C     Let VPERPN be the component of V perpendicular to U;
C     then
C
C        VPERPN = V - VPAR                                           (3)
C
C     and the time derivative DVPERP of VPERPN is
C
C        d(VPERPN)/dt = dV/dt - d(VPAR)/dt                           (4)
C
C     When the vertex is outside the unit sphere, if there is an
C     intercept X, the intercept is defined by
C
C        X = VPERPN  -  s * U                                        (5)
C
C     where 
C                            2   1/2
C        s = ( 1 - ||VPERPN||  )                                     (6)
C
C     and the velocity of X is
C
C        dX/dt = d(VPERPN)/dt  -  ds/dt * U  -  s * dU/dt            (7)
C
C     The time derivative DS of s is
C
C        ds/dt =  - (  ||VPERPN|| * d( ||VPERPN|| )/dt  ) / s        (8)
C
C     and the time derivative DVPMAG of ||VPERPN|| is
C
C        d( ||VPERPN|| )/dt = < d(VPERPN)/dt, VPERPN/||VPERPN|| >    (9)
C
C     If the vertex is inside the sphere, the sign of s is negated,
C     so for the interior vertex case we have
C
C        X     = VPERPN  +  s * U                                   (10)
C
C        dX/dt = d(VPERPN)/dt  +  ds/dt * U  +  s * dU/dt           (11)
C
C     For the interior case, we simply negate S and proceed as
C     in the exterior case.
C
C     We now compute the intermediate results required to 
C     compute the velocity of X.     
C
C     Start by computing the components of V parallel to and
C     perpendicular to U. We do this in-line because we'll re-use the
C     intermediate results. At this point we can check for the
C     exterior case where the ray points away from the target.
C
      VDU = VDOT ( V, U )

      IF ( EXTER ) THEN
         
         IF ( VDU .GE. 0.D0 ) THEN
C
C           The ray's vertex is outside the unit sphere and
C           the ray points into the half-space that doesn't
C           contain the sphere.
C
            CALL CHKOUT ( 'T_SURFPV' )
            RETURN

         END IF

      END IF

      CALL VSCL  ( VDU, U,    VPAR   )
      CALL VSUB  ( V,   VPAR, VPERPN )

C
C     Decide whether we have an intercept. We actually exclude
C     the tangency case as well, since the intercept is not
C     differentiable there.
C
      VPMAG = VNORM( VPERPN )

      IF ( VPMAG .GE. 1.D0 ) THEN
C
C        There's no intercept, or the intercept is a tangency point.
C
         CALL CHKOUT ( 'T_SURFPV' )
         RETURN

      END IF

C
C     Compute DVPERP and DVPMAG.
C
      CALL VLCOM ( VDOT(DV,U) + VDOT(V,DU),  U,  VDU,  DU,  DVPAR )
     
      CALL VSUB ( DV, DVPAR, DVPERP )

      CALL VHAT ( VPERPN, UPERPN )

      DVPMAG = VDOT( DVPERP, UPERPN )

C
C     We're still here, so compute the scale factor s. Use MIN for
C     safety here; we expect that VPMAG is less than 1.
C
      S = SQRT (  1.D0  -  MIN( 1.D0, VPMAG**2 )  )

C
C     For the interior case, S is negative.
C
      IF ( .NOT. EXTER ) THEN
         S = -S
      END IF

C
C     We're almost ready to compute ds/dt. However, this
C     computation, as well as the computation on the right
C     side of equation (7), can overflow. We must ensure that we
C     can compute dX/dt safely. Let DIFF be the maximum magnitude
C     that ds/dt can have, based on (7) and on our choice of
C     margin.
C
      DIFF = DPMAX()/MARGIN   -   VNORM(DVPERP)   -   ABS(S) * VNORM(DU)

C
C     If DIFF is not positive, we have a very, very large rate of
C     change of the pointing direction. Reject this case.
C
      IF (  DIFF  .LE. 0.D0  ) THEN

         CALL CHKOUT ( 'T_SURFPV' ) 
         RETURN

      END IF

C
C     Let BOUND be the upper bound we impose on the magnitude
C     of ds/dt.
C
      BOUND = MAX ( DIFF, 1.D0 )

C
C     Now apply (8). If DVPMAG is greater than 1, we use it
C     in computing the minimum allowable magnitude of s.
C
      IF ( DVPMAG .GT. 1.D0 ) THEN

         MINS = 0.5D0 * DVPMAG / BOUND 
      ELSE         
         MINS = 0.5D0 / BOUND
      END IF

C
C     Don't compute the derivative if s is too small.
C      
      IF (  TOUCHD( ABS(S) )  .LT.  TOUCHD( MINS )  ) THEN

         CALL CHKOUT ( 'T_SURFPV' )
         RETURN

      END IF

C
C     If we got this far, X and dX/dt are computable. Start
C     by applying (5) to compute X.
C
      CALL VLCOM ( 1.D0,  VPERPN,  -S,  U,  X )

C
C     Next compute ds/dt using 
C
C        ds/dt =  - (  ||VPERPN|| * d( ||VPERPN|| )/dt  ) / s        (8)
C
      DS =  - VPMAG * DVPMAG / S

C
C     Now compute dX/dt using (7), which we copy below.
C
C        dX/dt = d(VPERPN)/dt  -  ds/dt * U  -  s * dU/dt            (7)

      CALL VLCOM3 ( 1.D0, DVPERP,  -DS, U,  -S, DU,   DX )
      
C
C     Check that we can safely invert the scale transformation.
C
      MAXMAG = MAX ( ABS(A), ABS(B), ABS(C) )

      IF ( MAXMAG .GT. 1.D0 ) THEN

         SBOUND = DPMAX() / MARGIN / MAXMAG
      ELSE
         SBOUND = DPMAX() / MARGIN
      END IF

C
C     If either the intercept or the intercept velocity
C     have magnitude too large to allow scaling, return
C     here.
C
      IF (      ( VNORM(X)  .GT. SBOUND )
     .     .OR. ( VNORM(DX) .GT. SBOUND )  ) THEN

         CALL CHKOUT ( 'T_SURFPV' )
         RETURN
         
      END IF

C
C     Transform the intercept state back to the original
C     vector space.
C     
      CALL CLEARD ( 9, INVMAT )

      INVMAT(1,1) = A
      INVMAT(2,2) = B
      INVMAT(3,3) = C

      CALL MXV ( INVMAT, X,  STX    )
      CALL MXV ( INVMAT, DX, STX(4) )

      FOUND = .TRUE.

      CALL CHKOUT ( 'T_SURFPV' )
      RETURN
      END 


