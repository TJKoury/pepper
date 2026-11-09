!$Procedure      F_ZZHULLAX ( Test ZZHULLAX )

      SUBROUTINE F_ZZHULLAX ( OK )
      use pepper_inc_gf

! $ Abstract
!
!     Test the private FOV axis generation routine ZZHULLAX.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE


      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This family tests the private FOV axis generation
!     routine ZZHULLAX.
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
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 26-AUG-2008 (NJB)
!
!        Bug fix: real constants passed to EUL2M were changed to d.p.
!
!-    SPICELIB Version 1.1.0, 21-AUG-2008 (NJB)
!
!        Updated to reflect changes in SPICE errors signaled
!        as a result of the ZZHULLAX dot product sign bug
!        fix.
!
!-    SPICELIB Version 1.0.0, 31-JUL-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VSEP

      LOGICAL               EVEN
      LOGICAL               ODD

!
!     Test utility functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local parameters
!
      DOUBLE PRECISION      ANGLIM
      PARAMETER           ( ANGLIM = 1.D-12 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    QNAME

      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      BOUNDS ( 3, MAXVRT )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      E
      DOUBLE PRECISION      L
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SUM    ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      W
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      Y

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               SEED

!
!     Save everything.
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZHULLAX' )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Too few boundary vectors' )

      CALL CLEARD ( 3*MAXVRT, BOUNDS )

      CALL ZZHULLAX ( 'Test', 2, BOUNDS, AXIS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Linearly dependent consecutive boundary vectors' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, BOUNDS(1,1) )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, BOUNDS(1,2) )
      CALL VPACK ( 2.D0, 0.D0, 0.D0, BOUNDS(1,3) )

      CALL ZZHULLAX ( 'Test', 3, BOUNDS, AXIS )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'FOV region too close to half-space.' )

      E = 1.D-13
      N = 4

      CALL VPACK (  1.D0,    1.D0,   E, BOUNDS(1,1) )
      CALL VPACK ( -1.D0,    1.D0,   E, BOUNDS(1,2) )
      CALL VPACK ( -1.D0,   -1.D0,   E, BOUNDS(1,3) )
      CALL VPACK (  1.D0,   -1.D0,   E, BOUNDS(1,4) )

      CALL ZZHULLAX ( 'Test', 4, BOUNDS, AXIS )
      CALL CHCKXC ( .TRUE., 'SPICE(FACENOTFOUND)', OK )



!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Narrow square FOV' )

      E = 1.D-6
      N = 4

      CALL VPACK (  E,    E,   1.D0, BOUNDS(1,1) )
      CALL VPACK ( -E,    E,   1.D0, BOUNDS(1,2) )
      CALL VPACK ( -E,   -E,   1.D0, BOUNDS(1,3) )
      CALL VPACK (  E,   -E,   1.D0, BOUNDS(1,4) )

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Narrow square FOV, boundary order reversed' )

      E = 1.D-6
      N = 4

      CALL VPACK (  E,    E,   1.D0, BOUNDS(1,4) )
      CALL VPACK ( -E,    E,   1.D0, BOUNDS(1,3) )
      CALL VPACK ( -E,   -E,   1.D0, BOUNDS(1,2) )
      CALL VPACK (  E,   -E,   1.D0, BOUNDS(1,1) )

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Long, narrow rectangular FOV' )

      W = 1.D-6
      L = 1.D0 - W

      N = 4

      CALL VPACK (  L,    E,   1.D0, BOUNDS(1,1) )
      CALL VPACK ( -L,    E,   1.D0, BOUNDS(1,2) )
      CALL VPACK ( -L,   -E,   1.D0, BOUNDS(1,3) )
      CALL VPACK (  L,   -E,   1.D0, BOUNDS(1,4) )

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Pentagonal FOV consisting of "dented" rectangle' )

      E = 1.D-1
      N = 5

      CALL VPACK (  E,    E,     1.D0, BOUNDS(1,1) )
      CALL VPACK (  0.D0, E/2,   1.D0, BOUNDS(1,2) )
      CALL VPACK ( -E,    E,     1.D0, BOUNDS(1,3) )
      CALL VPACK ( -E,   -E,     1.D0, BOUNDS(1,4) )
      CALL VPACK (  E,   -E,     1.D0, BOUNDS(1,5) )

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( '5-pointed star' )

      E = 1.D-1
      N = 10

      DELTA = TWOPI()/N

      DO I = 1, N

         IF ( ODD(I) ) THEN
!
!           This boundary vector corresponds to a point
!           of the star.
!
            S = E
         ELSE
            S = E / 3
         END IF

         THETA = (I-1)*DELTA

         CALL VPACK ( S*COS(THETA), S*SIN(THETA), 1.D0, BOUNDS(1,I) )

      END DO

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( '5-pointed star, reversed boundary order' )

      E = 1.D-1
      N = 10

      DELTA = TWOPI()/N

      DO I = 1, N

         IF ( ODD(I) ) THEN
!
!           This boundary vector corresponds to a point
!           of the star.
!
            S = E
         ELSE
            S = E / 3
         END IF

         THETA = (I-1)*DELTA

         J = N + 1 - I

         CALL VPACK ( S*COS(THETA), S*SIN(THETA), 1.D0, BOUNDS(1,J) )

      END DO

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( '50-gon star with 5 points' )

      E = 1.D-1
      N = 50

      DELTA = TWOPI()/N

      DO I = 1, N

         IF ( MOD(I,10) .EQ. 0 ) THEN
!
!           This boundary vector corresponds to a point
!           of the star.
!
            S = E
         ELSE
            S = E / 3
         END IF

         THETA = (I-1)*DELTA

         CALL VPACK ( S*COS(THETA), S*SIN(THETA), 1.D0, BOUNDS(1,I) )

      END DO

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( '50-gon star with 5 points, wide FOV' )

      E = 1.5D0
      N = 50

      DELTA = TWOPI()/N

      DO I = 1, N

         IF ( MOD(I,10) .EQ. 0 ) THEN
!
!           This boundary vector corresponds to a point
!           of the star.
!
            S = E
         ELSE
            S = E / 3
         END IF

         THETA = (I-1)*DELTA

         CALL VPACK ( S*COS(THETA), S*SIN(THETA), 1.D0, BOUNDS(1,I) )

      END DO

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Random vectors, constrained within rectangle' )

      E    = 1.D0
      N    = 1000

      SEED = -1
!
!     Burn the first two values.
!
      X    = T_RANDD ( -2*E, 2*E, SEED )
      Y    = T_RANDD (   -E,   E, SEED )

      DO I = 1, N

         X = T_RANDD ( -2*E, 2*E, SEED )
         Y = T_RANDD (   -E,   E, SEED )

         CALL VPACK ( X, Y, 1.D0, BOUNDS(1,I) )

      END DO

      CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Validate the axis.
!
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO




!
!---- Case -------------------------------------------------------------
!
!
!     Test some regular polygons. Let the number of
!     sides range from 3 to 100. Let the angular extent
!     of the FOVs increase with the number of sides.
!
!
!     We'll use a transformation to a frame that's not lined
!     up with the original one.
!
      CALL EUL2M ( 0.6D0, -0.4D0, 1.2D0, 1, 2, 3, XFORM )


      DO N = 3, 100

         E = 1.D-2 * N

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Regular polygon: N = #; E = #'
         CALL REPMI ( TITLE, '#', N,    TITLE )
         CALL REPMD ( TITLE, '#', E, 6, TITLE )

         CALL TCASE ( TITLE )

         DELTA = TWOPI()/N

         DO I = 1, N

            THETA = (I-1)*DELTA

            CALL VPACK ( E*COS(THETA), E*SIN(THETA), 1.D0, V )

            CALL MXV ( XFORM, V, BOUNDS(1,I) )

         END DO

         CALL ZZHULLAX ( 'Test', N, BOUNDS, AXIS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Validate the axis.
!
         DO I = 1, N

            SEP = VSEP( BOUNDS(1,I), AXIS )

            QNAME = 'Boundary # sep'
            CALL REPMI ( QNAME, '#', I, QNAME )

            LIMIT = HALFPI()-ANGLIM

            CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

         END DO

!
!        For polygons with even numbers of edges, the axis
!        should be very close to the average of the boundary
!        vectors.
!
         IF ( EVEN(N) ) THEN

            CALL CLEARD ( 3, SUM )

            DO I = 1, N
               CALL VADD  ( SUM, BOUNDS(1,I), V   )
               CALL VEQU  ( V,                SUM )
            END DO

            CALL VSCL ( 1.D0/N, SUM, CENTER )

            CALL CHCKAD ( 'AXIS', AXIS, '~', CENTER, 3, TIGHT, OK )

         END IF

      END DO


      CALL T_SUCCESS ( OK )
      RETURN
      END




