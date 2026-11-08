C$Procedure      F_ZZFOVAXI ( Test ZZFOVAXI )
  
      SUBROUTINE F_ZZFOVAXI ( OK )

C$ Abstract
C
C     Test the private FOV axis generation routine ZZFOVAXI.
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
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
 
      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This family tests the private FOV axis generation
C     routine ZZFOVAXI.
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
C-    SPICELIB Version 1.0.0, 21-AUG-2008 (NJB)
C
C        Updated to reflect changes in SPICE errors signaled
C        as a result of the ZZHULLAX dot product sign bug
C        fix.
C
C-    SPICELIB Version 1.0.0, 31-JUL-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VSEP

      LOGICAL               EVEN
      LOGICAL               ODD

C
C     Test utility functions
C
      DOUBLE PRECISION      T_RANDD

C     
C     Local parameters
C
      DOUBLE PRECISION      ANGLIM
      PARAMETER           ( ANGLIM = 1.D-12 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local Variables
C
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
      DOUBLE PRECISION      UVEC   ( 3 )
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      W
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      Y

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               SEED

C
C     Save everything.
C
      SAVE
 
C
C     Initial values
C


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZFOVAXI' )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Too few boundary vectors' )

      CALL CLEARD ( 3*MAXVRT, BOUNDS )

      CALL ZZFOVAXI ( 'Test', 2, BOUNDS, AXIS )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'FOV region too close to half-space.' )

      E = 1.D-13
      N = 4

      CALL VPACK (  1.D0,    1.D0,   E, BOUNDS(1,1) )
      CALL VPACK ( -1.D0,    1.D0,   E, BOUNDS(1,2) )
      CALL VPACK ( -1.D0,   -1.D0,   E, BOUNDS(1,3) )
      CALL VPACK (  1.D0,   -1.D0,   E, BOUNDS(1,4) )

      CALL ZZFOVAXI ( 'Test', 4, BOUNDS, AXIS )
      CALL CHCKXC ( .TRUE., 'SPICE(FACENOTFOUND)', OK )



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Narrow square FOV' )

      E = 1.D-6
      N = 4

      CALL VPACK (  E,    E,   1.D0, BOUNDS(1,1) )
      CALL VPACK ( -E,    E,   1.D0, BOUNDS(1,2) )
      CALL VPACK ( -E,   -E,   1.D0, BOUNDS(1,3) )
      CALL VPACK (  E,   -E,   1.D0, BOUNDS(1,4) )

      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Narrow square FOV, boundary order reversed' )

      E = 1.D-6
      N = 4

      CALL VPACK (  E,    E,   1.D0, BOUNDS(1,4) )
      CALL VPACK ( -E,    E,   1.D0, BOUNDS(1,3) )
      CALL VPACK ( -E,   -E,   1.D0, BOUNDS(1,2) )
      CALL VPACK (  E,   -E,   1.D0, BOUNDS(1,1) )

      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Long, narrow rectangular FOV' )

      W = 1.D-6
      L = 1.D0 - W

      N = 4

      CALL VPACK (  L,    E,   1.D0, BOUNDS(1,1) )
      CALL VPACK ( -L,    E,   1.D0, BOUNDS(1,2) )
      CALL VPACK ( -L,   -E,   1.D0, BOUNDS(1,3) )
      CALL VPACK (  L,   -E,   1.D0, BOUNDS(1,4) )

      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Pentagonal FOV consisting of "dented" rectangle' )

      E = 1.D-1
      N = 5

      CALL VPACK (  E,    E,     1.D0, BOUNDS(1,1) )
      CALL VPACK (  0.D0, E/2,   1.D0, BOUNDS(1,2) )
      CALL VPACK ( -E,    E,     1.D0, BOUNDS(1,3) )
      CALL VPACK ( -E,   -E,     1.D0, BOUNDS(1,4) )
      CALL VPACK (  E,   -E,     1.D0, BOUNDS(1,5) )

      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


 
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( '5-pointed star' )

      E = 1.D-1
      N = 10

      DELTA = TWOPI()/N

      DO I = 1, N

         IF ( ODD(I) ) THEN
C
C           This boundary vector corresponds to a point
C           of the star.
C
            S = E
         ELSE
            S = E / 3
         END IF

         THETA = (I-1)*DELTA

         CALL VPACK ( S*COS(THETA), S*SIN(THETA), 1.D0, BOUNDS(1,I) )

      END DO
 
      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( '5-pointed star, reversed boundary order' )

      E = 1.D-1
      N = 10

      DELTA = TWOPI()/N

      DO I = 1, N

         IF ( ODD(I) ) THEN
C
C           This boundary vector corresponds to a point
C           of the star.
C
            S = E
         ELSE
            S = E / 3
         END IF

         THETA = (I-1)*DELTA

         J = N + 1 - I

         CALL VPACK ( S*COS(THETA), S*SIN(THETA), 1.D0, BOUNDS(1,J) )

      END DO
 
      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO

 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( '50-gon star with 5 points' )

      E = 1.D-1
      N = 50

      DELTA = TWOPI()/N

      DO I = 1, N

         IF ( MOD(I,10) .EQ. 0 ) THEN
C
C           This boundary vector corresponds to a point
C           of the star.
C
            S = E
         ELSE
            S = E / 3
         END IF

         THETA = (I-1)*DELTA

         CALL VPACK ( S*COS(THETA), S*SIN(THETA), 1.D0, BOUNDS(1,I) )

      END DO
 
      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO

 
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( '50-gon star with 5 points, wide FOV' )

      E = 1.5D0
      N = 50

      DELTA = TWOPI()/N

      DO I = 1, N

         IF ( MOD(I,10) .EQ. 0 ) THEN
C
C           This boundary vector corresponds to a point
C           of the star.
C
            S = E
         ELSE
            S = E / 3
         END IF

         THETA = (I-1)*DELTA

         CALL VPACK ( S*COS(THETA), S*SIN(THETA), 1.D0, BOUNDS(1,I) )

      END DO
 
      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO

 
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Random vectors, constrained within rectangle' )

      E    = 1.D0
      N    = 1000
      
      SEED = -1
C
C     Burn the first two values.
C     
      X    = T_RANDD ( -2*E, 2*E, SEED )
      Y    = T_RANDD (   -E,   E, SEED )

      DO I = 1, N

         X = T_RANDD ( -2*E, 2*E, SEED )
         Y = T_RANDD (   -E,   E, SEED )

         CALL VPACK ( X, Y, 1.D0, BOUNDS(1,I) )

      END DO
 
      CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Validate the axis.
C
      DO I = 1, N

         SEP = VSEP( BOUNDS(1,I), AXIS )

         QNAME = 'Boundary # sep'
         CALL REPMI ( QNAME, '#', I, QNAME )

         LIMIT = HALFPI()-ANGLIM

         CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

      END DO




C
C---- Case -------------------------------------------------------------
C
C
C     Test some regular polygons. Let the number of
C     sides range from 3 to 100. Let the angular extent 
C     of the FOVs increase with the number of sides.
C     
C
C     We'll use a transformation to a frame that's not lined
C     up with the original one.
C
      CALL EUL2M ( 0.6D0, -0.4D0, 1.2D0, 1, 2, 3, XFORM )


      DO N = 3, 100

         E = 1.D-2 * N

C
C---- Case -------------------------------------------------------------
C
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

         CALL ZZFOVAXI ( 'Test', N, BOUNDS, AXIS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Validate the axis.
C
         DO I = 1, N

            SEP = VSEP( BOUNDS(1,I), AXIS )

            QNAME = 'Boundary # sep'
            CALL REPMI ( QNAME, '#', I, QNAME )

            LIMIT = HALFPI()-ANGLIM

            CALL CHCKSD ( QNAME, SEP, '<', LIMIT, 0.D0, OK )

         END DO

C
C        For polygons with even numbers of edges, the axis
C        should be very close to the average of the unit boundary
C        vectors.
C
         IF ( EVEN(N) ) THEN

            CALL CLEARD ( 3, SUM )

            DO I = 1, N
               CALL VHAT  ( BOUNDS(1,I),   UVEC )
               CALL VADD  ( SUM,   UVEC,   V    )
               CALL VEQU  ( V,             SUM  )
            END DO

            CALL VSCL    ( 1.D0/N, SUM, CENTER )
            CALL VHATIP  ( CENTER )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL CHCKAD ( 'AXIS', AXIS, '~', CENTER, 3, TIGHT, OK )

         END IF

      END DO


      CALL T_SUCCESS ( OK )
      RETURN
      END




