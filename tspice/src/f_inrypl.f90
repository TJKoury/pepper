!$Procedure F_INRYPL ( INRYPL tests )

      SUBROUTINE F_INRYPL ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the SPICELIB ray-plane intersection routine
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
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
!     This routine tests the SPICELIB routine INRYPL.
!
!     Overview of test procedure
!     --------------------------
!
!     There are three general kinds of tests applied to whack the
!     candidate SPICELIB subroutine around.  The categories are:
!
!        -- Normal (non-exceptional) test cases whose results are
!           easy to check by hand.  These allow a tester to verify
!           that the tested routine is at least doing reasonable-looking
!           things.  (Expected results are shown for comparison.)
!
!        -- Normal test cases based on random inputs.  These
!           allow testing of a large number of cases.  Results will
!           be checked as follows:
!
!                The distance between the ray's vertex VERTEX and
!                the intercept point XPT will be computed.  This
!                distance will be used to scale the negative of the
!                ray's unit direction vector.  The scaled vector
!                will be added to XPT.  The difference between the
!                resulting vector and VERTEX will be divided by
!                the distance between VERTEX and XPT, and this scaled
!                difference will be used a measure of the error made
!                in computing the intercept point.
!
!
!        -- Exceptional cases.  All of the error detection code will
!           be exercised by these cases.
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
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 3.0.0, 01-MAY-2012 (NJB)
!
!        Loosened relative error tolerance for random
!        cases to 1e-1l.
!
!-    TSPICE Version 2.0.0, 23-OCT-2006 (NJB)
!
!        Corrected sign of expected intercept Z-component for
!        "normal test" 10.  Also corrected typo in the associated
!        comment.
!
!-    TSPICE Version 1.0.0, 20-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!


!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local Parameters
!
      DOUBLE PRECISION      EBIG
      PARAMETER           ( EBIG   = 1.D306 )

      DOUBLE PRECISION      EBIGN
      PARAMETER           ( EBIGN  = -EBIG )

      DOUBLE PRECISION      EBIG2
      PARAMETER           ( EBIG2  = 2.D306 )

      DOUBLE PRECISION      ESMALL
      PARAMETER           ( ESMALL = 1.D-306 )

      DOUBLE PRECISION      ESMALN
      PARAMETER           ( ESMALN = -1.D-306 )

      DOUBLE PRECISION      ESMAL2
      PARAMETER           ( ESMAL2 = 2.D-306 )


      INTEGER               EMAX
      PARAMETER           ( EMAX   = 306 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NBASIC
      PARAMETER           ( NBASIC =  11 )

      INTEGER               NRAND
      PARAMETER           ( NRAND  = 5000 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = NBASIC + NRAND )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN =  79 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   =   4 )

      DOUBLE PRECISION      MAXRE
      PARAMETER           ( MAXRE  =  1.D-11 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE


      DOUBLE PRECISION      C
      DOUBLE PRECISION      CONS
      DOUBLE PRECISION      D      ( 3 )
      DOUBLE PRECISION      DIFF   ( 3 )
      DOUBLE PRECISION      DIR    ( 3, NBASIC )
      DOUBLE PRECISION      ERRC
      DOUBLE PRECISION      ERRD   ( 3 )
      DOUBLE PRECISION      ERRN   ( 3 )
      DOUBLE PRECISION      ERROR  ( 3 )
      DOUBLE PRECISION      ERRV   ( 3 )
      DOUBLE PRECISION      EXPXPT ( 3, NBASIC )
      DOUBLE PRECISION      N      ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      PLCONS (    NBASIC )
      DOUBLE PRECISION      PLNORM ( 3, NBASIC )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      TOOBIG
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      VERTEX ( 3, NBASIC )
      DOUBLE PRECISION      VPRJ   ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               EXPNO  (    NBASIC )
      INTEGER               NXPTS
      INTEGER               I
      INTEGER               SEED

      LOGICAL               SEPOK

!
!     Saved variables
!
      SAVE                  PLCONS
      SAVE                  PLNORM
      SAVE                  VERTEX
      SAVE                  DIR
      SAVE                  EXPNO
      SAVE                  EXPXPT

!
!     Initial values
!


!
!     Normal test cases:
!


!
!     1)
!
!     Planes' normal vector:                  ( 0,   0,   1 )
!     Plane constant:                           0
!     Ray's vertex:                           ( 0,   0,   2 )
!     Ray's direction vector:                 ( 0,   0,  -1 )
!     Expected number of intercept points:      1
!     Expected intercept point:               ( 0,   0,   0 )
!
!
      DATA    PLCONS(1),             ( PLNORM (I,1), I = 1, 3  )       / &

              0.D0,                     0.D0,  0.D0,   1.D0            /


      DATA  ( VERTEX(I,1), I=1,3 ),  ( DIR(I,1),     I = 1, 3  )       / &

              0.D0,  0.D0,  2.D0,       0.D0,  0.D0,  -1.D0            /


      DATA   EXPNO(1),               ( EXPXPT(I,1),  I = 1, 3  )       / &

              1,                        0.D0,   0.D0,  0.D0            /



!
!     2)
!
!     Planes' normal vector:                  ( 0,     0,     1     )
!     Plane constant:                           10
!     Ray's vertex:                           ( 0,     0,     20    )
!     Ray's direction vector:                 ( 0,     3,    -1     )
!     Expected number of intercept points:      1
!     Expected intercept point:               ( 0,     30     10    )
!
!
      DATA    PLCONS(2),             ( PLNORM (I,2), I = 1, 3 )        / &

              1.D1,                    0.D0,  0.D0,   1.D0             /


      DATA  ( VERTEX(I,2), I=1,3 ),  ( DIR    (I,2), I = 1, 3 )        / &

              0.D0,  0.D0,  2.D1,      0.D0,  3.D0,  -1.D0             /


      DATA   EXPNO(2),               ( EXPXPT (I,2),  I = 1,3 )        / &

              1,                        0.D0,  3.D1,  1.D1             /



!
!     3)
!
!     Planes' normal vector:                  ( 0,   0,   1 )
!     Plane constant:                           0
!     Ray's vertex:                           ( 1,   1,   0 )
!     Ray's direction vector:                 ( 0,   0,  -1 )
!     Expected number of intercept points:      1
!     Expected intercept point:               ( 1,   1,   0 )
!
!
      DATA    PLCONS(3),             ( PLNORM (I,3), I = 1, 3  )       / &

              0.D0,                     0.D0,  0.D0,   1.D0            /


      DATA  ( VERTEX(I,3), I=1,3 ),  ( DIR    (I,3), I = 1, 3  )       / &

              0.D0,  0.D0,  0.D0,       0.D0,  0.D0,  -1.D0            /


      DATA   EXPNO(3),               ( EXPXPT (I,3), I = 1, 3  )       / &

              1,                        0.D0,   0.D0,  0.D0            /



!
!     4)
!
!     Planes' normal vector:                  ( 0,   1,   0 )
!     Plane constant:                           0
!     Ray's vertex:                           ( 0,   0,   0 )
!     Ray's direction vector:                 ( 1,   0,   0 )
!     Expected number of intercept points:     -1 (infinite)
!     Expected intercept point:               ( 0,   0,   0 )
!
!
      DATA    PLCONS(4),             ( PLNORM (I,4), I = 1, 3  )       / &

              0.D0,                     0.D0,  1.D0,   0.D0            /


      DATA  ( VERTEX(I,4), I=1,3 ),  ( DIR    (I,4), I = 1, 3  )       / &

              0.D0,  0.D0,  0.D0,       1.D0,  0.D0,   1.D0            /


      DATA   EXPNO(4),               ( EXPXPT (I,4), I = 1, 3  )       / &

             -1,                        0.D0,   0.D0,  0.D0            /




!
!     5)
!
!     Planes' normal vector:                  ( 0,   0,   1 )
!     Plane constant:                           0
!     Ray's vertex:                           ( 1,   1,   1 )
!     Ray's direction vector:                 ( 1,   0,   0 )
!     Expected number of intercept points:      0
!     Expected intercept point:               ( 0,   0,   0 )
!
!
      DATA    PLCONS(5),             ( PLNORM (I,5), I = 1, 3  )       / &

              0.D0,                     0.D0,  0.D0,   1.D0            /


      DATA  ( VERTEX(I,5), I=1,3 ),  ( DIR    (I,5), I = 1, 3  )       / &

              1.D0,  1.D0,  1.D0,       1.D0,  0.D0,   0.D0            /


      DATA    EXPNO(5),              ( EXPXPT (I,5), I = 1, 3  )       / &

              0,                        0.D0,   0.D0,  0.D0            /



!
!     6)
!
!     Planes' normal vector:                  ( 0,     0,       1 )
!     Plane constant:                           0
!     Ray's vertex:                           ( 1,     1,       1 )
!     Ray's direction vector:                 ( 0,     1.D16,  -1 )
!     Expected number of intercept points:      1
!     Expected intercept point:               ( 1,     1.D16,   0 )
!
!
      DATA    PLCONS(6),             ( PLNORM (I,6), I = 1, 3  )       / &

              0.D0,                     0.D0,  0.D0,   1.D0            /


      DATA  ( VERTEX(I,6), I=1,3 ),  ( DIR    (I,6), I = 1, 3  )       / &

              1.D0,  1.D0,  1.D0,       0.D0,  1.D16,  -1.D0           /


      DATA    EXPNO(6),              ( EXPXPT (I,6), I = 1, 3  )       / &

              1,                        1.D0,  1.D16,  0.D0            /



!
!     7)
!
!     Planes' normal vector:                  ( 0,     0,       1 )
!     Plane constant:                           0
!     Ray's vertex:                           ( 1,     1,       1 )
!     Ray's direction vector:                 ( 0,     1.D308, -1 )
!     Expected number of intercept points:      0
!     Expected intercept point:               ( 0,     0,       0 )
!
!
      DATA    PLCONS(7),             ( PLNORM (I,7), I = 1, 3  )       / &

              0.D0,                     0.D0,  0.D0,   1.D0            /


      DATA  ( VERTEX(I,7), I=1,3 ),  ( DIR    (I,7), I = 1, 3  )       / &

              1.D0,  1.D0,  1.D0,       0.D0,  1.D308,  -1.D0          /


      DATA    EXPNO(7),              ( EXPXPT (I,7), I = 1, 3  )       / &

              0,                        0.D0,  0.D0,    0.D0           /


!
!     8)
!
!     Planes' normal vector:                  ( 0,     0,       1     )
!     Plane constant:                           EBIG
!     Ray's vertex:                           ( 1,     1,      2*EBIG )
!     Ray's direction vector:                 ( 0,     100,    -1     )
!     Expected number of intercept points:      0
!     Expected intercept point:               ( 0,     0,       0 )
!
!
      DATA    PLCONS(8),             ( PLNORM (I,8), I = 1, 3  )      / &

              EBIG,                    0.D0,  0.D0,   1.D0            /


      DATA  ( VERTEX(I,8), I=1,3 ),  ( DIR    (I,8), I = 1, 3  )      / &

              1.D0,  1.D0,  EBIG2,     0.D0,  1.D2,   -1.D0           /


      DATA    EXPNO(8),              ( EXPXPT (I,8), I = 1, 3  )      / &

              0,                        0.D0,  0.D0,    0.D0          /


!
!     9)
!
!     Planes' normal vector:                  ( 0,      0,       1    )
!     Plane constant:                          -1.D306
!     Ray's vertex:                           ( 1.D306,  0,      0    )
!     Ray's direction vector:                 ( 1,      0,      -1    )
!     Expected number of intercept points:      1
!     Expected intercept point:               ( 2.D306,  0,   -1.D306 )
!
!
      DATA    PLCONS(9),             ( PLNORM (I,9), I = 1, 3  )       / &

              EBIGN,                     0.D0,  0.D0,   1.D0           /


      DATA  ( VERTEX(I,9), I=1,3 ),  ( DIR    (I,9), I = 1, 3  )       / &

              EBIG,  0.D0,  0.D0,        1.D0,  0.D0,   -1.D0          /


      DATA    EXPNO(9),              ( EXPXPT (I,9), I = 1, 3  )       / &

              1,                         EBIG2, 0.D0,  EBIGN           /


!
!     10)
!
!     Planes' normal vector:                  ( 0,       0,       1    )
!     Plane constant:                          -1.D-306
!     Ray's vertex:                           ( 1.D-306,  0,      0    )
!     Ray's direction vector:                 ( 1,       0,      -1    )
!     Expected number of intercept points:      1
!     Expected intercept point:               ( 2.D-306,  0,  -1.D-306 )
!
!
      DATA    PLCONS(10),             ( PLNORM (I,10), I = 1, 3  )     / &

             ESMALN,                    0.D0,  0.D0,   1.D0            /


      DATA  ( VERTEX(I,10), I=1,3 ),  ( DIR    (I,10), I = 1, 3  )     / &

              ESMALL,  0.D0,  0.D0,     1.D0,  0.D0,   -1.D0           /


      DATA    EXPNO(10),              ( EXPXPT (I,10), I = 1, 3  )     / &

              1,                        ESMAL2, 0.D0, ESMALN          /


!
!     11)
!
!     Planes' normal vector:                  ( 0,       0,       1    )
!     Plane constant:                           1
!     Ray's vertex:                           ( 0     ,  0,       0    )
!     Ray's direction vector:                 ( 1,       0,      -1    )
!     Expected number of intercept points:      0
!     Expected intercept point:               ( 0,       0,       0    )
!
!
      DATA    PLCONS(11),             ( PLNORM (I,11), I = 1, 3  )     / &

              1,                        0.D0,  0.D0,   1.D0            /


      DATA  ( VERTEX(I,11), I=1,3 ),  ( DIR    (I,11), I = 1, 3  )     / &

              0.D0,  0.D0,  0.D0,       1.D0,  0.D0,   -1.D0           /


      DATA    EXPNO(11),              ( EXPXPT (I,11), I = 1, 3  )     / &

              0,                        0.D0, 0.D0, 0.D0               /




!
!     Open the test family.
!
      CALL TOPEN ( 'F_INRYPL' )

!
!     We'll start out with some easy-to-check cases (11 of them).
!
      DO I = 1, NBASIC
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Basic case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )

!
!        Make a SPICELIB plane from the plane constant and normal
!        vector.
!
         CALL NVC2PL ( PLNORM(1,I), PLCONS(I), PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PL2NVC ( PLANE, NORMAL, CONS )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Call the routine to be tested.
!
         CALL INRYPL ( VERTEX(1,I), DIR(1,I), PLANE, NXPTS, XPT )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSI (  'NXPTS---number of intercept points', &
                        NXPTS, &
                        '=', &
                        EXPNO(I), &
                        0, &
                        OK                                   )

         CALL CHCKAD ( 'XPT---intercept', XPT,  '~~/', &
                        EXPXPT(1,I),      3,    MAXRE,  OK )

      END DO



!
!     Random cases follow.
!
      SEED = -1

      DO I = 1, NRAND
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Random case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE  ( TITLE )
!
!        Generate a random scale factor.
!
         SCALE = 10.D0 ** T_RANDD ( -DBLE(EMAX), DBLE(EMAX), SEED )

!
!        Generate a normal vector and plane constant, and from these
!        a SPICELIB plane.
!
         N(1)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         N(2)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         N(3)  =  T_RANDD ( -2.D0, 2.D0, SEED )

         CALL VHATIP ( N )
         CALL CHCKXC ( .FALSE., ' ', OK )

         C     =  T_RANDD ( -2.D0, 2.D0, SEED )  *  SCALE

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL NVC2PL ( N, C, PLANE )

!
!        Now generate a random ray vertex and ray direction vector.
!
         V(1)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         V(2)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         V(3)  =  T_RANDD ( -2.D0, 2.D0, SEED )

         CALL VSCLIP ( SCALE, V )
         CALL CHCKXC ( .FALSE., ' ', OK )

         D(1)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         D(2)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         D(3)  =  T_RANDD ( -2.D0, 2.D0, SEED )

!
!        The call.
!
         CALL INRYPL ( V, D, PLANE, NXPTS, XPT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We can pretty safely assume that we won't see a value of
!        -1 for NXPTS.  If the value is 1, we'll try to get back from
!        XPT to the ray's vertex.
!
         IF ( NXPTS .EQ. 1 ) THEN

            CALL VSUB   (  V,  XPT,  DIFF  )

            CALL VHATIP ( D )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VLCOM (  1.D0,        XPT, &
                         -VNORM(DIFF), D,    V2 )

            CALL CHCKAD ( 'V2', V2, '~~/', V, 3, MAXRE, OK )

            CALL VSUB  (  V,  V2,  ERROR )

         ELSE
!
!           Check the angular separation between the ray
!           and the vector from the ray's vertex to its orthogonal
!           projection to the plane.
!
            CALL VPRJP  ( V,      PLANE, VPRJ )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL VSUB   ( VPRJ,   V,     DIFF )

            SEP    =  VSEP ( DIFF,   D )

            TOOBIG = DPMAX() / 3

            IF ( SEP*DPR() .GE. 90.D0 )THEN
!
!              The ray is parallel to or points away from the plane.
!
               SEPOK = .TRUE.

            ELSE IF ( SEP .GT. DATAN2( TOOBIG, VNORM(DIFF) )  ) THEN
!
!              It doesn't happen often, but we might have a case
!              where the ray is too close to being parallel with
!              the plane for an intersection to occur.
!
               SEPOK = .TRUE.

            ELSE
!
!              This shouldn't happen.
!
               SEPOK = .FALSE.

            END IF

            CALL CHCKSL ( 'SEPOK---is angular separation of ray '  // &
                          'and plane consistent with return code?', &
                          SEPOK, &
                          .TRUE., &
                          OK                                         )

         END IF

      END DO


!
!     Now for the exceptions.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exception:  ray''s direction is the zero vector.' )

      CALL CLEARD ( 3, ERRV )
      CALL CLEARD ( 3, ERRD )
      CALL CLEARD ( 3, ERRN )

      ERRN(3) = 1.D0
      ERRC    = 0.D0

      CALL NVC2PL ( ERRN, ERRC, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INRYPL ( ERRV, ERRD, PLANE, NXPTS, XPT )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exception:  ray''s vertex is just too big.' )

      CALL CLEARD ( 3, ERRV )
      CALL CLEARD ( 3, ERRD )
      CALL CLEARD ( 3, ERRN )

      ERRV(3) =  1.D308
      ERRD(3) = -1.D0
      ERRN(3) =  1.D0
      ERRC    =  0.D0

      CALL NVC2PL ( ERRN, ERRC, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INRYPL ( ERRV, ERRD, PLANE, NXPTS, XPT )
      CALL CHCKXC ( .TRUE., 'SPICE(VECTORTOOBIG)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exception:  Plane is too far from the origin.' )

      CALL CLEARD ( 3, ERRV )
      CALL CLEARD ( 3, ERRD )
      CALL CLEARD ( 3, ERRN )

      ERRV(3) =  1.D0
      ERRD(3) = -1.D0
      ERRN(3) =  1.D0
      ERRC    =  1.D308

      CALL NVC2PL ( ERRN, ERRC, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INRYPL ( ERRV, ERRD, PLANE, NXPTS, XPT )
      CALL CHCKXC ( .TRUE., 'SPICE(VECTORTOOBIG)', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
