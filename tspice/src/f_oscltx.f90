!$Procedure      F_OSCLTX ( OSCLTX routine tests )

      SUBROUTINE F_OSCLTX ( OK )
      use pepper_inc_oscltx

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        OSCLTX
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
!     This routine tests the low-level SPICELIB routine OSCLTX.
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
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 02-FEB-2017 (NJB)
!
!        Previous version 11-NOV-2014
!
!           Based on TSPICE Version 2.0.0, 04-APR-2009 (NJB) (EDW)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      DACOSH
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      EXACT
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM

!
!     Local Parameters
!
      INTEGER               NARGP
      PARAMETER           ( NARGP  = 8 )

      INTEGER               NECC
      PARAMETER           ( NECC   = 6 )

      INTEGER               NINC
      PARAMETER           ( NINC   = 7 )

      INTEGER               NLNODE
      PARAMETER           ( NLNODE = 4 )

      INTEGER               NM0
      PARAMETER           ( NM0    = 8 )

      INTEGER               NRP
      PARAMETER           ( NRP    = 1 )

      INTEGER               STRSIZ
      PARAMETER           ( STRSIZ = 400 )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-12 )

      DOUBLE PRECISION      EASY
      PARAMETER           ( EASY   = 1.D-10 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-8 )

      DOUBLE PRECISION      VLOOSE
      PARAMETER           ( VLOOSE = 1.D-6 )

      DOUBLE PRECISION      SLOPPY
      PARAMETER           ( SLOPPY = 1.D-3 )

!
!     The value of CLOSE must match that used in OSCLTX.
!
      DOUBLE PRECISION      CLOSE
      PARAMETER           ( CLOSE = 1.D-10 )


!
!     Local Variables
!
      CHARACTER*(STRSIZ)    TSTDSC

      DOUBLE PRECISION      A
      DOUBLE PRECISION      COSEA
      DOUBLE PRECISION      COSHF
      DOUBLE PRECISION      D
      DOUBLE PRECISION      E
      DOUBLE PRECISION      EA
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      ELTS   ( OSCXSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      F
      DOUBLE PRECISION      H      ( 3 )
      DOUBLE PRECISION      INELTS ( 8 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      MU
      DOUBLE PRECISION      NODE   ( 3 )
      DOUBLE PRECISION      NU
      DOUBLE PRECISION      PERIM  ( 3, 3 )
      DOUBLE PRECISION      PERIP  ( 3 )
      DOUBLE PRECISION      PERIX  ( 3 )
      DOUBLE PRECISION      PERIY  ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SINEA
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )
      DOUBLE PRECISION      TAU
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XA
      DOUBLE PRECISION      XARGP  ( NARGP  )
      DOUBLE PRECISION      XECC   ( NECC   )
      DOUBLE PRECISION      XELTS  ( OSCXSZ )
      DOUBLE PRECISION      XINC   ( NINC   )
      DOUBLE PRECISION      XLNODE ( NLNODE )
      DOUBLE PRECISION      XM
      DOUBLE PRECISION      XM0    ( NM0    )
      DOUBLE PRECISION      XNU
      DOUBLE PRECISION      XRP    ( NRP    )
      DOUBLE PRECISION      XTAU
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               IECC
      INTEGER               IARGP
      INTEGER               ILNODE
      INTEGER               IINC
      INTEGER               IM0

      LOGICAL               SUCCES


!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  Z / 0.D0, 0.D0, 1.D0 /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_OSCLTX' )

!
!     Error tests:
!
      DO I = 1, 3
         STATE(I) = 1000.D0 * I
      END DO

      DO I = 4, 6
         STATE(I) = -10.D0 * I
      END DO

      CALL TCASE ( 'Invalid GM.' )


      CALL OSCLTX ( STATE, 100.D0, 0.D0, ELTS )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEMASS)', OK )



      CALL TCASE ( 'Specific angular momentum == 0' )

      DO I = 4, 6
         STATE(I) = 0.D0
      END DO

      CALL OSCLTX ( STATE, 100.D0, 1.D10, ELTS )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
!     Normal cases:
!

!
!     Set up the expected element values.
!
!
!     Expected argument of periapse:
!
      DO IARGP = 1, NARGP
         XARGP(IARGP)  =  (IARGP-1) * TWOPI() / NARGP
      END DO

!
!     Expected eccentricity:
!
      XECC(1)  =  0.D0
      XECC(2)  =  0.5D0
      XECC(3)  =  0.999D0
      XECC(4)  =  1.D0
      XECC(5)  =  1.001D0
      XECC(6)  =  1.5D0

!
!     Expected inclination:
!
      XINC(1)  =  0.D0
      XINC(2)  =  CLOSE / 2
      XINC(3)  =  0.25D0 * PI()
      XINC(4)  =  0.50D0 * PI()
      XINC(5)  =  0.75D0 * PI()
      XINC(6)  =  PI() -   CLOSE / 2
      XINC(7)  =  PI()

!
!     Expected longitude of the ascending node:
!
      DO ILNODE = 1, NLNODE
         XLNODE(ILNODE)  =  (ILNODE-1) * TWOPI() / NLNODE
      END DO

!
!     Expected mean anomaly:
!
      DO IM0 = 1, NM0
         XM0(IM0)  =  (IM0-1) * TWOPI() / NM0
      END DO

!
!     Epoch:
!
      ET     = 1.D8

!
!     Expected perifocal distance:
!
      XRP(1) =  2.0D0 ** 18

!
!     GM of central body (we make this the cube of RP so we may
!     recover an eccentricity of zero when we have a circular orbit):
!
      MU     = 2.0D0 ** 54

!
!     For a variety of element sets, we'll use CONICS to produce
!     the equivalent state vector.  We'll then try to recover
!     the elements from the state vector.
!
      DO IARGP = 1, NARGP

         DO IECC = 1, NECC

            DO IINC = 1, NINC

               DO ILNODE = 1, NLNODE

                  DO IM0 = 1, NM0
!
!                    Assign the input elements for this test case.
!
                     INELTS(1) = XRP   (1     )
                     INELTS(2) = XECC  (IECC  )
                     INELTS(3) = XINC  (IINC  )
                     INELTS(4) = XLNODE(ILNODE)
                     INELTS(5) = XARGP (IARGP )
                     INELTS(6) = XM0   (IM0   )
                     INELTS(7) = ET
                     INELTS(8) = MU


!
!                    Set the expected elements.
!
                     DO I = 1, 8
                        XELTS(I) = INELTS(I)
                     END DO

!                    In some cases, these are not the input
!                    elements, so adjust accordingly:
!
!                    If the input inclination is close to 0 or pi,
!                    the output inclination will be rounded.  The
!                    longitude of the ascending node becomes 0.
!
                     IF ( ABS ( INELTS(3) - 0.D0 ) .LT. CLOSE ) THEN

                        XELTS(3) = 0.D0
                        XELTS(4) = 0.D0

                     ELSE IF ( ABS(INELTS(3) - PI() ) .LT. CLOSE ) THEN

                        XELTS(3) = PI()
                        XELTS(4) = 0.D0

                     END IF

!
!                    If the eccentricity is "close" to 1, make it 1.
!
                     XELTS(2) = EXACT ( XELTS(2), 1.D0, CLOSE )


!
!                    Set up the test description for the TCASE call.
!
                     TSTDSC = 'RP = #; ECC = #; INC = #; LNODE = #; ' // &
                              'ARGP = #; M0 = #; ET = #;, MU = #'
                     DO I = 1, 8
                        CALL REPMD( TSTDSC, '#', INELTS(I), 14, TSTDSC )
                     END DO

                     CALL TCASE ( TSTDSC )

!
!                    Obtain the equivalent state vector, then call
!                    OSCLTX.
!
                     CALL CONICS ( INELTS, ET, STATE )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL OSCLTX ( STATE, ET, MU, ELTS )

                     CALL CHCKXC ( .FALSE., ' ', OK )


!                     WRITE (*,*) 'ELTS(2) = ', ELTS(2)
!                     WRITE (*,*) 'ELTS(3) = ', ELTS(3)


                     SUCCES = .TRUE.
!
!                    See how well we did.
!
!                    Set the tolerance to use for RP; acknowledge
!                    that we can't recover RP for nearly
!                    parabolic orbits.
!

                     IF ( INELTS(2) .EQ. 1.D0 ) THEN

                        TOL = MEDIUM

                     ELSE IF ( ABS(INELTS(2)-1.D0) .LT. CLOSE ) THEN

                        TOL = SLOPPY

                     ELSE IF ( ABS(INELTS(2)-1.D0) .LT. 1.D-4 ) THEN

                        TOL = LOOSE
                     ELSE
                        TOL = EASY
                     END IF

                     CALL CHCKSD ( 'RP',    ELTS(1), '~/', &
                                            XELTS(1), TOL, OK )

                     SUCCES = SUCCES .AND. OK

!
!                    We use the same tolerances for eccentricity
!                    as for inclination.
!
                     CALL CHCKSD ( 'ECC',   ELTS(2), '~', &
                                            XELTS(2), TOL, OK )

                     SUCCES = SUCCES .AND. OK

!
!                    We should always be able to recover inclination
!                    reasonably accurately.
!
                     TOL = EASY

                     CALL CHCKSD ( 'INC',   ELTS(3), '~', &
                                            XELTS(3), TOL, OK )

                     SUCCES = SUCCES .AND. OK

!
!                    When the inclination is not too close to 0
!                    or pi, we should be able to recover it
!                    reasonably accurately.
!
                     IF (  SIN( XELTS(4) ) .GT. CLOSE ) THEN
                        TOL = EASY
                     ELSE
                        TOL = LOOSE
                     END IF

                     IF ( ABS( ELTS(4)-XELTS(4)) .LE. PI() ) THEN

                        CALL CHCKSD ( 'LNODE', ELTS(4), '~', &
                                               XELTS(4), TOL, OK )
                     ELSE

                        IF ( ELTS(4) .GT. XELTS(4) ) THEN

                           XELTS(4) = XELTS(4) + TWOPI()
                        ELSE
                           XELTS(4) = XELTS(4) - TWOPI()
                        END IF

                        CALL CHCKSD ( 'LNODE', ELTS(4), '~', &
                                               XELTS(4), TOL, OK )

                     END IF

                     SUCCES = SUCCES .AND. OK

!
!                    Check ARGP:
!
!                    When the eccentricity is not too close to
!                    zero and the inclination is not too close to
!                    zero or pi, we can use a reasonably tight tolerance
!                    for ARGP.  When the eccentricity is determined
!                    by OSCLTX to be zero, we know exactly what
!                    ARGP is supposed to be.  When the eccentricity
!                    is very close to but not equal to zero, ARGP
!                    can be almost anything.  For these cases all
!                    we can do is check ARGP+M0.
!
!                    Note that our path is governed by the eccentricity
!                    found by OSCLTX, since an input eccentricity of
!                    zero may not be recovered as such.
!
                     IF (       ( INELTS(3) .GT. CLOSE        ) &
                          .AND. ( INELTS(3) .LT. PI() - CLOSE )  ) THEN
!
!                       These are the normal inclination cases.
!
                        IF (  ABS( ELTS(2) - 1.D0 ) .LE. CLOSE ) THEN

                           TOL = DPMAX()


                        ELSE IF ( ELTS(2) .GT. 1.D-5 ) THEN

                           TOL = MEDIUM


                        ELSE IF ( ELTS(2) .GT. 1.D-6 ) THEN

                           TOL = EASY


                        ELSE IF ( ELTS(2) .EQ. 0.D0 ) THEN

                           TOL = MEDIUM

                        ELSE
                           TOL = DPMAX()
                        END IF

                     ELSE IF (      ( INELTS(3) .EQ. 0.D0 ) &
                               .OR. ( INELTS(3) .EQ. PI() )  ) THEN
!
!                       These are exceptional, but reasonably
!                       well-behaved inclination cases.
!
                        IF (  ABS( ELTS(2) - 1.D0 ) .LE. CLOSE ) THEN

                           TOL = DPMAX()

                        ELSE IF ( ABS( INELTS(2) - 1.D0 ) .LE. 1.D-2 ) &
                        THEN

                           TOL = LOOSE

                        ELSE IF ( ELTS(2) .GT. 1.D-6 ) THEN

                           TOL = MEDIUM

                        ELSE IF ( ELTS(2) .EQ. 0.D0 ) THEN

                           TOL = MEDIUM

                        ELSE
                           TOL = DPMAX()
                        END IF


                     ELSE

                        IF (  ABS( ELTS(2) - 1.D0 ) .LE. CLOSE ) THEN

                           TOL = DPMAX()

                        ELSE IF (      ( ELTS(2) .GT. CLOSE ) &
                                  .OR. ( ELTS(2) .EQ. 0.D0  ) ) THEN

                           TOL = EASY
                        ELSE
                           TOL = DPMAX()
                        END IF

                     END IF

!
!                    When the eccentricity is zero, the argument of
!                    periapse is absorbed into the mean anomaly.  This
!                    only happens if OSCLTX is able to determine that
!                    the eccentricity is zero, so test ELTS(2).
!
                     IF ( ELTS(2) .EQ. 0.D0 ) THEN

                        XELTS(6) = XELTS(5) + XELTS(6)
                        XELTS(5) = 0.D0

                     END IF
!
!                    Adjust our expected value of ARGP if
!                    LNODE has been set to zero and if the
!                    eccentricity is non-zero.  The sign of
!                    the adjustment depends on INC.
!
                     IF (       ( ELTS(4)   .EQ. 0.D0 ) &
                          .AND. ( INELTS(4) .NE. 0.D0 ) &
                          .AND. ( ELTS(2)   .NE. 0.D0 ) &
                          .AND. ( ELTS(3)   .EQ. 0.D0 ) ) THEN


                        XELTS(5) = XELTS(5) + ( INELTS(4) - ELTS(4) )



                     ELSE IF (       ( ELTS(4)   .EQ. 0.D0 ) &
                               .AND. ( INELTS(4) .NE. 0.D0 ) &
                               .AND. ( ELTS(2)   .NE. 0.D0 ) &
                               .AND. ( ELTS(3)   .EQ. PI() ) ) THEN

                        XELTS(5) = XELTS(5) + ( ELTS(4) - INELTS(4) )

                     END IF

                     IF ( XELTS(5) .GE. TWOPI() ) THEN

                        XELTS(5) = XELTS(5) - TWOPI()

                     ELSE IF ( XELTS(5) .LT. 0.D0 ) THEN

                        XELTS(5) = XELTS(5) + TWOPI()

                     END IF


                     IF ( ABS( ELTS(5)-XELTS(5)) .LE. PI() ) THEN

                         CALL CHCKSD ( 'ARGP', ELTS(5), '~', &
                                              XELTS(5), TOL, OK )
                     ELSE

                        IF ( ELTS(5) .GT. XELTS(5) ) THEN

                           XELTS(5) = XELTS(5) + TWOPI()
                        ELSE
                           XELTS(5) = XELTS(5) - TWOPI()

                        END IF

                        CALL CHCKSD ( 'ARGP', ELTS(5), '~', &
                                              XELTS(5), TOL, OK )

                     END IF

                     SUCCES = SUCCES .AND. OK

!
!                    Check M0:
!
                     IF ( ABS( ELTS(2) - 1.D0 ) .LE.  CLOSE ) THEN

                        TOL = DPMAX()

                     ELSE IF ( ABS( ELTS(2) - 1.D0 ) .LE.  1.D-2 ) THEN

                        TOL = SLOPPY

                     ELSE IF ( ELTS(2) .GT. CLOSE ) THEN

                        TOL = MEDIUM

                     ELSE IF ( ELTS(2) .EQ. 0.D0 ) THEN

                        TOL = MEDIUM

                     ELSE
                        TOL = DPMAX()
                     END IF

                     SUCCES = SUCCES .AND. OK

!
!                    If the eccentricity is zero and the inclination
!                    is zero or pi, the original
!                    longitude of the ascending node is absorbed into
!                    M0.
!
                     IF (       ( ELTS(2) .EQ. 0.D0 ) &
                          .AND. ( ELTS(3) .EQ. 0.D0 )   ) THEN
!
!                       Eccentricity and inclination were both
!                       found to be zero by OSCLTX.  The input
!                       value of the argument of periapse is going
!                       to be added onto the mean anomaly.
!
                        XELTS(6) = XELTS(6) + INELTS(4)

                     ELSE IF (       ( ELTS(2) .EQ. 0.D0 ) &
                               .AND. ( ELTS(3) .EQ. PI() )  ) THEN
!
!                       Eccentricity was found to be zero and
!                       inclination was found to be pi by OSCLTX.
!                       The input value of the argument of periapse
!                       is going to be subtracted from the mean anomaly.
!
                        XELTS(6) = XELTS(6) - INELTS(4)

                     END IF

                     IF ( ABS( ELTS(6)-XELTS(6)) .LE. PI() ) THEN

                         CALL CHCKSD ( 'M0', ELTS(6), '~', &
                                              XELTS(6), TOL, OK )
                     ELSE

                        IF ( ELTS(6) .GT. XELTS(6) ) THEN

                           XELTS(6) = XELTS(6) + TWOPI()
                        ELSE
                           XELTS(6) = XELTS(6) - TWOPI()

                        END IF

                        CALL CHCKSD ( 'M0', ELTS(6), '~', &
                                            XELTS(6), TOL, OK )

                     END IF

                     SUCCES = SUCCES .AND. OK



                      CALL CHCKSD ( 'MU',   ELTS(8), '=', &
                                            XELTS(8), TIGHT, OK )


!
!                    Now that we've obtained elements, see whether
!                    we can use them to obtain the state we got
!                    back from CONICS the first time.
!
                     CALL CONICS ( ELTS, ET, STATE2 )


                     IF ( ABS( INELTS(2) - 1.D0 ) .LE. 1.D-2 ) THEN

                        TOL = VLOOSE

                     ELSE IF (     ( INELTS(3) .EQ. 0.D0 ) &
                              .OR. ( INELTS(3) .GE. PI() )  ) THEN

                        TOL = MEDIUM

                     ELSE IF (     ( INELTS(3) .LE. CLOSE      ) &
                              .OR. ( INELTS(3) .GE. PI()-CLOSE )  ) THEN

                        TOL = EASY

                     ELSE

                        TOL = MEDIUM

                     END IF

                     CALL CHCKAD( 'Position', STATE2,    '~~/', &
                                              STATE,     3, TOL, OK )

                     CALL CHCKAD( 'Velocity', STATE2(4), '~~/', &
                                              STATE(4),  3, TOL, OK )


!
!                    Check NU:
!
                     NU = ELTS(9)


                     IF ( INELTS(2) .EQ. 0.D0 ) THEN
!
!                       The orbit is circular. We expect NU to
!                       be equal to M0.
!
                        XNU = ELTS(6)

                        IF ( ABS(NU) .GT. 1.D0 ) THEN
                           CALL CHCKSD( 'NU', NU,  '~/', &
                                              XNU, MEDIUM, OK)
                        ELSE
                           CALL CHCKSD( 'NU', NU,  '~', &
                                              XNU, MEDIUM, OK)
                        END IF


                      ELSE
!
!                       This is a non-circular orbit.
!
!                       We'll use the state and elements we already
!                       computed to find the basis vectors of the
!                       perifocal frame.
!
!
!                       Start with the specific angular momentum vector:
!
                        CALL VCRSS ( STATE, STATE(4), H )

!
!                       The ascending node:
!
                        IF ( ABS( SIN( INELTS(3) ) ) .LT.  CLOSE  ) THEN
!
!                          Handle the case of inclination equal to zero
!                          or pi.
!
                           CALL VPACK( 1.D0, 0.D0, 0.D0, NODE )

                        ELSE

                           CALL UCRSS ( Z, H, NODE )

                        END IF

!
!                       Rotate the node vector about H by the argument
!                       of periapse to obtain the perifocal X vector:
!
                        CALL VROTV ( NODE, H, ELTS(5), PERIX )

                        CALL UCRSS ( H,    PERIX,      PERIY )

!
!                       Create a matrix to transform vectors from the
!                       base frame to the perifocal frame:
!
                        CALL TWOVEC ( PERIX, 1, PERIY, 2, PERIM )
                        CALL CHCKXC ( .FALSE., ' ', OK )


                        IF ( OK ) THEN
!
!                          Map the position to the perifocal frame.
!                          The longitude of the transformed vector is
!                          the expected true anomaly.
!
                           CALL MXV    ( PERIM, STATE, PERIP     )
                           CALL RECRAD ( PERIP, R,     XNU,  LAT )

                           IF ( XNU .LT. NU - PI() ) THEN

                              XNU = XNU + TWOPI()


                           ELSE IF ( XNU .GT. NU + PI() ) THEN

                              XNU = XNU - TWOPI()

                           END IF

                        END IF


                        IF ( ABS(XNU) .GT. 1.D-2 ) THEN
                           CALL CHCKSD( 'NU', NU, '~/', XNU, EASY, OK)
                        ELSE
                           CALL CHCKSD( 'NU', NU, '~',  XNU, EASY, OK)
                        END IF
                     END IF

!
!                    Check the compatibility of NU and M0: derive one of
!
!                       - the eccentric anomaly
!                       - the parabolic anomaly
!                       - the hyperbolic anomaly
!
!                    from NU, then compare to M0.
!

                     ECC   = ELTS(2)

                     IF ( ECC .LT. 1.D0 ) THEN
!
!                       This is the elliptical case.
!
!                       The mean anomaly can be computed as shown below.
!
!                                     e + cos(nu)
!                          cos(E)  = ---------------         (ellipse)
!                                     1 + e cos(nu)
!
!                          M0      = E - e sin(E)
!
!

                        COSEA =    (          ECC + COS(NU) ) &
                                 / ( 1.D0  +  ECC * COS(NU) )

!
!                       From the definition of the eccentric anomaly,
!                       we have
!
!                          a sin(EA) = (a/b) * r * sin(nu)
!
!                       Here b is the semi-minor axis, so
!
!                           2    2    2    2
!                          a  - b  = a  ecc
!
!
!                       and
!
!                           2    2        2
!                          b  = a (1 - ecc )
!
!                             = [a(1 - ecc)] * a * (1 + ecc)
!
!                                 2          2
!                             = [a  (1 - ecc) ] * (1 + ecc)/(1 - ecc)
!
!                                  2
!                             = r_p  * (1 + ecc) / (1 - ecc)
!
!                       so
!
!                          1/b = 1/r_p  * sqrt( (1 - ecc)/(1 + ecc) )
!
!                       Then
!
!                          sin(EA) = (1/b) * r *sin(nu)
!
!                                  = (r/r_p) * sin(nu)
!                                            * sqrt((1-ecc)/(1+ecc))
!
!
                        SINEA =    (VNORM(STATE)/ELTS(1)) &
                                 * SIN(NU) &
                                 * SQRT( (1.D0-ECC)/(1.D0+ECC) )

                        EA    = ATAN2 ( SINEA, COSEA )

                        XM    = EA  -  ECC * SINEA

                        IF ( XM .LT. 0.D0 ) THEN
                           XM = XM + TWOPI()
                        END IF

                        IF ( XM .LT. ELTS(6) - PI() ) THEN
                           XM = XM + TWOPI()
                        ELSE IF ( XM .GT. ELTS(6) + PI() ) THEN
                           XM = XM - TWOPI()
                        END IF

                        IF ( ABS(XM) .GT. 1.D-2 ) THEN
                           CALL CHCKSD('M', ELTS(6), '~/', XM, EASY, OK)
                        ELSE
                           CALL CHCKSD('M', ELTS(6), '~',  XM, EASY, OK)
                        END IF


                     ELSE IF ( ECC .EQ. 1.D0 ) THEN
!
!                       This is the parabolic case.
!
!                       The mean anomaly can be computed as shown
!                       below.
!
!                          D   = tan(nu/2)
!
!                                       3
!                          M0  = D  +  D / 3
!

                        D  = TAN( NU / 2 )

                        XM = D  +  (D**3)/3


                        IF ( ABS(XM) .GT. 1.D-2 ) THEN
                           CALL CHCKSD('M', ELTS(6), '~/', XM, EASY, OK)
                        ELSE
                           CALL CHCKSD('M', ELTS(6), '~',  XM, EASY, OK)
                        END IF


                     ELSE
!
!                       ECC > 1
!
!
!                       This is the hyperbolic case.
!
!                       The mean anomaly can be computed as shown
!                       below.
!
!
!                                      e + cos(nu)
!                          cosh(F) = ---------------         (hyperbola)
!                                      1 + e cos(nu)
!
!                          M       = e sinh(F) - F
!

                        COSHF =            ( ECC + COS(NU) ) &
                                 / ( 1.D0 +  ECC * COS(NU) )

!
!                       Make sure we have a valid value of COSHF.
!
                        COSHF = MAX( 1.D0, COSHF )

                        F     = DACOSH( COSHF )
!
!                       Note that DACOSH is a SPICELIB function, not a
!                       Fortran intrinsic. DACOSH can signal an error.
!
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        XM    = ECC * SINH(F)  -  F

                        IF ( NU .LT. 0.D0 ) THEN
                           XM = -XM
                        END IF


                        IF ( ABS(XM) .GT. 1.D-2 ) THEN
                           CALL CHCKSD('M', ELTS(6), '~/', XM, EASY, OK)
                        ELSE
                           CALL CHCKSD('M', ELTS(6), '~',  XM, EASY, OK)
                        END IF

                     END IF



!
!                    Check A:
!
                     A = ELTS(10)

                     IF ( ELTS(2) .LT. 1.D0 ) THEN
!
!                       For elliptical orbits, A = r_p / (1-ecc):
!
                        XA   = ELTS(1) / ( 1.D0 - ELTS(2) )

                        TOL  = MEDIUM

                        CALL CHCKSD ( 'A (ecc<1)', A, '~/', &
                                                   XA, TOL, OK )

                     ELSE IF ( ELTS(2) .EQ. 1.D0 ) THEN
!
!                       For parabolic orbits, A should be set to zero.
!
                        XA = 0.D0

                        CALL CHCKSD ( 'A (ecc==1)', A, '=', &
                                                    XA, TOL, OK )

                     END IF

!
!                    Check of A for all non-parabolic orbits: compute
!                    the energy E; then let
!
!                       E = - mu / (2A)
!
!                    and solve for A:
!
!                       A = - mu / (2E)
!
!
!                    To compute E, we note that E is constant
!                    over the orbit, so it suffices to compute
!                    E at periapse:
!
!                                2
!                       E = ( v_p / 2 )  -  mu / r_p
!
!                    where v_p and r_p are the speed and distance
!                    of the orbiting object, relative to the center
!                    of motion, at periapse.
!
!                    We also know that the semi-latus rectum p
!                    is equal to both
!
!                        2             2      2
!                       h / mu  =  (r_p  * v_p ) / mu
!
!
!                    and to
!
!                       r_p ( 1 + ecc )
!
!                    Then
!
!                          2
!                       v_p  = mu * ( 1 + ecc ) / r_p
!
!                    So
!
!                       E    = mu * ( ecc - 1 ) / ( 2 * r_p )
!
!
                     E = MU * ( ELTS(2) - 1.D0 ) / ( 2.D0 * ELTS(1) )

                     IF ( E .NE. 0.D0 ) THEN

                        XA  = - MU / (2*E)

                        TOL = EASY

                        CALL CHCKSD ( 'A (ecc!=1)', A, '~/', &
                                                    XA, TOL, OK )
                     END IF


                     SUCCES = SUCCES .AND. OK


!
!                    Check TAU:
!
!                    If we have an elliptical orbit, compute the
!                    expected value of TAU. Otherwise, TAU should
!                    be set to zero.
!
                     TAU = ELTS(11)

                     IF ( ELTS(2) .LT. 1.D0 ) THEN
!
!                       For elliptical orbits, A = r_p / (1-ecc):
!
                        XA   = ELTS(1) / ( 1.D0 - ELTS(2) )

                        XTAU = TWOPI() * SQRT( XA**3 / MU )

                        TOL  = MEDIUM

                        CALL CHCKSD ( 'TAU', TAU, '~/', XTAU, TOL, OK )

                     ELSE

                        TOL  = 0.D0

                        XTAU = 0.D0

                        CALL CHCKSD ( 'TAU', TAU, '=', XTAU, TOL, OK )

                     END IF

                     SUCCES = SUCCES .AND. OK


                     IF ( .NOT. SUCCES ) THEN
!                        WRITE (*, *) 'ELTS: '
!                        WRITE (*, '(8(1X,E25.16))') ELTS
                        SUCCES = .TRUE.
                     END IF

                  END DO

               END DO

            END DO

         END DO

      END DO


      CALL T_SUCCESS ( OK )
      RETURN
      END
