!$Procedure      F_OSCELT ( OSCELT routine tests )

      SUBROUTINE F_OSCELT ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        OSCELT
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
!     This routine tests the low-level SPICELIB routine OSCELT.
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
!-    SPICELIB Version 2.0.0, 04-APR-2009 (NJB) (EDW)
!
!        Reduced high elliptical eccentricity and loosened
!        tolerances to make test behavior portable.
!
!-    SPICELIB Version 1.0.0, 23-JAN-2003 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      EXACT
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

!      DOUBLE PRECISION      VNORM

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
!     The value of CLOSE must match that used in OSCELT.
!
      DOUBLE PRECISION      CLOSE
      PARAMETER           ( CLOSE = 1.D-10 )


!
!     Local Variables
!
      CHARACTER*(STRSIZ)    TSTDSC

      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      INELTS ( 8 )
      DOUBLE PRECISION      MU
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XARGP  ( NARGP  )
      DOUBLE PRECISION      XECC   ( NECC   )
      DOUBLE PRECISION      XELTS  ( 8 )
      DOUBLE PRECISION      XINC   ( NINC   )
      DOUBLE PRECISION      XLNODE ( NLNODE )
      DOUBLE PRECISION      XM0    ( NM0    )
      DOUBLE PRECISION      XRP    ( NRP    )

      INTEGER               I
      INTEGER               IECC
      INTEGER               IARGP
      INTEGER               ILNODE
      INTEGER               IINC
      INTEGER               IM0

      LOGICAL               SUCCES

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_OSCELT' )

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


      CALL OSCELT ( STATE, 100.D0, 0.D0, ELTS )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEMASS)', OK )



      CALL TCASE ( 'Specific angular momentum == 0' )

      DO I = 4, 6
         STATE(I) = 0.D0
      END DO

      CALL OSCELT ( STATE, 100.D0, 1.D10, ELTS )

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
!                    OSCELT.
!
                     CALL CONICS ( INELTS, ET, STATE )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL OSCELT ( STATE, ET, MU, ELTS )

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
!                    by OSCELT to be zero, we know exactly what
!                    ARGP is supposed to be.  When the eccentricity
!                    is very close to but not equal to zero, ARGP
!                    can be almost anything.  For these cases all
!                    we can do is check ARGP+M0.
!
!                    Note that our path is governed by the eccentricity
!                    found by OSCELT, since an input eccentricity of
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
!                    only happens if OSCELT is able to determine that
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
!                       found to be zero by OSCELT.  The input
!                       value of the argument of periapse is going
!                       to be added onto the mean anomaly.
!
                        XELTS(6) = XELTS(6) + INELTS(4)

                     ELSE IF (       ( ELTS(2) .EQ. 0.D0 ) &
                               .AND. ( ELTS(3) .EQ. PI() )  ) THEN
!
!                       Eccentricity was found to be zero and
!                       inclination was found to be pi by OSCELT.
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


!
!                    IF ( IECC .EQ. 3  .AND.VNORM(STATE(4)).GT.1D3) THEN
!                       WRITE (*,*) '-----------------'
!                       WRITE (*,*) 'XECC(IECC)   = ', XECC(IECC)
!                       WRITE (*,*) 'INELTS(2)    = ', INELTS(2)
!                       WRITE (*,*) 'STATE        = ', STATE
!                       WRITE (*,*) 'STATE2       = ', STATE2
!                       WRITE (*,*) 'XM0(IM0)     = ', XM0(IM0)
!                    END IF
!

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
