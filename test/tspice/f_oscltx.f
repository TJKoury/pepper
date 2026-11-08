C$Procedure      F_OSCLTX ( OSCLTX routine tests )
 
      SUBROUTINE F_OSCLTX ( OK )
 
C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        OSCLTX
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'oscltx.inc'
 
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
C     This routine tests the low-level SPICELIB routine OSCLTX.
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
C     N.J. Bachman     (JPL)
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 02-FEB-2017 (NJB) 
C
C        Previous version 11-NOV-2014  
C
C           Based on TSPICE Version 2.0.0, 04-APR-2009 (NJB) (EDW)
C
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C     
      DOUBLE PRECISION      DACOSH
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      EXACT
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM

C
C     Local Parameters
C
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

C
C     The value of CLOSE must match that used in OSCLTX.
C
      DOUBLE PRECISION      CLOSE
      PARAMETER           ( CLOSE = 1.D-10 )


C
C     Local Variables
C      
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
      
 
C
C     Saved variables
C
      SAVE

C
C     Initial values
C     
      DATA                  Z / 0.D0, 0.D0, 1.D0 /

 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_OSCLTX' )      

C
C     Error tests:
C
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

C
C     Normal cases:
C

C
C     Set up the expected element values.
C
C
C     Expected argument of periapse:
C
      DO IARGP = 1, NARGP
         XARGP(IARGP)  =  (IARGP-1) * TWOPI() / NARGP
      END DO

C
C     Expected eccentricity:
C
      XECC(1)  =  0.D0
      XECC(2)  =  0.5D0
      XECC(3)  =  0.999D0
      XECC(4)  =  1.D0  
      XECC(5)  =  1.001D0
      XECC(6)  =  1.5D0

C
C     Expected inclination:
C
      XINC(1)  =  0.D0
      XINC(2)  =  CLOSE / 2
      XINC(3)  =  0.25D0 * PI()  
      XINC(4)  =  0.50D0 * PI()
      XINC(5)  =  0.75D0 * PI() 
      XINC(6)  =  PI() -   CLOSE / 2 
      XINC(7)  =  PI()  

C
C     Expected longitude of the ascending node:
C
      DO ILNODE = 1, NLNODE
         XLNODE(ILNODE)  =  (ILNODE-1) * TWOPI() / NLNODE
      END DO

C
C     Expected mean anomaly:
C
      DO IM0 = 1, NM0
         XM0(IM0)  =  (IM0-1) * TWOPI() / NM0
      END DO

C
C     Epoch:
C
      ET     = 1.D8

C
C     Expected perifocal distance:
C
      XRP(1) =  2.0D0 ** 18

C
C     GM of central body (we make this the cube of RP so we may
C     recover an eccentricity of zero when we have a circular orbit):
C
      MU     = 2.0D0 ** 54

C
C     For a variety of element sets, we'll use CONICS to produce
C     the equivalent state vector.  We'll then try to recover
C     the elements from the state vector.
C
      DO IARGP = 1, NARGP

         DO IECC = 1, NECC

            DO IINC = 1, NINC
               
               DO ILNODE = 1, NLNODE

                  DO IM0 = 1, NM0
C
C                    Assign the input elements for this test case.
C
                     INELTS(1) = XRP   (1     )
                     INELTS(2) = XECC  (IECC  )
                     INELTS(3) = XINC  (IINC  )
                     INELTS(4) = XLNODE(ILNODE)
                     INELTS(5) = XARGP (IARGP )
                     INELTS(6) = XM0   (IM0   )
                     INELTS(7) = ET
                     INELTS(8) = MU


C
C                    Set the expected elements.  
C
                     DO I = 1, 8 
                        XELTS(I) = INELTS(I)
                     END DO

C                    In some cases, these are not the input 
C                    elements, so adjust accordingly:
C
C                    If the input inclination is close to 0 or pi,
C                    the output inclination will be rounded.  The
C                    longitude of the ascending node becomes 0.
C
                     IF ( ABS ( INELTS(3) - 0.D0 ) .LT. CLOSE ) THEN

                        XELTS(3) = 0.D0
                        XELTS(4) = 0.D0

                     ELSE IF ( ABS(INELTS(3) - PI() ) .LT. CLOSE ) THEN

                        XELTS(3) = PI()
                        XELTS(4) = 0.D0

                     END IF

C
C                    If the eccentricity is "close" to 1, make it 1.
C          
                     XELTS(2) = EXACT ( XELTS(2), 1.D0, CLOSE )


C
C                    Set up the test description for the TCASE call.
C
                     TSTDSC = 'RP = #; ECC = #; INC = #; LNODE = #; ' //
     .                        'ARGP = #; M0 = #; ET = #;, MU = #'
                     DO I = 1, 8
                        CALL REPMD( TSTDSC, '#', INELTS(I), 14, TSTDSC )
                     END DO

                     CALL TCASE ( TSTDSC )

C
C                    Obtain the equivalent state vector, then call
C                    OSCLTX.
C
                     CALL CONICS ( INELTS, ET, STATE )
                     
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL OSCLTX ( STATE, ET, MU, ELTS )

                     CALL CHCKXC ( .FALSE., ' ', OK )


C                     WRITE (*,*) 'ELTS(2) = ', ELTS(2)
C                     WRITE (*,*) 'ELTS(3) = ', ELTS(3)


                     SUCCES = .TRUE.
C
C                    See how well we did.
C
C                    Set the tolerance to use for RP; acknowledge
C                    that we can't recover RP for nearly 
C                    parabolic orbits.
C

                     IF ( INELTS(2) .EQ. 1.D0 ) THEN

                        TOL = MEDIUM

                     ELSE IF ( ABS(INELTS(2)-1.D0) .LT. CLOSE ) THEN

                        TOL = SLOPPY

                     ELSE IF ( ABS(INELTS(2)-1.D0) .LT. 1.D-4 ) THEN

                        TOL = LOOSE
                     ELSE
                        TOL = EASY
                     END IF

                     CALL CHCKSD ( 'RP',    ELTS(1), '~/',  
     .                                      XELTS(1), TOL, OK )

                     SUCCES = SUCCES .AND. OK

C
C                    We use the same tolerances for eccentricity 
C                    as for inclination.
C                     
                     CALL CHCKSD ( 'ECC',   ELTS(2), '~',  
     .                                      XELTS(2), TOL, OK )

                     SUCCES = SUCCES .AND. OK

C
C                    We should always be able to recover inclination
C                    reasonably accurately.
C                     
                     TOL = EASY

                     CALL CHCKSD ( 'INC',   ELTS(3), '~',  
     .                                      XELTS(3), TOL, OK )

                     SUCCES = SUCCES .AND. OK

C
C                    When the inclination is not too close to 0
C                    or pi, we should be able to recover it
C                    reasonably accurately.
C
                     IF (  SIN( XELTS(4) ) .GT. CLOSE ) THEN
                        TOL = EASY
                     ELSE
                        TOL = LOOSE
                     END IF

                     IF ( ABS( ELTS(4)-XELTS(4)) .LE. PI() ) THEN

                        CALL CHCKSD ( 'LNODE', ELTS(4), '~',  
     .                                         XELTS(4), TOL, OK )
                     ELSE

                        IF ( ELTS(4) .GT. XELTS(4) ) THEN

                           XELTS(4) = XELTS(4) + TWOPI()
                        ELSE
                           XELTS(4) = XELTS(4) - TWOPI()
                        END IF

                        CALL CHCKSD ( 'LNODE', ELTS(4), '~',  
     .                                         XELTS(4), TOL, OK )

                     END IF

                     SUCCES = SUCCES .AND. OK

C
C                    Check ARGP:
C                    
C                    When the eccentricity is not too close to 
C                    zero and the inclination is not too close to
C                    zero or pi, we can use a reasonably tight tolerance
C                    for ARGP.  When the eccentricity is determined
C                    by OSCLTX to be zero, we know exactly what
C                    ARGP is supposed to be.  When the eccentricity
C                    is very close to but not equal to zero, ARGP
C                    can be almost anything.  For these cases all
C                    we can do is check ARGP+M0.
C
C                    Note that our path is governed by the eccentricity
C                    found by OSCLTX, since an input eccentricity of
C                    zero may not be recovered as such.
C
                     IF (       ( INELTS(3) .GT. CLOSE        ) 
     .                    .AND. ( INELTS(3) .LT. PI() - CLOSE )  ) THEN
C
C                       These are the normal inclination cases.
C
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

                     ELSE IF (      ( INELTS(3) .EQ. 0.D0 ) 
     .                         .OR. ( INELTS(3) .EQ. PI() )  ) THEN
C
C                       These are exceptional, but reasonably
C                       well-behaved inclination cases.
C
                        IF (  ABS( ELTS(2) - 1.D0 ) .LE. CLOSE ) THEN

                           TOL = DPMAX()

                        ELSE IF ( ABS( INELTS(2) - 1.D0 ) .LE. 1.D-2 ) 
     .                  THEN

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

                        ELSE IF (      ( ELTS(2) .GT. CLOSE ) 
     .                            .OR. ( ELTS(2) .EQ. 0.D0  ) ) THEN

                           TOL = EASY
                        ELSE
                           TOL = DPMAX()
                        END IF

                     END IF
 
C
C                    When the eccentricity is zero, the argument of
C                    periapse is absorbed into the mean anomaly.  This
C                    only happens if OSCLTX is able to determine that
C                    the eccentricity is zero, so test ELTS(2).
C
                     IF ( ELTS(2) .EQ. 0.D0 ) THEN

                        XELTS(6) = XELTS(5) + XELTS(6)
                        XELTS(5) = 0.D0

                     END IF
C
C                    Adjust our expected value of ARGP if 
C                    LNODE has been set to zero and if the 
C                    eccentricity is non-zero.  The sign of
C                    the adjustment depends on INC.
C
                     IF (       ( ELTS(4)   .EQ. 0.D0 ) 
     .                    .AND. ( INELTS(4) .NE. 0.D0 )
     .                    .AND. ( ELTS(2)   .NE. 0.D0 )
     .                    .AND. ( ELTS(3)   .EQ. 0.D0 ) ) THEN


                        XELTS(5) = XELTS(5) + ( INELTS(4) - ELTS(4) )

                        

                     ELSE IF (       ( ELTS(4)   .EQ. 0.D0 ) 
     .                         .AND. ( INELTS(4) .NE. 0.D0 )
     .                         .AND. ( ELTS(2)   .NE. 0.D0 ) 
     .                         .AND. ( ELTS(3)   .EQ. PI() ) ) THEN
  
                        XELTS(5) = XELTS(5) + ( ELTS(4) - INELTS(4) )
                        
                     END IF

                     IF ( XELTS(5) .GE. TWOPI() ) THEN

                        XELTS(5) = XELTS(5) - TWOPI()

                     ELSE IF ( XELTS(5) .LT. 0.D0 ) THEN

                        XELTS(5) = XELTS(5) + TWOPI()

                     END IF


                     IF ( ABS( ELTS(5)-XELTS(5)) .LE. PI() ) THEN

                         CALL CHCKSD ( 'ARGP', ELTS(5), '~',  
     .                                        XELTS(5), TOL, OK )
                     ELSE

                        IF ( ELTS(5) .GT. XELTS(5) ) THEN

                           XELTS(5) = XELTS(5) + TWOPI()
                        ELSE
                           XELTS(5) = XELTS(5) - TWOPI()

                        END IF

                        CALL CHCKSD ( 'ARGP', ELTS(5), '~',  
     .                                        XELTS(5), TOL, OK )

                     END IF

                     SUCCES = SUCCES .AND. OK

C
C                    Check M0:
C
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

C
C                    If the eccentricity is zero and the inclination
C                    is zero or pi, the original
C                    longitude of the ascending node is absorbed into
C                    M0.
C
                     IF (       ( ELTS(2) .EQ. 0.D0 )
     .                    .AND. ( ELTS(3) .EQ. 0.D0 )   ) THEN
C
C                       Eccentricity and inclination were both
C                       found to be zero by OSCLTX.  The input
C                       value of the argument of periapse is going
C                       to be added onto the mean anomaly.
C
                        XELTS(6) = XELTS(6) + INELTS(4)

                     ELSE IF (       ( ELTS(2) .EQ. 0.D0 )
     .                         .AND. ( ELTS(3) .EQ. PI() )  ) THEN
C
C                       Eccentricity was found to be zero and 
C                       inclination was found to be pi by OSCLTX.
C                       The input value of the argument of periapse 
C                       is going to be subtracted from the mean anomaly.
C
                        XELTS(6) = XELTS(6) - INELTS(4)

                     END IF

                     IF ( ABS( ELTS(6)-XELTS(6)) .LE. PI() ) THEN

                         CALL CHCKSD ( 'M0', ELTS(6), '~',  
     .                                        XELTS(6), TOL, OK )
                     ELSE

                        IF ( ELTS(6) .GT. XELTS(6) ) THEN

                           XELTS(6) = XELTS(6) + TWOPI()
                        ELSE
                           XELTS(6) = XELTS(6) - TWOPI()

                        END IF

                        CALL CHCKSD ( 'M0', ELTS(6), '~',  
     .                                      XELTS(6), TOL, OK )

                     END IF

                     SUCCES = SUCCES .AND. OK



                      CALL CHCKSD ( 'MU',   ELTS(8), '=', 
     .                                      XELTS(8), TIGHT, OK )


C
C                    Now that we've obtained elements, see whether
C                    we can use them to obtain the state we got
C                    back from CONICS the first time.
C                    
                     CALL CONICS ( ELTS, ET, STATE2 )

 
                     IF ( ABS( INELTS(2) - 1.D0 ) .LE. 1.D-2 ) THEN

                        TOL = VLOOSE

                     ELSE IF (     ( INELTS(3) .EQ. 0.D0 ) 
     .                        .OR. ( INELTS(3) .GE. PI() )  ) THEN
                        
                        TOL = MEDIUM

                     ELSE IF (     ( INELTS(3) .LE. CLOSE      ) 
     .                        .OR. ( INELTS(3) .GE. PI()-CLOSE )  ) THEN
                        
                        TOL = EASY

                     ELSE

                        TOL = MEDIUM

                     END IF
 
                     CALL CHCKAD( 'Position', STATE2,    '~~/', 
     .                                        STATE,     3, TOL, OK )

                     CALL CHCKAD( 'Velocity', STATE2(4), '~~/', 
     .                                        STATE(4),  3, TOL, OK )


C
C                    Check NU:
C
                     NU = ELTS(9)


                     IF ( INELTS(2) .EQ. 0.D0 ) THEN
C
C                       The orbit is circular. We expect NU to
C                       be equal to M0.
C
                        XNU = ELTS(6)

                        IF ( ABS(NU) .GT. 1.D0 ) THEN
                           CALL CHCKSD( 'NU', NU,  '~/', 
     .                                        XNU, MEDIUM, OK)
                        ELSE
                           CALL CHCKSD( 'NU', NU,  '~',  
     .                                        XNU, MEDIUM, OK)
                        END IF 


                      ELSE
C
C                       This is a non-circular orbit.
C
C                       We'll use the state and elements we already
C                       computed to find the basis vectors of the
C                       perifocal frame. 
C
C                    
C                       Start with the specific angular momentum vector:
C
                        CALL VCRSS ( STATE, STATE(4), H )

C
C                       The ascending node:
C                     
                        IF ( ABS( SIN( INELTS(3) ) ) .LT.  CLOSE  ) THEN
C
C                          Handle the case of inclination equal to zero
C                          or pi.
C
                           CALL VPACK( 1.D0, 0.D0, 0.D0, NODE )

                        ELSE

                           CALL UCRSS ( Z, H, NODE )

                        END IF

C
C                       Rotate the node vector about H by the argument
C                       of periapse to obtain the perifocal X vector:
C
                        CALL VROTV ( NODE, H, ELTS(5), PERIX )

                        CALL UCRSS ( H,    PERIX,      PERIY )

C
C                       Create a matrix to transform vectors from the
C                       base frame to the perifocal frame:
C
                        CALL TWOVEC ( PERIX, 1, PERIY, 2, PERIM )
                        CALL CHCKXC ( .FALSE., ' ', OK )


                        IF ( OK ) THEN
C
C                          Map the position to the perifocal frame.
C                          The longitude of the transformed vector is
C                          the expected true anomaly.
C
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

C
C                    Check the compatibility of NU and M0: derive one of
C
C                       - the eccentric anomaly
C                       - the parabolic anomaly
C                       - the hyperbolic anomaly
C
C                    from NU, then compare to M0.
C

                     ECC   = ELTS(2)

                     IF ( ECC .LT. 1.D0 ) THEN
C
C                       This is the elliptical case.
C
C                       The mean anomaly can be computed as shown below.
C
C                                     e + cos(nu)
C                          cos(E)  = ---------------         (ellipse)
C                                     1 + e cos(nu)
C
C                          M0      = E - e sin(E)
C
C                       

                        COSEA =    (          ECC + COS(NU) ) 
     .                           / ( 1.D0  +  ECC * COS(NU) ) 

C
C                       From the definition of the eccentric anomaly,
C                       we have 
C
C                          a sin(EA) = (a/b) * r * sin(nu)
C
C                       Here b is the semi-minor axis, so
C
C                           2    2    2    2
C                          a  - b  = a  ecc
C
C
C                       and
C
C                           2    2        2
C                          b  = a (1 - ecc )
C
C                             = [a(1 - ecc)] * a * (1 + ecc)
C
C                                 2          2
C                             = [a  (1 - ecc) ] * (1 + ecc)/(1 - ecc)
C
C                                  2
C                             = r_p  * (1 + ecc) / (1 - ecc)
C
C                       so
C
C                          1/b = 1/r_p  * sqrt( (1 - ecc)/(1 + ecc) ) 
C
C                       Then
C
C                          sin(EA) = (1/b) * r *sin(nu)
C
C                                  = (r/r_p) * sin(nu) 
C                                            * sqrt((1-ecc)/(1+ecc))
C
C                       
                        SINEA =    (VNORM(STATE)/ELTS(1))
     .                           * SIN(NU)
     .                           * SQRT( (1.D0-ECC)/(1.D0+ECC) )

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
C
C                       This is the parabolic case.
C
C                       The mean anomaly can be computed as shown
C                       below.
C
C                          D   = tan(nu/2)
C
C                                       3
C                          M0  = D  +  D / 3
C
                        
                        D  = TAN( NU / 2 )

                        XM = D  +  (D**3)/3


                        IF ( ABS(XM) .GT. 1.D-2 ) THEN
                           CALL CHCKSD('M', ELTS(6), '~/', XM, EASY, OK)
                        ELSE
                           CALL CHCKSD('M', ELTS(6), '~',  XM, EASY, OK)
                        END IF

 
                     ELSE
C
C                       ECC > 1
C                       
C
C                       This is the hyperbolic case.
C
C                       The mean anomaly can be computed as shown
C                       below.
C
C
C                                      e + cos(nu)
C                          cosh(F) = ---------------         (hyperbola)
C                                      1 + e cos(nu)
C
C                          M       = e sinh(F) - F
C
                        
                        COSHF =            ( ECC + COS(NU) ) 
     .                           / ( 1.D0 +  ECC * COS(NU) )

C
C                       Make sure we have a valid value of COSHF.
C
                        COSHF = MAX( 1.D0, COSHF )

                        F     = DACOSH( COSHF )
C
C                       Note that DACOSH is a SPICELIB function, not a 
C                       Fortran intrinsic. DACOSH can signal an error.
C
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



C
C                    Check A:
C
                     A = ELTS(10)

                     IF ( ELTS(2) .LT. 1.D0 ) THEN
C
C                       For elliptical orbits, A = r_p / (1-ecc):
C
                        XA   = ELTS(1) / ( 1.D0 - ELTS(2) )

                        TOL  = MEDIUM

                        CALL CHCKSD ( 'A (ecc<1)', A, '~/', 
     .                                             XA, TOL, OK )

                     ELSE IF ( ELTS(2) .EQ. 1.D0 ) THEN
C
C                       For parabolic orbits, A should be set to zero.
C
                        XA = 0.D0

                        CALL CHCKSD ( 'A (ecc==1)', A, '=', 
     .                                              XA, TOL, OK )

                     END IF

C
C                    Check of A for all non-parabolic orbits: compute
C                    the energy E; then let
C
C                       E = - mu / (2A)
C
C                    and solve for A:
C
C                       A = - mu / (2E)
C
C
C                    To compute E, we note that E is constant
C                    over the orbit, so it suffices to compute
C                    E at periapse:
C
C                                2  
C                       E = ( v_p / 2 )  -  mu / r_p
C
C                    where v_p and r_p are the speed and distance
C                    of the orbiting object, relative to the center
C                    of motion, at periapse.
C
C                    We also know that the semi-latus rectum p
C                    is equal to both
C
C                        2             2      2
C                       h / mu  =  (r_p  * v_p ) / mu
C
C                    
C                    and to
C
C                       r_p ( 1 + ecc )
C
C                    Then 
C
C                          2                             
C                       v_p  = mu * ( 1 + ecc ) / r_p 
C
C                    So
C
C                       E    = mu * ( ecc - 1 ) / ( 2 * r_p )
C
C
                     E = MU * ( ELTS(2) - 1.D0 ) / ( 2.D0 * ELTS(1) )

                     IF ( E .NE. 0.D0 ) THEN

                        XA  = - MU / (2*E)

                        TOL = EASY

                        CALL CHCKSD ( 'A (ecc!=1)', A, '~/', 
     .                                              XA, TOL, OK )
                     END IF


                     SUCCES = SUCCES .AND. OK


C
C                    Check TAU:
C
C                    If we have an elliptical orbit, compute the
C                    expected value of TAU. Otherwise, TAU should
C                    be set to zero.
C
                     TAU = ELTS(11)

                     IF ( ELTS(2) .LT. 1.D0 ) THEN
C
C                       For elliptical orbits, A = r_p / (1-ecc):
C
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
C                        WRITE (*, *) 'ELTS: '
C                        WRITE (*, '(8(1X,E25.16))') ELTS
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
